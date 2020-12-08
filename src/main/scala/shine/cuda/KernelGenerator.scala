package shine.cuda

import java.io.{File, PrintWriter}

import shine.C.AST.{DeclRef, Node, ParamDecl}
import shine.DPIA.Compilation.{SimplifyNats, TranslationToImperative, UnrollLoops}
import shine.DPIA.DSL.identifier
import shine.DPIA.{LetNatIdentifier, Lifting}
import shine.DPIA.Phrases.{Apply, DepApply, DepLambda, Identifier, Lambda, LetNat, Phrase, xmlPrinter}
import shine.DPIA.Types.{AccType, BasePhraseType, CommType, ExpType, NatKind, PhraseType, TypeCheck, int, read}
import shine.OpenCL.AST.RequiredWorkGroupSize
import shine.cuda.codegen.{AdaptKernelBody, HoistMemoryAllocations}
import shine.OpenCL.CodeGeneration.HoistMemoryAllocations.AllocationInfo
import shine.OpenCL.CodeGeneration.AdaptKernelParameters
import shine.OpenCL.{FlagPrivateArrayLoops, GlobalSize, LocalSize}
import shine.{C, OpenCL, cuda}

import scala.collection.immutable.{Map, Seq}

object KernelGenerator {
  def apply(): shine.cuda.KernelGenerator =
    new shine.cuda.KernelGenerator(
      cuda.codegen.CodeGenerator(),
      cuda.ast.Printer(_))
}

//noinspection VariablePatternShadow
class KernelGenerator(
                       val codeGen: C.CodeGeneration.CodeGenerator, //TODO Is this right?
                       val printer: Node => String) {
  def makeCode[T <: PhraseType](
                                 localSize: LocalSize,
                                 globalSize: GlobalSize)(
                                 originalPhrase: Phrase[T],
                                 name: String
                               ): util.KernelWithSizes = {
    val (phrase, params, defs) =
      getPhraseAndParams(originalPhrase, Seq(), Seq())
    makeKernel(name, phrase, params.reverse, defs.reverse,
      Some(localSize), Some(globalSize)).getOrElse(???)
  }

  def makeCode[T <: PhraseType](
                                 originalPhrase: Phrase[T],
                                 name: String = "KERNEL"
                               ): util.KernelNoSizes = {
    val (phrase, params, defs) =
      getPhraseAndParams(originalPhrase, Seq(), Seq())
    makeKernel(name, phrase, params.reverse, defs.reverse, None, None).swap.getOrElse(???)
  }

  @scala.annotation.tailrec
  private def getPhraseAndParams[_ <: PhraseType](
                                                   p: Phrase[_],
                                                   ps: Seq[Identifier[ExpType]],
                                                   defs:Seq[(LetNatIdentifier, Phrase[ExpType])]
                                                 ): (Phrase[ExpType],
    Seq[Identifier[ExpType]],
    Seq[(LetNatIdentifier, Phrase[ExpType])]) = {
    p match {
      case Apply(f, a) =>
        getPhraseAndParams(Lifting.liftFunction(f).reducing(a), ps, defs)
      case DepApply(f, a) =>
        getPhraseAndParams(Lifting.liftDependentFunction(f)(a), ps, defs)
      case l: Lambda[ExpType, _]@unchecked =>
        getPhraseAndParams(l.body, l.param +: ps, defs)
      case ndl: DepLambda[NatKind, _]@unchecked =>
        getPhraseAndParams(
          ndl.body, Identifier(ndl.x.name, ExpType(int, read)) +: ps, defs)
      case ln: LetNat[ExpType, _]@unchecked =>
        getPhraseAndParams(ln.body, ps, (ln.binder, ln.defn) +: defs)
      case ep: Phrase[ExpType]@unchecked => (ep, ps, defs)
    }
  }

  private def makeKernel(
                          name:String,
                          p: Phrase[ExpType],
                          inputParams: Seq[Identifier[ExpType]],
                          letNatDefs:Seq[(LetNatIdentifier, Phrase[ExpType])],
                          localSize: Option[LocalSize],
                          globalSize: Option[GlobalSize],
                        ): Either[util.KernelNoSizes, util.KernelWithSizes] = {

    val outParam = createOutputParam(outT = p.t)

    val gen = codeGen

    checkTypes(p) |> (p =>

      rewriteToImperative(p, outParam, localSize, globalSize) |> (p =>

        hoistMemoryAllocations(p) |> { case (p, intermediateAllocations) =>

          adaptKernelParameters(
            p, outParam, inputParams, intermediateAllocations, gen) |> {
            case (p, outParam, inputParams, intermediateAllocations, kernelParams) =>

              type IdentMap = Predef.Map[Identifier[_ <: BasePhraseType], DeclRef]
              val identMap: IdentMap =
                (outParam +: inputParams).map( p => p -> C.AST.DeclRef(p.name) ).toMap

              val intermediateIdentMap: IdentMap =
                intermediateAllocations.flatMap(p =>
                  Seq(
                    Identifier(s"${p.identifier.name}_1", p.identifier.`type`.t1) ->
                      C.AST.DeclRef(p.identifier.name),
                    Identifier(s"${p.identifier.name}_2", p.identifier.`type`.t2) ->
                      C.AST.DeclRef(p.identifier.name))).toMap

              val env = C.CodeGeneration.CodeGenerator.Environment(
                identMap ++ intermediateIdentMap, Map.empty, Map.empty, Map.empty)

              val (declarations, code) = gen.generate(p, letNatDefs, env)

              val typeDeclarations =
                C.ProgramGenerator.collectTypeDeclarations(code, kernelParams)

              val (body, dynamicSharedMemory) = adaptKernelBody(C.AST.Block(Seq(code)))

              val cuKernel = cuda.Kernel(typeDeclarations ++ declarations,
                kernel = makeKernelFunction(
                  name,
                  kernelParams,
                  body,
                  localSize),
                outputParam = outParam,
                inputParams = inputParams,
                dynamicSharedMemory,
                intermediateParams = intermediateAllocations.map (_.identifier),
                printer)

              (localSize, globalSize) match {
                case (None, None) => Left(util.KernelNoSizes(cuKernel))
                case (Some(localSize_), Some(globalSize_)) =>
                  Right(util.KernelWithSizes(cuKernel,localSize_, globalSize_))
                case _ =>
                  throw new Exception(
                    "At the moment, we assume that local and global size" +
                      "are always provided together.")
              }
          }}))
  }

  private def checkTypes(p: Phrase[ExpType]): Phrase[ExpType] = {
    xmlPrinter.writeToFile("/tmp/p1.xml", p)
    TypeCheck(p)
    p
  }

  private def createOutputParam(outT: ExpType): Identifier[AccType] = {
    identifier("output", AccType(outT.dataType))
  }

  private def rewriteToImperative(
                                   p: Phrase[ExpType],
                                   a: Phrase[AccType],
                                   localSize: Option[LocalSize],
                                   globalSize: Option[GlobalSize]
                                 ): Phrase[CommType] = {
    SimplifyNats(
      UnrollLoops(
        FlagPrivateArrayLoops(
          InjectThreadSizes(localSize, globalSize)(
            TranslationToImperative.acc(
              p)(a)(new shine.cuda.TranslationContext())
              |> (p => {
              xmlPrinter.writeToFile("/tmp/p2.xml", p)
              TypeCheck(p) // TODO: only in debug
              p
            })))))
  }

  private def hoistMemoryAllocations(
                                      p: Phrase[CommType]
                                    ): (Phrase[CommType], List[AllocationInfo]) = {
    HoistMemoryAllocations(p) |> { case (p, intermediateAllocations) =>
      xmlPrinter.writeToFile("/tmp/p4.xml", p)
      TypeCheck(p) // TODO: only in debug
      (p, intermediateAllocations)
    }
  }

  private def adaptKernelParameters(
                                     p: Phrase[CommType],
                                     out: Identifier[AccType],
                                     ins: Seq[Identifier[ExpType]],
                                     intermediateAllocations: Seq[AllocationInfo],
                                     gen: C.CodeGeneration.CodeGenerator
                                   ): (Phrase[CommType],
    Identifier[AccType],
    Seq[Identifier[ExpType]],
    Seq[AllocationInfo],
    Seq[ParamDecl]) = {
    AdaptKernelParameters(p, out, ins, intermediateAllocations, gen) |> { r =>
      val p = r._1
      xmlPrinter.writeToFile("/tmp/p5.xml", p)
      TypeCheck(p) // TODO: only in debug
      r
    }
  }

  private def adaptKernelBody(
                               body: C.AST.Block): (C.AST.Block, Long) = {
    val pw = new PrintWriter(new File("/tmp/p6.cu"))
    try pw.write(printer(body)) finally pw.close()
    AdaptKernelBody(body)
  }

  private def makeKernelFunction(
                                  name: String,
                                  params: Seq[ParamDecl],
                                  body: C.AST.Block, localSize: Option[LocalSize]): OpenCL.AST.KernelDecl = {
    OpenCL.AST.KernelDecl(name, params = params, body = body,
      attribute =
        if (localSize.isEmpty) None
        else Some(RequiredWorkGroupSize(localSize.get.size)))
  }
}