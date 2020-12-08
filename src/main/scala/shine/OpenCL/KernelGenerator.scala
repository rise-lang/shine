package shine.OpenCL

import java.io.{File, PrintWriter}

import shine.C.AST.{DeclRef, ParamDecl}
import shine.DPIA.Compilation._
import shine.DPIA.DSL._
import shine.DPIA.Phrases._
import shine.DPIA.Types._
import shine.DPIA._
import shine.OpenCL.AST.RequiredWorkGroupSize
import shine.OpenCL.CodeGeneration.HoistMemoryAllocations.AllocationInfo
import shine.OpenCL.CodeGeneration.{AdaptKernelBody, AdaptKernelParameters, HoistMemoryAllocations}
import shine._
import util.{KernelNoSizes, KernelWithSizes}

import scala.annotation.tailrec
import scala.collection._

//noinspection VariablePatternShadow
object KernelGenerator {
  def makeCode[T <: PhraseType](localSize: LocalSize, globalSize: GlobalSize)
                               (originalPhrase: Phrase[T], name: String): KernelWithSizes = {
    val (phrase, params, defs) = getPhraseAndParams(originalPhrase, immutable.Seq(), immutable.Seq())
    makeKernel(name, phrase, params.reverse, defs.reverse,
      Some(localSize), Some(globalSize)).getOrElse(throw new Exception("Expected KernelWithSizes"))
  }

  def makeCode[T <: PhraseType](originalPhrase: Phrase[T], name: String = "KERNEL"): KernelNoSizes = {
    val (phrase, params, defs) = getPhraseAndParams(originalPhrase, immutable.Seq(), immutable.Seq())
    makeKernel(name, phrase, params.reverse, defs.reverse,
      None, None).swap.getOrElse(throw new Exception("Expected KernelNoSizes"))
  }


  @tailrec
  private def getPhraseAndParams[_ <: PhraseType](p: Phrase[_],
                                                  ps: immutable.Seq[Identifier[ExpType]],
                                                  defs: immutable.Seq[(LetNatIdentifier, Phrase[ExpType])]):
  ( Phrase[ExpType], immutable.Seq[Identifier[ExpType]],
    immutable.Seq[(LetNatIdentifier, Phrase[ExpType])] ) =
  {
    p match {
      case Apply(f, a) => getPhraseAndParams(Lifting.liftFunction(f).reducing(a), ps, defs)
      case DepApply(f, a) => getPhraseAndParams(Lifting.liftDependentFunction(f)(a), ps, defs)
      case l: Lambda[ExpType, _]@unchecked => getPhraseAndParams(l.body, l.param +: ps, defs)
      case ndl: DepLambda[NatKind, _]@unchecked => getPhraseAndParams(ndl.body, Identifier(ndl.x.name, ExpType(int, read)) +: ps, defs)
      case ln: LetNat[ExpType, _]@unchecked => getPhraseAndParams(ln.body, ps, (ln.binder, ln.defn) +: defs)
      case ep: Phrase[ExpType]@unchecked => (ep, ps, defs)
    }
  }

  private def makeKernel(name:String,
                         p: Phrase[ExpType],
                         inputParams: immutable.Seq[Identifier[ExpType]],
                         letNatDefs: immutable.Seq[(LetNatIdentifier, Phrase[ExpType])],
                         localSize: Option[LocalSize],
                         globalSize: Option[GlobalSize]): Either[KernelNoSizes, KernelWithSizes] = {

    val outParam = createOutputParam(outT = p.t)

    val gen = OpenCL.CodeGeneration.CodeGenerator()

    checkTypes(p) |> (p =>

    rewriteToImperative(p, outParam, localSize, globalSize) |> (p =>

    hoistMemoryAllocations(p) |> { case (p, intermediateAllocations) =>

    adaptKernelParameters(p, outParam, inputParams, intermediateAllocations, gen) |> {
      case (p, outParam, inputParams, intermediateAllocations, kernelParams) =>

      val identMap: Predef.Map[Identifier[_ <: BasePhraseType], DeclRef] =
        (outParam +: inputParams).map( p => p -> C.AST.DeclRef(p.name) ).toMap

      val intermediateIdentMap: Predef.Map[Identifier[_ <: BasePhraseType], DeclRef] =
          intermediateAllocations.flatMap( p =>
            Seq(Identifier(s"${p.identifier.name}_1", p.identifier.`type`.t1) -> C.AST.DeclRef(p.identifier.name),
                Identifier(s"${p.identifier.name}_2", p.identifier.`type`.t2) -> C.AST.DeclRef(p.identifier.name) ) ).toMap

      val env = C.CodeGeneration.CodeGenerator.Environment(identMap ++ intermediateIdentMap,
        immutable.Map.empty, immutable.Map.empty, immutable.Map.empty)

      val (declarations, code) = gen.generate(p, letNatDefs, env)

      val typeDeclarations = C.ProgramGenerator.collectTypeDeclarations(code, kernelParams)

      val oclKernel = OpenCL.Kernel(typeDeclarations ++ declarations,
            kernel = makeKernelFunction(name, kernelParams, adaptKernelBody(C.AST.Block(immutable.Seq(code))), localSize),
            outputParam = outParam,
            inputParams = inputParams,
            intermediateParams = intermediateAllocations.map (_.identifier),
            OpenCL.AST.Printer(_))

      (localSize, globalSize) match {
        case (None, None) => Left(KernelNoSizes(oclKernel))
        case (Some(localSize_), Some(globalSize_)) => Right(KernelWithSizes(oclKernel,localSize_, globalSize_))
        case _ => throw new Exception("At the moment, we assume that local and global size are always provided together.")
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

  private def rewriteToImperative(p: Phrase[ExpType], a: Phrase[AccType],
                                  localSize: Option[LocalSize], globalSize: Option[GlobalSize]): Phrase[CommType] = {
    SimplifyNats(UnrollLoops(FlagPrivateArrayLoops(InjectWorkItemSizes(localSize, globalSize)(TranslationToImperative.acc(p)(a)(
      new shine.OpenCL.TranslationContext) |> (p => {
      xmlPrinter.writeToFile("/tmp/p2.xml", p)
      TypeCheck(p) // TODO: only in debug
      p
    })))))
  }

  private def hoistMemoryAllocations(p: Phrase[CommType]): (Phrase[CommType], List[AllocationInfo]) = {
    HoistMemoryAllocations(p) |> { case (p, intermediateAllocations) =>
      xmlPrinter.writeToFile("/tmp/p4.xml", p)
      TypeCheck(p) // TODO: only in debug
      (p, intermediateAllocations)
    }
  }

  private def adaptKernelParameters(p: Phrase[CommType],
                                    out: Identifier[AccType],
                                    ins: immutable.Seq[Identifier[ExpType]],
                                    intermediateAllocations: immutable.Seq[AllocationInfo],
                                    gen: CodeGeneration.CodeGenerator
                                   ): (Phrase[CommType], Identifier[AccType], immutable.Seq[Identifier[ExpType]], immutable.Seq[AllocationInfo], immutable.Seq[ParamDecl]) = {
    AdaptKernelParameters(p, out, ins, intermediateAllocations, gen) |> { r =>
      val p = r._1
      xmlPrinter.writeToFile("/tmp/p5.xml", p)
      TypeCheck(p) // TODO: only in debug
      r
    }
  }

  private def adaptKernelBody(body: C.AST.Block): C.AST.Block = {
    val pw = new PrintWriter(new File("/tmp/p6.cl"))
    try pw.write(shine.OpenCL.AST.Printer(body)) finally pw.close()
    AdaptKernelBody(body)
  }

  private def makeKernelFunction(name: String,
                                 params: immutable.Seq[ParamDecl],
                                 body: C.AST.Block, localSize: Option[LocalSize]): OpenCL.AST.KernelDecl = {
    OpenCL.AST.KernelDecl(name, params = params, body = body,
                          attribute = if (localSize.isEmpty) None else Some(RequiredWorkGroupSize(localSize.get.size)))
  }

}
