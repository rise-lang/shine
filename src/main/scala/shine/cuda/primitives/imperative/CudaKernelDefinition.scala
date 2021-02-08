package shine.cuda.primitives.imperative

import arithexpr.arithmetic.Cst
import shine.C.ParamMetaData
import shine.C.primitives.imperative.CFunctionDefinition
import shine.DPIA.Compilation._
import shine.DPIA.DSL._
import shine.DPIA.Phrases._
import shine.DPIA.Types._
import shine.DPIA._
import shine.DPIA.primitives.functional
import shine.OpenCL.AST.RequiredWorkGroupSize
import shine.OpenCL.compilation.HoistMemoryAllocations.AllocationInfo
import shine.OpenCL.compilation.{AdaptKernelParameters, FlagPrivateArrayLoops, InsertMemoryBarriers}
import shine.OpenCL.{GlobalSize, LocalSize}
import shine.cuda.compilation._
import shine.cuda.KernelModule
import shine.macros.Primitive.comPrimitive
import shine.{C, DPIA, OpenCL}
import util.compiler.DSL.run

import scala.collection.immutable

//Todo similar to OpenCLKernelDefinition
@comPrimitive
final case class CudaKernelDefinition(name: String,
                                      definition: Phrase[_ <: PhraseType]) extends CommandPrimitive {
  private val cFunDef = CFunctionDefinition(name, definition)
  val body: Phrase[ExpType] = cFunDef.body
  val params: Seq[Identifier[ExpType]] = cFunDef.params
  val topLevelLetNats: Seq[(LetNatIdentifier, Phrase[ExpType])] = cFunDef.topLevelLetNats

  val returnType: ExpType = cFunDef.returnType

  val paramTypes: Seq[ExpType] = cFunDef.paramTypes

  override def prettyPrint: String = cFunDef.prettyPrint

  type CodeGenerator = OpenCL.CodeGenerator

  def translateToModule(gen: CodeGenerator)
                       (wgConfig: Option[(LocalSize, GlobalSize)]): KernelModule = {
    val outParam = cFunDef.createOutputParam(outT = body.t)

    body |>
      ( run(TypeCheck(_: Phrase[ExpType])) andThen
        rewriteToImperative(gen)(wgConfig)(outParam) andThen
        InsertMemoryBarriers.insert andThen
        HoistMemoryAllocations.hoist andThen { case (temps, phrase) =>
        AdaptKernelParameters.adapt(gen)(outParam, params, temps)(phrase) } andThen {
          case (outParam, params, temps, kernelParams, phrase) =>
            phrase |>
              ( generateCode(gen)(outParam, params, temps) _ andThen { case (decls, code) =>
                val (codeBlock, dynamicSharedMemory) = AdaptKernelBody.adapt(C.AST.Block(immutable.Seq(code)))
                (decls, codeBlock, dynamicSharedMemory) } andThen
                makeModule(gen)(outParam, temps, kernelParams, wgConfig) )
      } )
  }

  private def makeModule(gen: CodeGenerator)
                        (outputParam: Identifier[AccType],
                         temps: Seq[AllocationInfo],
                         kernelParams: Seq[C.AST.ParamDecl],
                         wgConfig: Option[(LocalSize, GlobalSize)]
                        ): ((immutable.Seq[gen.Decl], gen.Stmt, Long)) => KernelModule = {
    case (declarations, code, dynamicSharedMemory) =>
      KernelModule(
        decls = CFunctionDefinition.collectTypeDeclarations(code, kernelParams) ++ declarations,
        kernels = immutable.Seq(
          shine.cuda.Kernel(
            code = OpenCL.AST.KernelDecl(name,
              params = kernelParams,
              body = code,
              attribute = wgConfig.map(_._1).map(localSize => RequiredWorkGroupSize(localSize.size))),
              paramKinds = ParamMetaData(outputParam.`type`.dataType, C.ParamMetaData.Kind.output) +:
                ( this.params.map(p => ParamMetaData(p.`type`.dataType, C.ParamMetaData.Kind.input)) ++
                  temps.map(t => ParamMetaData(t.identifier.`type`.t1.dataType, C.ParamMetaData.Kind.temporary))),
            wgConfig = wgConfig,
            dynamicSharedMemory)))
  }

  private def rewriteToImperative(gen: CodeGenerator)
                                 (wgConfig: Option[(LocalSize, GlobalSize)])
                                 (outParam: Phrase[AccType])(p: Phrase[ExpType]): Phrase[CommType] = {
    implicit val context: shine.DPIA.Compilation.TranslationContext = gen.translationContext

    val output = (outParam.t.dataType, p.t.dataType) match {
      case (lhsT, rhsT) if lhsT == rhsT => outParam
      case (DPIA.Types.ArrayType(Cst(1), lhsT), rhsT) if lhsT == rhsT =>
        outParam `@` functional.NatAsIndex(1, Natural(0))
      case (lhsT, rhsT) => throw new Exception(s" $lhsT and $rhsT should match")
    }

    val (localSize, globalSize) = wgConfig match {
      case Some((localSize, globalSize)) => (Some(localSize), Some(globalSize))
      case _ => (None, None)
    }

    output |>
      ( TranslationToImperative.acc(p) _ andThen
        run(TypeCheck(_)) andThen
        InjectThreadSizes.inject(localSize, globalSize) andThen
        FlagPrivateArrayLoops.flag andThen
        UnrollLoops.unroll andThen
        SimplifyNats.simplify )
  }

  private def generateCode(gen: CodeGenerator)
                          (outParam: Identifier[AccType],
                           params: Seq[Identifier[ExpType]],
                           temps: Seq[AllocationInfo])
                          (p: Phrase[CommType]): (immutable.Seq[gen.Decl], gen.Stmt) = {
    val identMap: Predef.Map[Identifier[_ <: BasePhraseType], C.AST.DeclRef] =
      (outParam +: params).map( p => p -> C.AST.DeclRef(p.name) ).toMap

    val tempsIdentMap: Predef.Map[Identifier[_ <: BasePhraseType], C.AST.DeclRef] =
      temps.flatMap(temp =>
        Seq(
          Identifier(s"${temp.identifier.name}_1", temp.identifier.`type`.t1) -> C.AST.DeclRef(temp.identifier.name),
          Identifier(s"${temp.identifier.name}_2", temp.identifier.`type`.t2) -> C.AST.DeclRef(temp.identifier.name)
        )).toMap

    val env = shine.DPIA.Compilation.CodeGenerator.Environment(
      identMap ++ tempsIdentMap,
      immutable.Map.empty, immutable.Map.empty, immutable.Map.empty)

    gen.generate(topLevelLetNats, env)(p)
  }
}

object CudaKernelDefinition {
  def fromPhrase(name: String)(p: Phrase[_ <: PhraseType]): CudaKernelDefinition =
    CudaKernelDefinition(name, p)
}
