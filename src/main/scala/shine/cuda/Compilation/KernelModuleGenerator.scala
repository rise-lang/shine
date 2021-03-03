package shine.cuda.Compilation

import arithexpr.arithmetic.Cst
import shine.C.AST.ParamKind
import shine.C.Compilation.{ModuleGenerator => CModuleGenerator}
import shine.DPIA.Compilation.ModuleGenerator
import shine.DPIA.Phrases._
import shine.DPIA.Types._
import shine.OpenCL.AST.RequiredWorkGroupSize
import shine.OpenCL.Compilation.KernelDef
import shine.OpenCL.Compilation.Passes.{AdaptKernelParameters, FlagPrivateArrayLoops}
import shine.OpenCL.Compilation.Passes.HoistMemoryAllocations.AllocationInfo
import shine.cuda.Compilation.Passes._
import shine.OpenCL._
import shine.{C, OpenCL}
import util.TupleOps._

import scala.collection.immutable

object KernelModuleGenerator extends ModuleGenerator[KernelDef] {
  override type Module = shine.cuda.KernelModule
  override type CodeGenerator = KernelCodeGenerator

  override def createOutputParam(outT: ExpType): Identifier[AccType] =
    CModuleGenerator.createOutputParam(outT)

  override def toImperative(gen: KernelCodeGenerator,
                            funDef: KernelDef,
                            outParam: Identifier[AccType]
                           ): Phrase[ExpType] => Phrase[CommType] =
    CModuleGenerator.toImperative(gen, funDef, outParam)


  override def imperativeToModule(gen: KernelCodeGenerator,
                                  funDef: KernelDef,
                                  outParam: Identifier[AccType]
                                 ): Phrase[CommType] => shine.cuda.KernelModule =
    imperativePasses(gen, funDef, outParam) andThen
      generateCode(gen, funDef) andThen
      makeKernelModule(funDef)

  type PhraseAfterPasses = (
    Identifier[AccType],
      Seq[Identifier[ExpType]],
      Seq[AllocationInfo],
      Seq[C.AST.ParamDecl],
      Phrase[CommType]
    )
  def imperativePasses(gen: KernelCodeGenerator,
                       funDef: KernelDef,
                       outParam: Identifier[AccType]
                      ): Phrase[CommType] => PhraseAfterPasses = {
    InjectThreadSizes.inject(funDef.wgConfig) andThen
      FlagPrivateArrayLoops.flag andThen
      CModuleGenerator.imperativePasses andThen
      HoistMemoryAllocations.hoist andThen
      AdaptKernelParameters.adapt(gen, outParam, funDef.params)
  }

  type GeneratedCode = (
    Identifier[AccType],
      Seq[AllocationInfo],
      Seq[C.AST.ParamDecl],
      Seq[C.AST.Decl],
      (C.AST.Stmt,
      Long)
    )
  def generateCode(gen: KernelCodeGenerator,
                   funDef: KernelDef): PhraseAfterPasses => GeneratedCode = {
    case (outParam, params, temps, kernelParams, p) =>
      val identMap: Map[Identifier[_ <: BasePhraseType], C.AST.DeclRef] =
        (outParam +: params).map( p => p -> C.AST.DeclRef(p.name) ).toMap

      val tempsIdentMap =
        temps.flatMap(temp => Seq(
          Identifier(
            s"${temp.identifier.name}_1",
            temp.identifier.`type`.t1
          ) -> C.AST.DeclRef(temp.identifier.name),
          Identifier(
            s"${temp.identifier.name}_2",
            temp.identifier.`type`.t2
          ) -> C.AST.DeclRef(temp.identifier.name))).toMap

      val env = shine.DPIA.Compilation.CodeGenerator.Environment(
        identMap ++ tempsIdentMap,
        immutable.Map.empty, immutable.Map.empty, immutable.Map.empty)

      p |> (
        gen.generate(funDef.topLevelLetNats, env) andThen
          T2.mapSnd(AdaptKernelBody.adapt) andThen
          ((outParam, temps, kernelParams) ++ _) )
  }

  def makeKernelModule(funDef: KernelDef): GeneratedCode => shine.cuda.KernelModule = {
    case (outParam, temps, kernelParams, declarations, (code, dynamicSharedMemory)) =>
      shine.cuda.KernelModule(
        decls = CModuleGenerator.collectTypeDeclarations(code, kernelParams) ++
          declarations,
        kernels = immutable.Seq(
          shine.cuda.AST.Kernel(
            code = OpenCL.AST.KernelDecl(funDef.name,
              params = kernelParams,
              body = code,
              attribute = funDef.wgConfig match {
                case Some((LocalSize(cstSize @ NDRange(Cst(_), Cst(_), Cst(_))), _)) =>
                  Some(RequiredWorkGroupSize(cstSize))
                case _ => None
              }
            ),
            paramKinds = ParamKind.output(outParam) +:
              ( funDef.params.map(ParamKind.input) ++
                temps.map(_.identifier.t.t1.dataType).map(ParamKind.temporary)),
            wgConfig = funDef.wgConfig,
            dynamicSharedMemory
          )
        )
      )
  }
}
