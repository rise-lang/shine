package util

import rise.elevate.rules.traversal.default
import shine.C.Compilation.{CodeGenerator => CCodeGenerator, ModuleGenerator => CModuleGenerator}
import shine.DPIA.Compilation.FunDef
import shine.OpenCL.Compilation._
import shine.OpenCL._
import shine.{C, DPIA, Pipe}
import util.compiler.DSL._
import util.compiler.PartialCompiler

object gen {
  type Expr     = rise.core.Expr
  type Phrase   = DPIA.Phrases.Phrase[_ <: DPIA.Types.PhraseType]

  private def exprToPhrase: Expr => Phrase =
    shine.DPIA.fromRise(_)(default.RiseTraversable)

  type CModule  = C.Module

  object c {

    object function {
      def fromExpr: Expr => CModule =
        gen.c.function().fromExpr

      def asStringFromExpr: Expr => String =
        gen.c.function().asStringFromExpr

      def asString: CModule => String =
        gen.functionAsString
    }

    case class function(name: String = "foo") {
      def fromExpr: Expr => CModule =
        gen.functionFromExpr(name)

      def asStringFromExpr: Expr => String =
        gen.functionAsStringFromExpr(name)
    }
  }

  private def funDefToFunction(name: String,
                               gen: CCodeGenerator): Phrase => CModule =
    CModuleGenerator.generateFunction(name, gen)

  private def functionFromExpr(name: String = "foo",
                               gen: CCodeGenerator = CCodeGenerator()
                              ): Expr => CModule =
    exprToPhrase andThen
      funDefToFunction(name, gen)

  private def functionAsStringFromExpr(name: String = "foo",
                                       gen: CCodeGenerator = CCodeGenerator()
                                      ): Expr => String =
    functionFromExpr(name, gen) andThen
      C.Module.translateToString andThen
      run(SyntaxChecker(_))

  private def functionAsString: CModule => String =
    C.Module.translateToString _ andThen
      run(SyntaxChecker(_))

  object openmp {
    import shine.OpenMP

    object function {
      def fromExpr: Expr => CModule =
        gen.openmp.function().fromExpr

      def asStringFromExpr: Expr => String = {
        gen.openmp.function().asStringFromExpr
      }

      def asString: CModule => String =
        gen.functionAsString
    }

    case class function(name: String = "foo") {
      def fromExpr: Expr => CModule =
        gen.functionFromExpr(name, OpenMP.CodeGenerator())

      def asStringFromExpr: Expr => String =
        gen.functionAsStringFromExpr(name, OpenMP.CodeGenerator())
    }
  }

  object opencl {
    import shine.OpenCL

    type KernelModule = OpenCL.KernelModule

    sealed trait WorkGroupConfig
    case class LocalAndGlobalSize(wgConfig: (LocalSize, GlobalSize)) extends WorkGroupConfig
    case class PhraseDepLocalAndGlobalSize(f: Phrase => LocalAndGlobalSize) extends WorkGroupConfig

    object kernel {
      def apply(name: String = "foo"): kernel =
        new kernel(None, name)

      def apply(localSize: LocalSize, globalSize: GlobalSize): kernel =
        new kernel(Some(LocalAndGlobalSize((localSize, globalSize))), "foo")

      def apply(localSize: LocalSize, globalSize: GlobalSize, name: String): kernel =
        new kernel(Some(LocalAndGlobalSize((localSize, globalSize))), name)

      def fromExpr: Expr => KernelModule =
        gen.opencl.kernel().fromExpr

      def fromPhrase: Phrase => KernelModule =
        gen.opencl.kernel().fromPhrase

      def asStringFromExpr: Expr => String =
        gen.opencl.kernel().asStringFromExpr

      def asStringFromPhrase: Phrase => String =
        gen.opencl.kernel().asStringFromPhrase

      def asString: KernelModule => String =
        OpenCL.KernelModule.translationToString _ andThen
        run(SyntaxChecker.checkOpenCL)
    }

    case class kernel(wgConfig: Option[WorkGroupConfig], name: String) {
      def fromExpr: Expr => KernelModule =
        exprToPhrase andThen
          fromPhrase

      def fromPhrase: Phrase => KernelModule = wgConfig match {
        case Some(PhraseDepLocalAndGlobalSize(f)) => p => p |>
          kernelDefToKernel(name, Some(f(p)))
        case Some(config: LocalAndGlobalSize) =>
          kernelDefToKernel(name, Some(config))
        case None =>
          kernelDefToKernel(name, None)
      }

      def asStringFromExpr: Expr => String =
        fromExpr andThen
          gen.opencl.kernel.asString

      def asStringFromPhrase: Phrase => String =
        fromPhrase andThen
          gen.opencl.kernel.asString
    }

    private def kernelDefToKernel(name: String,
                                  wgConfig: Option[LocalAndGlobalSize]
                                 ): Phrase => KernelModule =
      KernelModuleGenerator(wgConfig.map(_.wgConfig))
        .generateFunction(name, shine.OpenCL.Compilation.KernelCodeGenerator())

    private def sizedKernelDefToKernel: SizedKernelDef => KernelModule = {
      case (localSize, globalSize, kernelDef) =>
        KernelModuleGenerator(Some(localSize, globalSize))
          .funDefToModule(shine.OpenCL.Compilation.KernelCodeGenerator())(kernelDef)
    }

    type HostedModule = OpenCL.Module
    type SizedKernelDef = SeparateHostAndKernelCode.SizedKernelDef

    object hosted {
      def fromExpr: Expr => HostedModule = gen.opencl.hosted().fromExpr
      def fromPhrase: Phrase => HostedModule = gen.opencl.hosted().fromPhrase
      def asString: HostedModule => String = OpenCL.Module.translateToString
    }

    case class hosted(name: String = "foo") {
      def fromExpr: Expr => HostedModule = exprToPhrase andThen fromPhrase

      def fromPhrase: Phrase => HostedModule =
        partialHostCompiler(name) composeWith
          (   hostFunDefToHostPart()
            x map(sizedKernelDefToKernel) )
    }

    private def hostFunDefToHostPart(gen: HostCodeCodeGenerator =
                                        shine.OpenCL.Compilation.HostCodeCodeGenerator()
                                    ): FunDef => CModule =
      HostCodeModuleGenerator.funDefToModule(gen)

    private def partialHostCompiler(hostFunName: String): PartialCompiler[
      Phrase, HostedModule,
      (FunDef,  Seq[SizedKernelDef]),
      (CModule, Seq[KernelModule])] =
        PartialCompiler.functor(
          Compilation.SeparateHostAndKernelCode.separate(hostFunName),
          (OpenCL.Module.apply _).tupled )
  }
}
