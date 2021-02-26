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
    (FunDef(name, _)) andThen
      CModuleGenerator.funDefToModule(gen)

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
        case Some(PhraseDepLocalAndGlobalSize(f)) => p => p |> (
          phraseToKernelDef(name) andThen
            kernelWithWgConfig(f(p)) andThen
            kernelDefToKernel())
        case Some(config: LocalAndGlobalSize) =>
          phraseToKernelDef(name) andThen
            kernelWithWgConfig(config) andThen
            kernelDefToKernel()
        case None =>
          phraseToKernelDef(name) andThen
            kernelDefToKernel()
      }

      def asStringFromExpr: Expr => String =
        fromExpr andThen
          gen.opencl.kernel.asString

      def asStringFromPhrase: Phrase => String =
        fromPhrase andThen
          gen.opencl.kernel.asString
    }

    private def phraseToKernelDef(name: String): Phrase => KernelDef =
      KernelDef(name, _)

    private def kernelWithWgConfig: LocalAndGlobalSize => KernelDef => KernelDef = {
      case LocalAndGlobalSize((ls, gs)) =>_.withWgConfig(ls, gs)
    }

    private def kernelDefToKernel(): KernelDef => KernelModule =
      KernelModuleGenerator.funDefToModule(
        shine.OpenCL.Compilation.KernelCodeGenerator())

    type HostedModule = OpenCL.Module

    object hosted {
      def fromExpr: Expr => HostedModule = gen.opencl.hosted().fromExpr
      def fromPhrase: Phrase => HostedModule = gen.opencl.hosted().fromPhrase
      def asString: HostedModule => String = OpenCL.Module.translateToString
    }

    case class hosted(name: String = "foo") {
      def fromExpr: Expr => HostedModule = exprToPhrase andThen fromPhrase

      def fromPhrase: Phrase => HostedModule =
        partialHostCompiler(name) composeWith
          (hostFunDefToHostPart() x map(kernelDefToKernel()))
    }

    private def hostFunDefToHostPart(gen: HostCodeGenerator =
                                        shine.OpenCL.Compilation.HostCodeGenerator()
                                    ): FunDef => CModule =
      HostCodeModuleGenerator.funDefToModule(gen)

    private def partialHostCompiler(hostFunName: String): PartialCompiler[
      Phrase,   HostedModule,
      (FunDef,  Seq[KernelDef]),
      (CModule, Seq[KernelModule])] =
        PartialCompiler.functor(
          OpenCL.Compilation.SeparateHostAndKernelCode.separate(hostFunName),
          (OpenCL.Module.apply _).tupled)
  }
}
