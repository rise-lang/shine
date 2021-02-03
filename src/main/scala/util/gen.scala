package util

import rise.elevate.rules.traversal.default
import shine.C.primitives.imperative.CFunctionDefinition
import shine.{DPIA, Pipe}
import shine.OpenCL.{GlobalSize, LocalSize}
import util.compiler.DSL.run

object gen {
  import shine.C

  type Expr     = rise.core.Expr
  type Phrase   = DPIA.Phrases.Phrase[_ <: DPIA.Types.PhraseType]
  type Module     = C.Module
  type CFunDef  = CFunctionDefinition

  object c {
    object function {
      def fromExpr: Expr => Module =
        gen.c.function().fromExpr

      def asStringFromExpr: Expr => String =
        gen.c.function().asStringFromExpr

      def asString: Module => String =
        gen.functionAsString
    }

    case class function(name: String = "foo") {
      def fromExpr: Expr => Module =
        gen.functionFromExpr(name, C.CodeGenerator())

      def asStringFromExpr: Expr => String =
        gen.functionAsStringFromExpr(name, C.CodeGenerator())
    }
  }

  object openmp {
    import shine.OpenMP

    object function {
      def fromExpr: Expr => Module =
        gen.openmp.function().fromExpr

      def asStringFromExpr: Expr => String = {
        gen.openmp.function().asStringFromExpr
      }

      def asString: Module => String =
        gen.functionAsString
    }

    case class function(name: String = "foo") {
      def fromExpr: Expr => Module =
        gen.functionFromExpr(name, OpenMP.CodeGenerator())

      def asStringFromExpr: Expr => String =
        gen.functionAsStringFromExpr(name, OpenMP.CodeGenerator())
    }
  }

  object opencl {
    import shine.OpenCL
    import shine.OpenCL.primitives.imperative.OpenCLKernelDefinition

    type KernelDef = OpenCLKernelDefinition
    type KernelModule = OpenCL.KernelModule

    sealed trait WorkGroupConfig
    case class LocalAndGlobalSize(localSize: LocalSize, globalSize: GlobalSize) extends WorkGroupConfig
    case class PhraseDepLocalAndGlobalSize(f: Phrase => LocalAndGlobalSize) extends WorkGroupConfig

    object kernel {
      def apply(name: String = "foo"): kernel =
        new kernel(None, name)

      def apply(localSize: LocalSize, globalSize: GlobalSize): kernel =
        new kernel(Some(LocalAndGlobalSize(localSize, globalSize)), "foo")

      def apply(localSize: LocalSize, globalSize: GlobalSize, name: String): kernel =
        new kernel(Some(LocalAndGlobalSize(localSize, globalSize)), name)

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
            kernelDefToKernel(Some(f(p))))
        case Some(config: LocalAndGlobalSize) =>
          phraseToKernelDef(name) andThen
            kernelDefToKernel(Some(config))
        case None =>
          phraseToKernelDef(name) andThen
            kernelDefToKernel(None)
      }

      def asStringFromExpr: Expr => String =
        fromExpr andThen
          gen.opencl.kernel.asString

      def asStringFromPhrase: Phrase => String =
        fromPhrase andThen
          gen.opencl.kernel.asString
    }

    def phraseToKernelDef(name: String): Phrase => KernelDef =
      OpenCLKernelDefinition.fromPhrase(name)

    def kernelDefToKernel(wgConfig: Option[LocalAndGlobalSize]): KernelDef => KernelModule = wgConfig match {
      case Some(LocalAndGlobalSize(localSize, globalSize)) =>
        shine.OpenCL.KernelModule.fromKernelDef(Some(localSize, globalSize))
      case None =>
        shine.OpenCL.KernelModule.fromKernelDef(None)
    }

    object hosted {
      def apply(name: String = "foo"): hosted = new hosted(name)

      def fromExpr: Expr => OpenCL.Module = gen.opencl.hosted().fromExpr
      def fromPhrase: Phrase => OpenCL.Module = gen.opencl.hosted().fromPhrase
    }

    case class hosted(name: String) {
      def fromExpr: Expr => OpenCL.Module = exprToPhrase andThen fromPhrase
      def fromPhrase: Phrase => OpenCL.Module = OpenCL.Module.fromPhrase(OpenCL.HostCodeGenerator(), name)
    }
  }

  private def exprToPhrase: Expr => Phrase =
    shine.DPIA.fromRise(_)(default.RiseTraversable)

  private def phraseToFunDef(name: String): Phrase => CFunDef =
    CFunctionDefinition.fromPhrase(name)

  private def funDefToFunction(gen: shine.C.CodeGenerator): CFunDef => Module =
    shine.C.Module.fromCFunDef(gen)

  private def functionFromExpr(name: String = "foo",
                               gen: shine.C.CodeGenerator = shine.C.CodeGenerator()): Expr => Module =
    exprToPhrase andThen
      phraseToFunDef(name) andThen
        funDefToFunction(gen)

  private def functionAsStringFromExpr(name: String = "foo",
                                       gen: shine.C.CodeGenerator = shine.C.CodeGenerator()): Expr => String =
    functionFromExpr(name, gen) andThen
      C.Module.translateToString andThen
        run(SyntaxChecker(_))

  private def functionAsString: Module => String =
    // FIXME: SyntaxChecker disabled for host code prototype
    C.Module.translateToString/* _ andThen
      run(SyntaxChecker(_))*/
}