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
  type C_TU     = C.TranslationUnit
  type CFunDef  = CFunctionDefinition

  object c {
    object function {
      def fromExpr: Expr => C_TU =
        gen.c.function().fromExpr

      def asStringFromExpr: Expr => String =
        gen.c.function().asStringFromExpr

      def asString: C_TU => String =
        gen.functionAsString
    }

    case class function(name: String = "foo") {
      def fromExpr: Expr => C_TU =
        gen.functionFromExpr(name, C.CodeGenerator())

      def asStringFromExpr: Expr => String =
        gen.functionAsStringFromExpr(name, C.CodeGenerator())
    }
  }

  object openmp {
    import shine.OpenMP

    object function {
      def fromExpr: Expr => C_TU =
        gen.openmp.function().fromExpr

      def asStringFromExpr: Expr => String = {
        gen.openmp.function().asStringFromExpr
      }

      def asString: C_TU => String =
        gen.functionAsString
    }

    case class function(name: String = "foo") {
      def fromExpr: Expr => C_TU =
        gen.functionFromExpr(name, OpenMP.CodeGenerator())

      def asStringFromExpr: Expr => String =
        gen.functionAsStringFromExpr(name, OpenMP.CodeGenerator())
    }
  }

  object opencl {
    import shine.OpenCL
    import shine.OpenCL.primitives.imperative.OpenCLKernelDefinition

    type KernelDef = OpenCLKernelDefinition
    type Kernel_TU = OpenCL.KernelTranslationUnit

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

      def fromExpr: Expr => Kernel_TU =
        gen.opencl.kernel().fromExpr

      def fromPhrase: Phrase => Kernel_TU =
        gen.opencl.kernel().fromPhrase

      def asStringFromExpr: Expr => String =
        gen.opencl.kernel().asStringFromExpr

      def asStringFromPhrase: Phrase => String =
        gen.opencl.kernel().asStringFromPhrase

      def asString: Kernel_TU => String =
        OpenCL.KernelTranslationUnit.translationToString
    }

    case class kernel(wgConfig: Option[WorkGroupConfig], name: String) {
      def fromExpr: Expr => Kernel_TU =
        exprToPhrase andThen
          fromPhrase

      def fromPhrase: Phrase => Kernel_TU = wgConfig match {
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

    def kernelDefToKernel(wgConfig: Option[LocalAndGlobalSize]): KernelDef => Kernel_TU = wgConfig match {
      case Some(LocalAndGlobalSize(localSize, globalSize)) =>
        shine.OpenCL.KernelTranslationUnit.fromKernelDef(Some(localSize, globalSize))
      case None =>
        shine.OpenCL.KernelTranslationUnit.fromKernelDef(None)
    }
  }

  private def exprToPhrase: Expr => Phrase =
    shine.DPIA.fromRise(_)(default.RiseTraversable)

  private def phraseToFunDef(name: String): Phrase => CFunDef =
    CFunctionDefinition.fromPhrase(name)

  private def funDefToFunction(gen: shine.C.CodeGenerator): CFunDef => C_TU =
    shine.C.TranslationUnit.fromCFunDef(gen)

  private def functionFromExpr(name: String = "foo",
                               gen: shine.C.CodeGenerator = shine.C.CodeGenerator()): Expr => C_TU =
    exprToPhrase andThen
      phraseToFunDef(name) andThen
        funDefToFunction(gen)

  private def functionAsStringFromExpr(name: String = "foo",
                                       gen: shine.C.CodeGenerator = shine.C.CodeGenerator()): Expr => String =
    functionFromExpr(name, gen) andThen
      C.TranslationUnit.translateToString andThen
        run(SyntaxChecker(_))

  private def functionAsString: C_TU => String =
    C.TranslationUnit.translateToString _ andThen
      run(SyntaxChecker(_))
}