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
      def fromExpr(name: String = "foo"): Expr => C_TU =
        gen.functionFromExpr(name, C.CodeGenerator())

      def asStringFromExpr(name: String = "foo"): Expr => String =
        gen.functionAsStringFromExpr(name, C.CodeGenerator())

      def asString: C_TU => String =
        gen.functionAsString
    }
  }

  object openmp {
    import shine.OpenMP

    object function {
      def fromExpr(name: String = "foo"): Expr => C_TU =
        gen.functionFromExpr(name, OpenMP.CodeGenerator())

      def asStringFromExpr(name: String = "foo"): Expr => String =
        gen.functionAsStringFromExpr(name, OpenMP.CodeGenerator())

      def asString: C_TU => String =
        gen.functionAsString
    }
  }

  object opencl {
    import shine.OpenCL
    import shine.OpenCL.primitives.imperative.OpenCLKernelDefinition

    type KernelDef = OpenCLKernelDefinition
    type Kernel_TU = OpenCL.KernelTranslationUnit

    object kernel {
      def fromExpr(name: String = "foo",
                   wgConfig: Option[(LocalSize, GlobalSize)] = None): Expr => Kernel_TU =
        exprToPhrase andThen
          gen.opencl.kernel.fromPhrase(name, wgConfig)

      def fromExpr(name: String,
                   wgConfig: DPIA.Phrases.Phrase[_ <: DPIA.Types.PhraseType] => (LocalSize, GlobalSize)
                  ): Expr => Kernel_TU =
        exprToPhrase andThen
          gen.opencl.kernel.fromPhrase(name, wgConfig)

      def fromPhrase(name: String = "foo",
                     wgConfig: Option[(LocalSize, GlobalSize)] = None): Phrase => Kernel_TU =
        phraseToKernelDef(name) andThen
          kernelDefToKernel(wgConfig)

      def fromPhrase(name: String,
                     wgConfig: DPIA.Phrases.Phrase[_ <: DPIA.Types.PhraseType] => (LocalSize, GlobalSize)
                    ): Phrase => Kernel_TU = p => p |>
        (phraseToKernelDef(name) andThen
          kernelDefToKernel(Some(wgConfig(p))))

      def asStringFromExpr(name: String = "foo",
                           wgConfig: Option[(LocalSize, GlobalSize)] = None): Expr => String =
        kernel.fromExpr(name, wgConfig) andThen
          gen.opencl.kernel.asString

      def asStringFromPhrase(name: String = "foo",
                             wgConfig: Option[(LocalSize, GlobalSize)] = None): Phrase => String =
        kernel.fromPhrase(name, wgConfig) andThen
          gen.opencl.kernel.asString

      def asString: Kernel_TU => String =
        OpenCL.KernelTranslationUnit.translationToString
    }

    def phraseToKernelDef(name: String): Phrase => KernelDef =
      OpenCLKernelDefinition.fromPhrase(name)

    def kernelDefToKernel(wgConfig: Option[(LocalSize, GlobalSize)]): KernelDef => Kernel_TU =
      shine.OpenCL.KernelTranslationUnit.fromKernelDef(wgConfig)
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