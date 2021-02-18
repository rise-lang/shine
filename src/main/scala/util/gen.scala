package util

import rise.elevate.rules.traversal.default
import shine.C.primitives.imperative.CFunctionDefinition
import shine.OpenCL.{GlobalSize, LocalSize}
import shine.{DPIA, Pipe}
import util.compiler.DSL._
import util.compiler.PartialCompiler

object gen {
  import shine.C

  type Expr     = rise.core.Expr
  type Phrase   = DPIA.Phrases.Phrase[_ <: DPIA.Types.PhraseType]

  private def exprToPhrase: Expr => Phrase =
    shine.DPIA.fromRise(_)(default.RiseTraversable)

  type CModule  = C.Module
  type CFunDef  = CFunctionDefinition

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
        gen.functionFromExpr(name, C.CodeGenerator())

      def asStringFromExpr: Expr => String =
        gen.functionAsStringFromExpr(name, C.CodeGenerator())
    }
  }

  private def phraseToFunDef(name: String): Phrase => CFunDef =
    CFunctionDefinition.fromPhrase(name)

  private def funDefToFunction(gen: shine.C.CodeGenerator): CFunDef => CModule =
    _.translateToModule(gen)

  private def functionFromExpr(name: String = "foo",
                               gen: shine.C.CodeGenerator =
                                  shine.C.CodeGenerator()
                              ): Expr => CModule =
    exprToPhrase andThen
      phraseToFunDef(name) andThen
      funDefToFunction(gen)

  private def functionAsStringFromExpr(name: String = "foo",
                                       gen: shine.C.CodeGenerator =
                                          shine.C.CodeGenerator()
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

    private def phraseToKernelDef(name: String): Phrase => KernelDef =
      OpenCLKernelDefinition.fromPhrase(name)

    private def kernelDefToKernel(
        wgConfig: Option[LocalAndGlobalSize]
                                 ): KernelDef => KernelModule = wgConfig match {
      case Some(LocalAndGlobalSize(localSize, globalSize)) =>
        _.translateToModule(shine.OpenCL.CodeGenerator())(Some(localSize, globalSize))
      case None =>
        _.translateToModule(shine.OpenCL.CodeGenerator())(None)
    }

    private def sizedKernelDefToKernel: SizedKernelDef => KernelModule = {
      case (localSize, globalSize, kernelDef) =>
        kernelDef.translateToModule(shine.OpenCL.CodeGenerator())(
          Some(localSize, globalSize))
    }

    type HostedModule = OpenCL.Module
    type HostFunDef   = OpenCL.HostFunctionDefinition
    type SizedKernelDef = OpenCL.SeparateHostAndKernelCode.SizedKernelDef

    object hosted {
      def fromExpr: Expr => HostedModule = gen.opencl.hosted().fromExpr
      def fromPhrase: Phrase => HostedModule = gen.opencl.hosted().fromPhrase
      def asString: HostedModule => String = OpenCL.Module.translateToString
    }

    case class hosted(name: String = "foo") {
      def fromExpr: Expr => HostedModule = exprToPhrase andThen fromPhrase

      def fromPhrase: Phrase => HostedModule =
        partialHostCompiler(name) <<>>:
          (   hostFunDefToFunction()
            x map(sizedKernelDefToKernel) )
    }

    private def hostFunDefToFunction(gen: shine.OpenCL.HostCodeGenerator =
                                        shine.OpenCL.HostCodeGenerator()
                                    ): HostFunDef => CModule =
      _.translateToModule(gen)

    private def partialHostCompiler(hostFunName: String): PartialCompiler[
      Phrase, HostedModule,
      (HostFunDef, Seq[SizedKernelDef]),
      (CModule, Seq[KernelModule])] =
        PartialCompiler.functor(
          OpenCL.SeparateHostAndKernelCode.separate(hostFunName),
          (OpenCL.Module.apply _).tupled )
  }
}
