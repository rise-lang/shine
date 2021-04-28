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

  object gap8 {
    import shine.OpenMP
    import shine.GAP8

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
      def fromExpr: Expr => GAP8.Module =
        functionFromExpr(name, OpenMP.CodeGenerator())

      def asStringFromExpr: Expr => String =
        functionAsStringFromExpr(name, OpenMP.CodeGenerator())

      private def funDefToFunction(name: String,
                                   gen: CCodeGenerator): Phrase => GAP8.Module =
        (FunDef(name, _)) andThen
          GAP8.Compilation.ModuleGenerator.funDefToModule(gen)

      private def functionFromExpr(name: String = "foo",
                                   gen: CCodeGenerator = CCodeGenerator()
                                  ): Expr => GAP8.Module =
        exprToPhrase andThen
          funDefToFunction(name, gen)

      private def functionAsStringFromExpr(name: String = "foo",
                                           gen: CCodeGenerator = CCodeGenerator()
                                          ): Expr => String =
        functionFromExpr(name, gen) andThen
          GAP8.Module.translateToString andThen
          run(SyntaxChecker(_))
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
          ((((x: FunDef) => x) x map(kernelDefToKernel())) andThen
          hostFunDefToHostPart)
    }

    private val hostFunDefToHostPart:
      ((FunDef, Seq[KernelModule])) => (CModule, Seq[KernelModule]) = { case (hm, kms) =>
      val gen = shine.OpenCL.Compilation.HostCodeGenerator(kms)
      (HostCodeModuleGenerator.funDefToModule(gen)(hm), kms)
    }

    private def partialHostCompiler(hostFunName: String): PartialCompiler[
      Phrase,   HostedModule,
      (FunDef,  Seq[KernelDef]),
      (CModule, Seq[KernelModule])] =
        PartialCompiler.functor(
          OpenCL.Compilation.SeparateHostAndKernelCode.separate(hostFunName),
          (OpenCL.Module.apply _).tupled)
  }

  object cuda {
    import opencl._

    type KernelModule = shine.cuda.KernelModule

    object kernel {
      def apply(name: String = "foo"): kernel =
        new kernel(None, name)

      def apply(localSize: LocalSize, globalSize: GlobalSize): kernel =
        new kernel(Some(LocalAndGlobalSize(localSize, globalSize)), "foo")

      def apply(localSize: LocalSize, globalSize: GlobalSize, name: String): kernel =
        new kernel(Some(LocalAndGlobalSize(localSize, globalSize)), name)

      def fromExpr: Expr => KernelModule =
        gen.cuda.kernel().fromExpr

      def fromPhrase: Phrase => KernelModule =
        gen.cuda.kernel().fromPhrase

      def asStringFromExpr: Expr => String =
        gen.cuda.kernel().asStringFromExpr

      def asStringFromPhrase: Phrase => String =
        gen.cuda.kernel().asStringFromPhrase

      def asString: KernelModule => String = {
        shine.cuda.KernelModule.translationToString
      }
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
          gen.cuda.kernel.asString

      def asStringFromPhrase: Phrase => String =
        fromPhrase andThen
          gen.cuda.kernel.asString
    }

    private def phraseToKernelDef(name: String): Phrase => KernelDef =
      KernelDef(name, _)

    private def kernelWithWgConfig: LocalAndGlobalSize => KernelDef => KernelDef = {
      case LocalAndGlobalSize((ls, gs)) =>_.withWgConfig(ls, gs)
    }

    private def kernelDefToKernel(): KernelDef => KernelModule =
      shine.cuda.Compilation.KernelModuleGenerator.funDefToModule(
        shine.cuda.Compilation.KernelCodeGenerator())

    type HostedModule = shine.cuda.Module

    object hosted {
      def fromExpr: Expr => HostedModule = gen.cuda.hosted().fromExpr
      def fromPhrase: Phrase => HostedModule = gen.cuda.hosted().fromPhrase
      def asString: HostedModule => String = shine.cuda.Module.translateToString
    }

    case class hosted(name: String = "foo") {
      def fromExpr: Expr => HostedModule = exprToPhrase andThen fromPhrase

      def fromPhrase: Phrase => HostedModule =
        partialHostCompiler(name) composeWith
          ((((x: FunDef) => x) x map(kernelDefToKernel())) andThen
          hostFunDefToHostPart)
    }

    private val hostFunDefToHostPart:
      ((FunDef, Seq[KernelModule])) => (CModule, Seq[KernelModule]) = { case (hm, kms) =>
      // FIXME: The OpenCL host code generator does not work with CUDA kernel modules
      //  We need to refactor the OpenCL and CUDA backends and generalize host code generation
      ???
    }

    private def partialHostCompiler(hostFunName: String): PartialCompiler[
      Phrase,   HostedModule,
      (FunDef,  Seq[KernelDef]),
      (CModule, Seq[KernelModule])] =
      PartialCompiler.functor(
        shine.OpenCL.Compilation.SeparateHostAndKernelCode.separate(hostFunName),
        (shine.cuda.Module.apply _).tupled)
  }
}
