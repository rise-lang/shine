package shine.cuda

import shine.OpenCL.{GlobalSize, LocalSize}
import shine.{C, cuda}
import shine.cuda.primitives.imperative.CudaKernelDefinition

case class KernelModule(decls: Seq[C.AST.Decl],
                        kernels: Seq[cuda.Kernel]) {
  def compose(other: KernelModule): KernelModule =
    KernelModule((decls ++ other.decls).distinct, kernels ++ other.kernels)
}

object KernelModule {
  def compose(ms: Seq[KernelModule]): KernelModule = ms.reduce(_ compose _)

  def translationToString(m: KernelModule): String =
    s"""
       |${m.decls.map(shine.cuda.ast.Printer(_)).mkString("\n")}
       |
       |${m.kernels.map(k => shine.cuda.ast.Printer(k.code)).mkString("\n")}
       |""".stripMargin

  def fromKernelDef(wgConfig: Option[(LocalSize, GlobalSize)])
                   (kernelDef: CudaKernelDefinition): KernelModule =
    kernelDef.translateToModule(shine.cuda.CodeGenerator())(wgConfig)
}