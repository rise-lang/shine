package shine.OpenCL

import shine.{C, OpenCL}
import shine.OpenCL.primitives.imperative.OpenCLKernelDefinition

case class KernelModule(decls: Seq[C.AST.Decl],
                        kernels: Seq[OpenCL.Kernel]) {
  def compose(other: KernelModule): KernelModule =
    KernelModule((decls ++ other.decls).distinct, kernels ++ other.kernels)
}

object KernelModule {
  def compose(ms: Seq[KernelModule]): KernelModule = ms.reduce(_ compose _)

  def translationToString(m: KernelModule): String =
    s"""
       |${m.decls.map(OpenCL.AST.Printer(_)).mkString("\n")}
       |
       |${m.kernels.map(k => OpenCL.AST.Printer(k.code)).mkString("\n")}
       |""".stripMargin

  def fromKernelDef(wgConfig: Option[(LocalSize, GlobalSize)])
                   (kernelDef: OpenCLKernelDefinition): KernelModule =
    kernelDef.translateToModule(shine.OpenCL.CodeGenerator())(wgConfig)
}
