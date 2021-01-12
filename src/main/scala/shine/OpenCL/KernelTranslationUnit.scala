package shine.OpenCL

import shine.{C, OpenCL}
import shine.OpenCL.primitives.imperative.OpenCLKernelDefinition

case class KernelTranslationUnit(decls: Seq[C.AST.Decl],
                                 kernels: Seq[OpenCL.Kernel]) {
  def compose(other: KernelTranslationUnit): KernelTranslationUnit =
    KernelTranslationUnit((decls ++ other.decls).distinct, kernels ++ other.kernels)
}

object KernelTranslationUnit {
  def compose(tus: Seq[KernelTranslationUnit]): KernelTranslationUnit = tus.reduce(_ compose _)

  def translationToString(tu: KernelTranslationUnit): String =
    s"""
       |${tu.decls.map(OpenCL.AST.Printer(_)).mkString("\n")}
       |
       |${tu.kernels.map(k => OpenCL.AST.Printer(k.code)).mkString("\n")}
       |""".stripMargin

  def fromKernelDef(wgConfig: Option[(LocalSize, GlobalSize)])
                   (kernelDef: OpenCLKernelDefinition): KernelTranslationUnit =
    kernelDef.translateToTranslationUnit(shine.OpenCL.CodeGenerator())(wgConfig)
}
