package shine.cuda

import shine.C
import shine.cuda.AST.Kernel

case class KernelModule(decls: Seq[C.AST.Decl],
                        kernels: Seq[Kernel]) {
  def compose(other: KernelModule): KernelModule =
    KernelModule((decls ++ other.decls).distinct, kernels ++ other.kernels)
}

object KernelModule {
  def compose(ms: Seq[KernelModule]): KernelModule = ms.reduce(_ compose _)

  def translationToString(m: KernelModule): String =
    s"""
       |${m.decls.map(shine.cuda.AST.Printer(_)).mkString("\n")}
       |
       |${m.kernels.map(k => shine.cuda.AST.Printer(k.code)).mkString("\n")}
       |""".stripMargin
}