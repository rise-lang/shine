package shine.GAP8

import shine.C

/**
  * This is a prototype of GAP8 module. Right now it consists of
  * a set of includes, declarations (structure for passing arguments to cluster), and
  * accelerator function (forked code)
  *
  * To be added:
  * 1. host (fiber controller) code
  * 2. code initially executed on core 0 after sending task to cluster
  *   (executed prior to forking on cluster)
  * 3. System kickoff code
  * 4. TBA
  * */
case class Module(includes: Seq[C.AST.IncludeDirective],
                  decls: Seq[C.AST.Decl],
                  functions: Seq[C.AST.Function]) {
  def compose(other: Module): Module =
    Module(
      (includes ++ other.includes).distinct,
      (decls ++ other.decls).distinct,
      functions ++ other.functions)
}

object Module {
  def compose(ms: Seq[Module]): Module = ms.reduce(_ compose _)

  def translateToString(m: Module): String =
    s"""
       |${m.includes.map(_.toString).mkString("\n")}
       |${m.decls.map(C.AST.Printer(_)).mkString("\n")}
       |${m.functions.map(f => C.AST.Printer(f.code)).mkString("\n")}
       |""".stripMargin
}
