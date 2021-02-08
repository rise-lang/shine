package shine.C

import shine.C.primitives.imperative.CFunctionDefinition
import shine.C

case class Module(decls: Seq[C.AST.Decl],
                  functions: Seq[C.Function]) {
  def compose(other: Module): Module =
    Module((decls ++ other.decls).distinct, functions ++ other.functions)
}

object Module {
  def compose(ms: Seq[Module]): Module = ms.reduce(_ compose _)

  def translateToString(m: Module): String =
      s"""
         |#include <stdint.h>
         |${m.decls.map(C.AST.Printer(_)).mkString("\n")}
         |
         |${m.functions.map(f => C.AST.Printer(f.code)).mkString("\n")}
         |""".stripMargin

  def fromCFunDef(gen: C.CodeGenerator)(cFunDef: CFunctionDefinition): Module =
    cFunDef.translateToModule(gen)
}
