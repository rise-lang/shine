package shine.C

import shine.C.primitives.imperative.CFunctionDefinition
import shine.C

case class TranslationUnit(decls: Seq[C.AST.Decl],
                           functions: Seq[C.Function]) {
  def compose(other: TranslationUnit): TranslationUnit =
    TranslationUnit((decls ++ other.decls).distinct, functions ++ other.functions)
}

object TranslationUnit {
  def compose(tus: Seq[TranslationUnit]): TranslationUnit = tus.reduce(_ compose _)

  def translateToString(tu: TranslationUnit): String =
      s"""
         |#include <stdint.h>
         |${tu.decls.map(C.AST.Printer(_)).mkString("\n")}
         |
         |${tu.functions.map(f => C.AST.Printer(f.code)).mkString("\n")}
         |""".stripMargin

  def fromCFunDef(gen: C.CodeGenerator)(cFunDef: CFunctionDefinition): TranslationUnit =
    cFunDef.translateToTranslationUnit(gen)
}
