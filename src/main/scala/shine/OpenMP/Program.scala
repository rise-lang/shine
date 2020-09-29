package shine.OpenMP

import shine.C.AST.{Decl, FunDecl}
import shine.DPIA.Phrases.Identifier
import shine.DPIA.Types.{AccType, ExpType}

import scala.collection.immutable

case class Program(decls: immutable.Seq[Decl],
                   function: FunDecl,
                   outputParam: Identifier[AccType],
                   inputParams: immutable.Seq[Identifier[ExpType]]) {

  def code: String =
    "#include <stdint.h>\n" +
    decls.map(shine.C.AST.Printer(_)).mkString("\n") +
    "\n\n" +
    shine.C.AST.Printer(function)

}
