package shine.OpenMP

import shine.C.AST.{Decl, FunDecl}
import shine.DPIA.Phrases.Identifier
import shine.DPIA.Types.{AccType, ExpType}

case class Program(decls: Seq[Decl],
                   function: FunDecl,
                   outputParam: Identifier[AccType],
                   inputParams: Seq[Identifier[ExpType]]) {

  def code: String = decls.map(shine.C.AST.Printer(_)).mkString("\n") +
    "\n\n" +
    shine.C.AST.Printer(function)

}
