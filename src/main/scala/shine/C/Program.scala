package shine.C

import shine.DPIA.Phrases.Identifier
import shine.DPIA.Types.{AccType, ExpType}
import shine._

case class Program(decls: Seq[C.AST.Decl],
                   function: C.AST.FunDecl,
                   outputParam: Identifier[AccType],
                   inputParams: Seq[Identifier[ExpType]]) {

  def code: String =
    "#include <stdint.h>\n" +
    decls.map(C.AST.Printer(_)).mkString("\n") +
    "\n\n" +
    C.AST.Printer(function)
}

