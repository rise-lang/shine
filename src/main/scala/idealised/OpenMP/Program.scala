package idealised.OpenMP

import idealised.C.AST.{Decl, FunDecl}
import idealised.DPIA.Phrases.Identifier
import idealised.DPIA.Types.{AccType, ExpType}

case class Program(decls: Seq[Decl],
                   function: FunDecl,
                   outputParam: Identifier[AccType],
                   inputParams: Seq[Identifier[ExpType]]) {

  def code: String = decls.map(idealised.C.AST.Printer(_)).mkString("\n") + idealised.C.AST.Printer(function)

}
