package idealised.OpenMP

import idealised.C.AST.FunDecl
import idealised.DPIA.Phrases.Identifier
import idealised.DPIA.Types.{AccType, ExpType}

case class Program(function: FunDecl,
                   outputParam: Identifier[AccType],
                   inputParams: Seq[Identifier[ExpType]]) {

  def code: String = idealised.C.AST.Printer(function)

}
