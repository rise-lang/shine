package idealised.C

import idealised._
import idealised.DPIA.Phrases.Identifier
import idealised.DPIA.Types.{AccType, ExpType}

case class Program(function: C.AST.FunDecl,
                   outputParam: Identifier[AccType],
                   inputParams: Seq[Identifier[ExpType]]) {

  def code: String = C.AST.Printer(function)

}
