package idealised.SurfaceLanguage.DSL

import idealised.SurfaceLanguage.IdentifierExpr
import lift.arithmetic.{ArithExpr, ArithExprFunction}


object Macros {

  class GetLength(val x: IdentifierExpr) extends ArithExprFunction(s"getLength($x)") {
    override def visitAndRebuild(f: ArithExpr => ArithExpr): ArithExpr = this
  }

  object GetLength {
    def apply(x: IdentifierExpr): GetLength = new GetLength(x)

    def unapply(arg: GetLength): Option[IdentifierExpr] = Some(arg.x)
  }

}