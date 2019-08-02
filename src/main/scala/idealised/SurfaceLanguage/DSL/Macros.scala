package idealised.SurfaceLanguage.DSL

import idealised.SurfaceLanguage.IdentifierExpr
import lift.arithmetic.{ArithExpr, ArithExprFunctionCall, SimplifiedExpr}


object Macros {

  class GetLength(val x: IdentifierExpr) extends ArithExprFunctionCall(s"getLength($x)") {
    override def visitAndRebuild(f: ArithExpr => ArithExpr): ArithExpr = this

    override def exposedArgs: Seq[ArithExpr] = Seq()

    override def substituteExposedArgs(subMap: Map[ArithExpr, SimplifiedExpr]): ArithExprFunctionCall = this
  }

  object GetLength {
    def apply(x: IdentifierExpr): GetLength = new GetLength(x)

    def unapply(arg: GetLength): Option[IdentifierExpr] = Some(arg.x)
  }
}

