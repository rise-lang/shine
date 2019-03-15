package idealised.SurfaceLanguage.Primitives

import idealised.SurfaceLanguage.Types._
import idealised.SurfaceLanguage.{Expr, PrimitiveExpr, VisitAndRebuild}

final case class PrintType(input: Expr,
                           msg: String,
                           override val t: Option[DataType])
  extends PrimitiveExpr {

  override def inferType(subs: TypeInference.SubstitutionMap): PrintType = {
    TypeInference(input, subs) |> (input => {
      println(s"Type $msg: ${
        input.t match {
          case None => "NoType"
          case Some(dt) => dt.toString
        }
      }")
      PrintType(input, msg, input.t match {
        case None => None
        case Some(dt: DataType) => Some(dt)
      })
    })
  }

  override def visitAndRebuild(f: VisitAndRebuild.Visitor): Expr = {
    PrintType(VisitAndRebuild(input, f), msg, t.map(f(_)))
  }

  override def toString: String = ""
}
