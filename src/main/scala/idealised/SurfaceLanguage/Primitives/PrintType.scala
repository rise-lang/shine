package idealised.SurfaceLanguage.Primitives

import idealised.SurfaceLanguage.Types.TypeInference.error
import idealised.SurfaceLanguage.Types._
import idealised.SurfaceLanguage.{Expr, PrimitiveExpr, VisitAndRebuild}

final case class PrintType(input: Expr,
                           msg: String,
                           override val t: Option[DataType])
  extends PrimitiveExpr {

  override def inferType(subs: TypeInference.SubstitutionMap): PrintType = {
    TypeInference(input, subs) |> (input =>
    input.t match {
      case Some(dt: DataType) =>
        println(s"Type $msg: ${dt.toString}")
        PrintType(input, msg, Some(dt))
      case None =>
        println(s"Type $msg: NoType")
        PrintType(input, msg, None)
      case x => error(expr = s"PrintType($input, $msg)",
        found = s"`${x.toString}'", expected = "Datatype or None")
    })
  }

  override def visitAndRebuild(f: VisitAndRebuild.Visitor): Expr = {
    PrintType(VisitAndRebuild(input, f), msg, t.map(f(_)))
  }

  override def toString: String = ""
}
