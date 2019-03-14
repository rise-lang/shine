package idealised.SurfaceLanguage.Primitives

import idealised.SurfaceLanguage.Types._
import idealised.SurfaceLanguage._
import idealised.{DPIA, SurfaceLanguage}

final case class Pad(l:Nat, r:Nat, padExpr:Expr, array: Expr,
                      override val t: Option[DataType])
  extends PrimitiveExpr {


  override def convertToPhrase: DPIA.Phrases.Phrase[DPIA.Types.ExpType] = {
    array.t match {
      case Some(ArrayType(n, dt)) =>
        DPIA.FunctionalPrimitives.Pad(n, l, r, dt, padExpr.toPhrase[DPIA.Types.ExpType], array.toPhrase[DPIA.Types.ExpType])
      case _ => throw new Exception("")
    }
  }

  override def inferType(subs: TypeInference.SubstitutionMap): Pad = {
    import TypeInference._
    TypeInference(array, subs) |> (array =>
      TypeInference(padExpr, subs) |> (padExpr =>
        array.t match {
          case Some(ArrayType(n, dt)) => Pad(l, r, padExpr, array, Some(ArrayType(l + n + r, dt)))
          case x => error(expr = s"Pad($array)", found = s"`${x.toString}'", expected = "n.dt")
        })
      )
  }

  override def visitAndRebuild(f: SurfaceLanguage.VisitAndRebuild.Visitor): Expr = {
    Pad(f(l), f(r), SurfaceLanguage.VisitAndRebuild(padExpr, f), SurfaceLanguage.VisitAndRebuild(array, f), t.map(f(_)))
  }
}
