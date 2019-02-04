package idealised.SurfaceLanguage.Primitives

import idealised.SurfaceLanguage.DSL.DataExpr
import idealised.SurfaceLanguage._
import idealised.{DPIA, SurfaceLanguage}
import idealised.SurfaceLanguage.Types._

final case class Pad(l:Nat, r:Nat, padExpr:DataExpr, array: DataExpr,
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
          //TODO: Check that n < m
          case x => error(expr = s"Pad($array)", found = s"`${x.toString}'", expected = "n.dt")
        })
      )
  }

  override def visitAndRebuild(f: SurfaceLanguage.VisitAndRebuild.Visitor): DataExpr = {
    Pad(f(l), f(r), SurfaceLanguage.VisitAndRebuild(padExpr, f), SurfaceLanguage.VisitAndRebuild(array, f), t.map(f(_)))
  }
}
