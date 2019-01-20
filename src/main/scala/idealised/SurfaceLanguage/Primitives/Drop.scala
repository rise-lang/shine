package idealised.SurfaceLanguage.Primitives

import idealised.SurfaceLanguage.DSL.DataExpr
import idealised.SurfaceLanguage.Types._
import idealised.SurfaceLanguage._
import idealised.{DPIA, SurfaceLanguage}

final case class Drop(n: Nat, array: DataExpr,
                       override val t: Option[DataType])
  extends PrimitiveExpr
{


  override def convertToPhrase: DPIA.Phrases.Phrase[DPIA.Types.ExpType] = {
    array.t match {
      case Some(ArrayType(m, dt)) =>
        DPIA.FunctionalPrimitives.Drop(n, m, dt, array.toPhrase[DPIA.Types.ExpType])
      case _ => throw new Exception("")
    }
  }

  override def inferType(subs: TypeInference.SubstitutionMap): Drop = {
    import TypeInference._
    TypeInference(array, subs) |> (array =>
      array.t match {
        case Some(ArrayType(m, dt)) => Drop(n, array, Some(ArrayType(m - n, dt)))
          //TODO: Check that n < m
        case x => error(expr = s"Drop($n, $array)", found = s"`${x.toString}'", expected = "n.dt")
      })
  }

  override def visitAndRebuild(f: SurfaceLanguage.VisitAndRebuild.Visitor): DataExpr = {
    Drop(f(n), SurfaceLanguage.VisitAndRebuild(array, f), t.map(f(_)))
  }
}
