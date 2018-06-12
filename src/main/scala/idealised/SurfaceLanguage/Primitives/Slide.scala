package idealised.SurfaceLanguage.Primitives

import idealised.SurfaceLanguage.DSL.DataExpr
import idealised.SurfaceLanguage._
import idealised.{DPIA, SurfaceLanguage}
import idealised.SurfaceLanguage.Types._

final case class Slide(s1: Nat, s2: Nat, array: DataExpr,
                       override val t: Option[DataType] = None)
  extends PrimitiveExpr
{


  override def convertToPhrase: DPIA.Phrases.Phrase[DPIA.Types.ExpType] = {
    array.t match {
      case Some(ArrayType(m, dt)) =>
        val n = (m - s1 + s2) /^ s2
        DPIA.FunctionalPrimitives.Slide(n, s1, s2, dt, array.toPhrase[DPIA.Types.ExpType])
      case _ => throw new Exception("")
    }
  }

  override def inferType(subs: TypeInference.SubstitutionMap): Slide = {
    import TypeInference._
    TypeInference(array, subs) |> (array =>
      array.t match {
        case Some(ArrayType(m, dt)) =>
          val n = (m - s1 + s2) /^ s2
          Slide(s1, s2, array, Some(ArrayType(n, ArrayType(s1, dt))))
        case x => error(expr = s"Slide($s1, $s2, $array)", found = s"`${x.toString}'", expected = "n.dt")
      })
  }

  override def visitAndRebuild(f: SurfaceLanguage.VisitAndRebuild.Visitor): DataExpr = {
    Slide(f(s1), f(s2), SurfaceLanguage.VisitAndRebuild(array, f), t.map(f(_)))
  }
}
