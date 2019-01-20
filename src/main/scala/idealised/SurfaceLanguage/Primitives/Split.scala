package idealised.SurfaceLanguage.Primitives

import idealised.SurfaceLanguage.DSL.DataExpr
import idealised.SurfaceLanguage._
import idealised.{DPIA, SurfaceLanguage}
import idealised.SurfaceLanguage.Types._

final case class Split(n: Nat, array: DataExpr,
                       override val t: Option[DataType])
  extends PrimitiveExpr
{


  override def convertToPhrase: DPIA.Phrases.Phrase[DPIA.Types.ExpType] = {
    array.t match {
      case Some(ArrayType(mn, dt)) =>
        DPIA.FunctionalPrimitives.Split(n, mn /^ n, dt, array.toPhrase[DPIA.Types.ExpType])
      case _ => throw new Exception("")
    }
  }

  override def inferType(subs: TypeInference.SubstitutionMap): Split = {
    import TypeInference._
    TypeInference(array, subs) |> (array =>
      array.t match {
        case Some(ArrayType(mn, dt)) => Split(n, array, Some(ArrayType(mn /^ n, ArrayType(n, dt))))
        case x => error(expr = s"Split($n, $array)", found = s"`${x.toString}'", expected = "n.dt")
      })
  }

  override def visitAndRebuild(f: SurfaceLanguage.VisitAndRebuild.Visitor): DataExpr = {
    Split(f(n), SurfaceLanguage.VisitAndRebuild(array, f), t.map(f(_)))
  }
}
