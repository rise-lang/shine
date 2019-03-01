package idealised.SurfaceLanguage.Primitives


import idealised.DPIA.Phrases.Phrase
import idealised.DPIA.Types.ExpType
import idealised.SurfaceLanguage.DSL.DataExpr
import idealised.SurfaceLanguage.Types.TypeInference.SubstitutionMap
import idealised.SurfaceLanguage._
import idealised.{DPIA, SurfaceLanguage}
import idealised.SurfaceLanguage.Types._

case class WithIndex(array: DataExpr, override val t: Option[DataType]) extends PrimitiveExpr {
  override def visitAndRebuild(f: VisitAndRebuild.Visitor): WithIndex =
    WithIndex(VisitAndRebuild(array, f), t)

  override def inferType(subs: SubstitutionMap): DataExpr = {
    import TypeInference._
    TypeInference(array, subs) |> (array => array.t match {
      case Some(ArrayType(n, elemT)) => WithIndex(array, Some(DepArrayType(n, _ => elemT)))
      case x => error(expr = s"WithIndex($array)", found = s"`$x'", expected = "n.dt")
    })
  }

  override def convertToPhrase: Phrase[ExpType] = {
    array.t match {
      case Some(ArrayType(n, dt)) =>
        DPIA.FunctionalPrimitives.WithIndex(n, dt, array.toPhrase[DPIA.Types.ExpType])
      case _ => throw new Exception("")
    }
  }
}