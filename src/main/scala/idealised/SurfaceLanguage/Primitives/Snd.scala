package idealised.SurfaceLanguage.Primitives

import idealised.DPIA.Phrases.Primitive
import idealised.DPIA.Types.{TypeInference, _}
import idealised.SurfaceLanguage.DSL.DataExpr
import idealised.SurfaceLanguage.PrimitiveExpr
import idealised.{DPIA, SurfaceLanguage}

final case class Snd(record: DataExpr) extends PrimitiveExpr {

  override def inferTypes(subs: TypeInference.SubstitutionMap): Primitive[ExpType] = {
    import idealised.DPIA.Types.TypeInference._
    val record_ = TypeInference(record, subs)
    record_.t match {
      case ExpType(RecordType(dt1_, dt2_)) =>
        DPIA.FunctionalPrimitives.Snd(dt1_, dt2_, record_)

      case x => error(expr = s"Fst($record_)",
        found = s"`${x.toString}'", expected = "exp[dt1 x dt2]")
    }
  }

  override def visitAndRebuild(f: SurfaceLanguage.VisitAndRebuild.Visitor): DataExpr = {
    Snd(SurfaceLanguage.VisitAndRebuild(record, f))
  }

  override def toString: String = s"$record._2"

}
