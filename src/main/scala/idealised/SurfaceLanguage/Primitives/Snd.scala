package idealised.SurfaceLanguage.Primitives

import idealised.utils._
import idealised.DPIA.Types.{TypeInference, _}
import idealised.DPIA.Phrases.Primitive
import idealised.{DPIA, SurfaceLanguage}
import idealised.SurfaceLanguage.DSL.DataExpr
import idealised.SurfaceLanguage.PrimitiveExpr

final case class Snd(record: DataExpr) extends PrimitiveExpr {

  override def inferTypes(subs: TypeInference.SubstitutionMap): Primitive[ExpType] = {
    import idealised.DPIA.Types.TypeInference._
    val record_ = TypeInference(record, subs)
    record_.t match {
      case ExpType(RecordType(dt1_, dt2_)) =>
        DPIA.FunctionalPrimitives.Snd(dt1_, dt2_, record_)

      case x => error(this.toString, s"`${x.toString}'", "exp[dt1 x dt2]")
    }
  }

  override def visitAndRebuild(f: SurfaceLanguage.VisitAndRebuild.Visitor): DataExpr = {
    Snd(SurfaceLanguage.VisitAndRebuild(record, f))
  }

}
