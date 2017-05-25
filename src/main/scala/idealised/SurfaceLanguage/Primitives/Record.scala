package idealised.SurfaceLanguage.Primitives

import idealised.DPIA.Phrases.Primitive
import idealised.DPIA.Types.TypeInference.SubstitutionMap
import idealised.DPIA.Types.{ExpType, TypeInference}
import idealised.SurfaceLanguage.DSL.DataExpr
import idealised.SurfaceLanguage.PrimitiveExpr
import idealised.{DPIA, SurfaceLanguage}

final case class Record(fst: DataExpr, snd: DataExpr) extends PrimitiveExpr {

  override def inferTypes(subs: SubstitutionMap): Primitive[ExpType] = {
    val fst_ = TypeInference(fst, subs)
    val snd_ = TypeInference(snd, subs)
    DPIA.FunctionalPrimitives.Record(fst_.t.dataType, snd_.t.dataType, fst_, snd_)
  }

  override def visitAndRebuild(f: SurfaceLanguage.VisitAndRebuild.Visitor): DataExpr = {
    Record(SurfaceLanguage.VisitAndRebuild(fst, f), SurfaceLanguage.VisitAndRebuild(snd, f))
  }

}
