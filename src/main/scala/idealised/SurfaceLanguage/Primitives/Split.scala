package idealised.SurfaceLanguage.Primitives

import idealised.DPIA.Phrases.Primitive
import idealised.DPIA.Types.{TypeInference, _}
import idealised.DPIA._
import idealised.SurfaceLanguage.DSL.DataExpr
import idealised.SurfaceLanguage.PrimitiveExpr
import idealised.{DPIA, SurfaceLanguage}

final case class Split(n: Nat, array: DataExpr) extends PrimitiveExpr {

  override def inferTypes(subs: TypeInference.SubstitutionMap): Primitive[ExpType] = {
    import TypeInference._
    val array_ = TypeInference(array, subs)
    array_.t match {
      case ExpType(ArrayType(mn_, dt_)) =>
        DPIA.FunctionalPrimitives.Split(n, mn_ /^ n, dt_, array_)
      case x => error(expr = s"Split($n, $array_)",
        found = s"`${x.toString}'", expected = "exp[n.dt]")
    }
  }

  override def visitAndRebuild(f: SurfaceLanguage.VisitAndRebuild.Visitor): DataExpr = {
    Split(f(n), SurfaceLanguage.VisitAndRebuild(array, f))
  }
}
