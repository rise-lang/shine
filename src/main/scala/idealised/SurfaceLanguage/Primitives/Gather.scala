package idealised.SurfaceLanguage.Primitives

import idealised.DPIA._
import idealised.DPIA.Types.{TypeInference, _}
import idealised.DPIA.Phrases._
import idealised.{DPIA, SurfaceLanguage}
import idealised.SurfaceLanguage.DSL.DataExpr
import idealised.SurfaceLanguage.{Expr, PrimitiveExpr}

import scala.language.postfixOps
import scala.language.reflectiveCalls

final case class Gather(idxF: Expr[ExpType -> ExpType],
                        array: DataExpr) extends PrimitiveExpr {

  override def inferTypes(subs: TypeInference.SubstitutionMap): Primitive[ExpType] = {
    import TypeInference._

    val array_ = TypeInference(array, subs)
    array_.t match {
      case ExpType(ArrayType(n_, dt_)) =>
        val idxF_ = TypeInference(idxF, subs)
        idxF_.t match {
          case FunctionType(ExpType(IndexType(m: NatIdentifier)), _) =>
            DPIA.FunctionalPrimitives.Gather(n_, dt_, idxF_ `[` n_ `/` m `]`, array_)
          case FunctionType(ExpType(IndexType(m1: Nat)), ExpType(IndexType(m2: Nat)))
            if n_ == m1 && n_ == m2 =>
            DPIA.FunctionalPrimitives.Gather(n_, dt_, idxF_, array_)
          case x => error(this.toString, s"`${x.toString}'", "exp[idx(n)] -> exp[idx(n)]")
        }
      case x => error(this.toString, s"`${x.toString}'", "exp[n.dt]")
    }
  }

  override def visitAndRebuild(f: SurfaceLanguage.VisitAndRebuild.Visitor): DataExpr = {
    Gather(SurfaceLanguage.VisitAndRebuild(idxF, f), SurfaceLanguage.VisitAndRebuild(array, f))
  }
}
