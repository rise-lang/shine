package idealised.SurfaceLanguage.Primitives

import idealised.DPIA.Phrases._
import idealised.DPIA.Types.{TypeInference, _}
import idealised.DPIA._
import idealised.{DPIA, SurfaceLanguage}
import idealised.SurfaceLanguage.DSL.DataExpr
import idealised.SurfaceLanguage.{Expr, NatDependentLambdaExpr, PrimitiveExpr}
import lift.arithmetic._

final case class Iterate(k: Nat,
                         f: Expr[`(nat)->`[ExpType -> ExpType]],
                         array: DataExpr) extends PrimitiveExpr {

  override def inferTypes(subs: TypeInference.SubstitutionMap): Primitive[ExpType] = {
    import TypeInference._
    val array_ = TypeInference(array, subs)
    array_.t match {
      case ExpType(ArrayType(m_, dt_)) =>
        f match {
          case NatDependentLambdaExpr(l, body: Expr[ExpType -> ExpType]) =>
            val b = TypeInference.setParamAndInferType(body, exp"[$l.$dt_]", subs)
            val f_ = NatDependentLambda(l, b) //f.copy(body=b)
            f_.t match {
              case NatDependentFunctionType(_,
              FunctionType(ExpType(ArrayType(l_, dt1_)),
                           ExpType(ArrayType(l_n, dt2_)))) =>
                if (l == l_ && dt1_ == dt_ && dt2_ == dt_) {

                  val n_ = l_n match {
                    case Prod(l__ :: Pow(n1_, Cst(-1)) :: Nil) if l__.equals(l) => n1_
                    case _ => error(this, "???")//error(s"$l_n", s"${l_ / n}")
                  }

                  DPIA.FunctionalPrimitives.Iterate(n_, m_, k, dt_, f_, array_)
                } else {
                  error(this, s"expected $l == $l_ && $dt1_ == $dt_ && $dt2_ == $dt_")
                }
              case ft => error(this, ft.toString, "(x : Nat) -> (exp[n.dt1] -> exp[m.dt2])")
            }
          case _ => error(this, f.toString, NatDependentLambdaExpr.toString)
        }
      case t_ => error(this, t_.toString, "exp[n.dt]")
    }
  }

  override def visitAndRebuild(fun: SurfaceLanguage.VisitAndRebuild.Visitor): DataExpr = {
    Iterate(fun(k), SurfaceLanguage.VisitAndRebuild(f, fun), SurfaceLanguage.VisitAndRebuild(array, fun))
  }

}
