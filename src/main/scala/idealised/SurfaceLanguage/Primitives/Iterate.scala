package idealised.SurfaceLanguage.Primitives

import idealised.DPIA
import idealised.SurfaceLanguage.DSL.DataExpr
import idealised.SurfaceLanguage.Types._
import idealised.SurfaceLanguage._
import idealised.SurfaceLanguage.Types.TypeInference.SubstitutionMap
import lift.arithmetic._

final case class Iterate(k: Nat,
                         f: Expr[`(nat)->`[DataType -> DataType]],
                         array: DataExpr,
                         override val t: Option[DataType] = None)
  extends PrimitiveExpr
{

  override def convertToPhrase: DPIA.Phrases.Phrase[DPIA.Types.ExpType] = {
    (f.t, array.t) match {
      case (Some(NatDependentFunctionType(_,
        FunctionType(ArrayType(l, dt1), ArrayType(l_n, dt2)))), Some(ArrayType(m, dt)))
          if dt1 == dt && dt2 == dt =>
            val n = l_n match {
              case Prod(l__ :: Pow(n1_, Cst(-1)) :: Nil) if l__.equals(l) => n1_
              case _ => throw new Exception("")
            }
            DPIA.FunctionalPrimitives.Iterate(n, m, k, dt,
              f.toPhrase[DPIA.Types.NatDependentFunctionType[DPIA.Types.FunctionType[DPIA.Types.ExpType, DPIA.Types.ExpType]]],
//              .asInstanceOf[DPIA.Phrases.Phrase[DPIA.Types.NatDependentFunctionType[
//              DPIA.Types.FunctionType[DPIA.Types.ExpType, DPIA.Types.ExpType]]]]

              array.toPhrase[DPIA.Types.ExpType]
            )
      case _ => throw new Exception("")
    }
  }

  override def inferType(subs: SubstitutionMap): Iterate = {
    import TypeInference._
    val array_ = TypeInference(array, subs)
    array_.t match {
      case Some(ArrayType(m, dt)) =>
        f match {
          case NatDependentLambdaExpr(l, body: Expr[DataType -> DataType]) =>
            val b = setParamAndInferType(body, ArrayType(l, dt), subs)
            val f_ = NatDependentLambdaExpr(l, b)
            f_.t match {
              case Some(NatDependentFunctionType(_,
                FunctionType(ArrayType(l_, dt1), ArrayType(l_n, dt2)))) =>
                if (l == l_ && dt1 == dt && dt2 == dt) {
                  val n = l_n match {
                    case Prod(l__ :: Pow(n1_, Cst(-1)) :: Nil) if l__.equals(l) => n1_
                    case _ => error(this.toString, l_n.toString, "l / n")
                  }
                  Iterate(k, f_, array_, Some(ArrayType(m /^ n.pow(k), dt)))
                } else {
                  error(expr = s"Iterate($k, $f_, $array_)",
                    msg = s"expected $l == $l_ && $dt1 == $dt && $dt2 == $dt")
                }
              case ft => error(expr = s"Iterate($k, $f_, $array_)",
                found = s"`${ft.toString}'", expected = "(x : Nat) -> (n.dt1 -> m.dt2)")
            }
          case _ => error(expr = s"Iterate($k, $f, $array_)",
            found = s"`${f.toString}'", expected = NatDependentLambdaExpr.toString)
        }
      case t_ => error(expr = s"Iterate($k, $f, $array_)",
        found = s"`${t_.toString}'", expected = "n.dt")
    }
  }

//  override def inferTypes(subs: TypeInference.SubstitutionMap): Primitive[ExpType] = {
//    import TypeInference._
//    val array_ = TypeInference(array, subs)
//    array_.t match {
//      case ExpType(ArrayType(m_, dt_)) =>
//        f match {
//          case NatDependentLambdaExpr(l, body: Expr[ExpType -> ExpType]) =>
//            val b = TypeInference.setParamAndInferType(body, exp"[$l.$dt_]", subs)
//            val f_ = NatDependentLambda(l, b) //f.copy(body=b)
//            f_.t match {
//              case NatDependentFunctionType(_,
//              FunctionType(ExpType(ArrayType(l_, dt1_)),
//                           ExpType(ArrayType(l_n, dt2_)))) =>
//                if (l == l_ && dt1_ == dt_ && dt2_ == dt_) {
//
//                  val n_ = l_n match {
//                    case Prod(l__ :: Pow(n1_, Cst(-1)) :: Nil) if l__.equals(l) => n1_
//                    case _ => error(this.toString, l_n.toString, "l / n")
//                  }
//
//                  DPIA.FunctionalPrimitives.Iterate(n_, m_, k, dt_, f_, array_)
//                } else {
//                  error(expr = s"Iterate($k, $f_, $array_)",
//                    msg = s"expected $l == $l_ && $dt1_ == $dt_ && $dt2_ == $dt_")
//                }
//              case ft => error(expr = s"Iterate($k, $f_, $array_)",
//                found = s"`${ft.toString}'", expected = "(x : Nat) -> (exp[n.dt1] -> exp[m.dt2])")
//            }
//          case _ => error(expr = s"Iterate($k, $f, $array_)",
//            found = s"`${f.toString}'", expected = NatDependentLambdaExpr.toString)
//        }
//      case t_ => error(expr = s"Iterate($k, $f, $array_)",
//        found = s"`${t_.toString}'", expected = "exp[n.dt]")
//    }
//  }

  override def visitAndRebuild(fun: VisitAndRebuild.Visitor): DataExpr = {
    Iterate(fun(k),
      VisitAndRebuild(f, fun),
      VisitAndRebuild(array, fun),
      t.map(fun(_)))
  }

}
