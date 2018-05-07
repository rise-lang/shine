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
    TypeInference(array, subs) |> (array =>
      array.t match {
        case Some(ArrayType(m, dt)) =>
          f match {
            case NatDependentLambdaExpr(l, body) =>
              setParamAndInferType(body, ArrayType(l, dt), subs) |> (body =>
                NatDependentLambdaExpr(l, body) |> (f =>
                  f.t match {
                    case Some(NatDependentFunctionType(_,
                                FunctionType(ArrayType(l_, dt1), ArrayType(l_n, dt2)))) =>
                      if (l == l_ && dt1 == dt && dt2 == dt) {
                        val n = l_n match {
                          case Prod(l__ :: Pow(n1_, Cst(-1)) :: Nil) if l__.equals(l) => n1_
                          case _ => error(this.toString, l_n.toString, "l / n")
                        }
                        Iterate(k, f, array, Some(ArrayType(m /^ n.pow(k), dt)))
                      } else {
                        error(expr = s"Iterate($k, $f, $array)",
                          msg = s"expected $l == $l_ && $dt1 == $dt && $dt2 == $dt")
                      }
                    case ft => error(expr = s"Iterate($k, $f, $array)",
                      found = s"`${ft.toString}'", expected = "(x : Nat) -> (n.dt1 -> m.dt2)")
                  }))
            case _ => error(expr = s"Iterate($k, $f, $array)",
              found = s"`${f.toString}'", expected = NatDependentLambdaExpr.toString)
          }
        case t_ => error(expr = s"Iterate($k, $f, $array)",
          found = s"`${t_.toString}'", expected = "n.dt")
      })
  }

  override def visitAndRebuild(fun: VisitAndRebuild.Visitor): DataExpr = {
    Iterate(fun(k),
      VisitAndRebuild(f, fun),
      VisitAndRebuild(array, fun),
      t.map(fun(_)))
  }

}
