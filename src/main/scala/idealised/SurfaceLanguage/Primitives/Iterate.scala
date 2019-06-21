package idealised.SurfaceLanguage.Primitives

import idealised.SurfaceLanguage.Types.TypeInference.SubstitutionMap
import idealised.SurfaceLanguage.Types._
import idealised.SurfaceLanguage._
import lift.arithmetic._

final case class Iterate(k: Nat,
                         f: Expr,
                         array: Expr,
                         override val t: Option[DataType])
  extends PrimitiveExpr
{

  override def inferType(subs: SubstitutionMap): Iterate = {
    import TypeInference._
    TypeInference(array, subs) |> (array =>
      array.t match {
        case Some(ArrayType(m, dt)) =>
          f match {
            case NatDependentLambdaExpr(l, body, _) =>
              setParamAndInferType(body, ArrayType(l, dt), subs) |> (body =>
                natDepLambdaWithType(l, body) |> (f =>
                  f.t match {
                    case Some(DependentFunctionType(_: NatIdentifier,
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

  override def children: Seq[Any] = Seq(k, f, array, t)

  override def rebuild: Seq[Any] => Expr = {
    case Seq(k: Nat, f: Expr, array: Expr, t: Option[DataType]@unchecked) =>
      Iterate(k, f, array, t)
  }
}
