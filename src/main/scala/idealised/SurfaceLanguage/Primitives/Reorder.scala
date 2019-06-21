package idealised.SurfaceLanguage.Primitives

import idealised.SurfaceLanguage.Types.{TypeInference, _}
import idealised.SurfaceLanguage._

import scala.language.{postfixOps, reflectiveCalls}

final case class Reorder(idxF: Expr,
                         idxFinv: Expr,
                         array: Expr,
                         override val t: Option[DataType])
  extends PrimitiveExpr
{
  override def inferType(subs: TypeInference.SubstitutionMap): Expr = {
    import TypeInference._

    TypeInference(array, subs) |> (array =>
      array.t match {
        case Some(arrayT@ArrayType(n, _)) =>
          def idxFcheck(f: Expr) =
            f.t match {
              case Some(FunctionType(IndexType(m: NatIdentifier), _)) =>
                Type.substitute(n, `for` = m, in = f)
              case Some(FunctionType(IndexType(m1), IndexType(m2))) =>
                if (n == m1 && n == m2) {
                  f
                } else {
                  error(expr = s"Reorder($idxF, $idxFinv, $array)",
                    found = s"`$n', `$m1' and `$m2'", expected = "them to match")
                }
              case x => error(expr = s"Reorder($idxF, $idxFinv, $array)",
                found = s"`${x.toString}'", expected = "idx(_) -> idx(_)")
            }

          TypeInference(idxF, subs) |> (idxF =>
            TypeInference(idxFinv, subs) |> (idxFinv =>
              Reorder(idxFcheck(idxF), idxFcheck(idxFinv), array, Some(arrayT))))

        case x => error(expr = s"Reorder($idxF, $idxFinv, $array)",
          found = s"`${x.toString}'", expected = "n.dt")
      })
  }

  override def children: Seq[Any] = Seq(idxF, idxFinv, array, t)

  override def rebuild: Seq[Any] => Expr = {
    case Seq(idxF: Expr, idxFinv: Expr, array: Expr, t: Option[DataType]@unchecked) =>
      Reorder(idxF, idxFinv, array, t)
  }
}
