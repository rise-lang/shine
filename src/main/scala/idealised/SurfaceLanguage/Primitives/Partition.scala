package idealised.SurfaceLanguage.Primitives

import idealised.DPIA.{Nat, NatNatTypeFunction}
import idealised.SurfaceLanguage.Types._
import idealised.SurfaceLanguage._
import lift.arithmetic.BigSum

final case class Partition(m: Nat, lenF: NatNatTypeFunction, array: Expr,
                           override val t: Option[DataType])
  extends PrimitiveExpr {

  override def inferType(subs: TypeInference.SubstitutionMap): Partition = {
    import TypeInference._
    val typed = TypeInference(array, subs) |> (array =>
      array.t match {
        case Some(ArrayType(n, dt)) =>
          (BigSum(from = 0, upTo = m - 1, `for` = lenF.x, lenF.body) == n,
            s"Total output size of partition not provably equal to input size!" +
              s"${BigSum(from = 0, upTo = m - 1, `for` = lenF.x, lenF.body)} != $n."
          )
          Partition(m, lenF, array,
            Some(DepArrayType(m, i => ArrayType(lenF(i), dt))))
        case x => error(expr = s"Partition($m, $lenF $array)", found = s"`${x.toString}'", expected = "n.dt")
      })
    typed
  }

  override def children: Seq[Any] = Seq(m, lenF, array, t)

  override def rebuild: Seq[Any] => Expr = {
    case Seq(m: Nat, lenF: NatNatTypeFunction, array: Expr, t: Option[DataType]@unchecked) =>
      Partition(m, lenF, array, t)
  }
}
