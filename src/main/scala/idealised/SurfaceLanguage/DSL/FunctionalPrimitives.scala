package idealised.SurfaceLanguage.DSL

import idealised.SurfaceLanguage._
import idealised.SurfaceLanguage.Expr
import idealised.SurfaceLanguage.Primitives._
import idealised.SurfaceLanguage.Types._
import lift.arithmetic.NamedVar


object map {
  def apply(f: Expr[DataType -> DataType]): Expr[DataType -> DataType] = λ(x => map(f, x))

  def apply(f: Expr[DataType -> DataType], x: DataExpr): Map = Map(f, x)
}

object zip {
  def apply(lhs: DataExpr, rhs: DataExpr): Zip = Zip(lhs, rhs)
}

object split {
  def apply(n: Nat): Expr[DataType -> DataType] =
    λ(array => split(n, array))

  def apply(n: Nat, array: DataExpr): Split = Split(n, array)
}

object join {
  def apply(): Expr[DataType -> DataType] = λ(array => join(array))

  def apply(array: DataExpr): Join = Join(array)
}

object reduce {
  def apply(f: Expr[DataType -> (DataType -> DataType)]): Expr[DataType -> (DataType -> DataType)] =
    λ((init, array) => reduce(f, init, array))

  def apply(f: Expr[DataType -> (DataType -> DataType)],
            init: DataExpr): Expr[DataType -> DataType] =
    λ(array => reduce(f, init, array))

  def apply(f: Expr[DataType -> (DataType -> DataType)],
            init: DataExpr,
            array: DataExpr): Reduce =
    Reduce(f, init, array)
}

object iterate {
  def apply(k: Nat, f: Expr[`(nat)->`[DataType -> DataType]]): Expr[DataType -> DataType] =
    λ(array => iterate(k, f, array))

  def apply(k: Nat,
            f: Expr[`(nat)->`[DataType -> DataType]],
            array: DataExpr): Iterate =
    Iterate(k, f, array)
}

object gather {
  def apply(idxF: Expr[`(nat)->`[DataType ->DataType]]): Expr[DataType -> DataType] = {
//    val x: Expr[->[DataType, DataType]] = idxF(NamedVar(newName()))
    λ(array => Gather(idxF, array))
  }
}

object tuple {
  def apply(fst: DataExpr, snd: DataExpr): Tuple = Tuple(fst, snd)
}
