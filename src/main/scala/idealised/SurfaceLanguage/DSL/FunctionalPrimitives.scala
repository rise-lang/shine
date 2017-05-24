package idealised.SurfaceLanguage.DSL

import idealised.DPIA._
import idealised.DPIA.Types.ExpType
import idealised.SurfaceLanguage.Primitives._
import idealised.SurfaceLanguage.Expr
import lift.arithmetic.NamedVar


object map {
  def apply(f: Expr[ExpType -> ExpType]): Expr[ExpType -> ExpType] = λ(x => map(f, x))

  def apply(f: Expr[ExpType -> ExpType], x: DataExpr): Map = Map(f, x)
}

object zip {
  def apply(lhs: DataExpr, rhs: DataExpr): Zip = Zip(lhs, rhs)
}

object split {
  def apply(n: Nat): Expr[ExpType -> ExpType] =
    λ(array => split(n, array))

  def apply(n: Nat, array: DataExpr): Split = Split(n, array)
}

object join {
  def apply(): Expr[ExpType -> ExpType] = λ(array => join(array))

  def apply(array: DataExpr): Join = Join(array)
}

object reduce {
  def apply(f: Expr[ExpType -> (ExpType -> ExpType)]): Expr[ExpType -> (ExpType -> ExpType)] =
    λ((init, array) => reduce(f, init, array))

  def apply(f: Expr[ExpType -> (ExpType -> ExpType)],
            init: DataExpr): Expr[ExpType -> ExpType] =
    λ(array => reduce(f, init, array))

  def apply(f: Expr[ExpType -> (ExpType -> ExpType)],
            init: DataExpr,
            array: DataExpr): Reduce =
    Reduce(f, init, array)
}

object iterate {
  def apply(k: Nat, f: Expr[`(nat)->`[ExpType -> ExpType]]): Expr[ExpType -> ExpType] =
    λ(array => iterate(k, f, array))

  def apply(k: Nat,
            f: Expr[`(nat)->`[ExpType -> ExpType]],
            array: DataExpr): Iterate =
    Iterate(k, f, array)
}

object gather {
  def apply(idxF: Expr[`(nat)->`[ExpType ->ExpType]]): Expr[ExpType -> ExpType] = {
    val x: Expr[->[ExpType, ExpType]] = idxF(NamedVar(newName()))
    λ(array => Gather(x, array))
  }
}

object record {
  def apply(fst: DataExpr, snd: DataExpr): Record = Record(fst, snd)
}
