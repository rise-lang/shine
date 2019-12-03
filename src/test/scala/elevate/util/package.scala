package elevate

import elevate.lift.strategies.normalForm.BENF
import _root_.lift.core.primitives._
import _root_.lift.core._
import _root_.lift.core.types._
import _root_.lift.core.TypedDSL._

package object util {
  def betaEtaEquals(a: Expr, b: Expr): Boolean = BENF(a).get == BENF(b).get

  // notation
  val tileSize = 4

  def T: TDSL[Transpose] = transpose
  def S: TDSL[DepApp[NatKind]] = split(tileSize) //slide(3)(1)
  def J: TDSL[Join] = join
  def *[T <: Expr](x: TDSL[T]): TDSL[App] = map(x)
  def **[T <: Expr](x: TDSL[T]): TDSL[App] = map(map(x))
  def ***[T <: Expr](x: TDSL[T]): TDSL[App] = map(map(map(x)))
  def ****[T <: Expr](x: TDSL[T]): TDSL[App] = map(map(map(map(x))))
  def *****[T <: Expr](x: TDSL[T]): TDSL[App] = map(map(map(map(map(x)))))
  def ******[T <: Expr](x: TDSL[T]): TDSL[App] = map(map(map(map(map(map(x))))))

  def λ[T <: Expr](f: TDSL[Identifier] => TDSL[T]): TDSL[Lambda] = fun(f)

  // map in LCNF
  def *![T <: Expr](x: TDSL[T]): TDSL[App] = {
    val i = identifier(freshName("e"))
    map(lambda(i, app(x, i)))
  }

  def **![T <: Expr](x: TDSL[T]): TDSL[App] = {
    val i = identifier(freshName("e"))
    map(lambda(i, app(*!(x), i)))
  }

  def ***![T <: Expr](x: TDSL[T]): TDSL[App] = {
    val i = identifier(freshName("e"))
    map(lambda(i, app(**!(x), i)))
  }

  def ****![T <: Expr](x: TDSL[T]): TDSL[App] = {
    val i = identifier(freshName("e"))
    map(lambda(i, app(***!(x), i)))
  }
}
