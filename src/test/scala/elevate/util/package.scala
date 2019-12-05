package elevate

import elevate.rise._
import elevate.rise.strategies.normalForm.BENF
import _root_.lift.core.primitives._
import _root_.lift.core._
import _root_.lift.core.types._
import _root_.lift.core.TypedDSL._

package object util {

  // Rise-related utils

  def betaEtaEquals(a: Rise, b: Rise): Boolean = BENF(a).get == BENF(b).get

  val tileSize = 4

  // notation
  def T: TDSL[Transpose] = transpose
  def S: TDSL[DepApp[NatKind]] = split(tileSize) //slide(3)(1)
  def J: TDSL[Join] = join
  def *[T <: Rise](x: TDSL[T]): TDSL[App] = map(x)
  def **[T <: Rise](x: TDSL[T]): TDSL[App] = map(map(x))
  def ***[T <: Rise](x: TDSL[T]): TDSL[App] = map(map(map(x)))
  def ****[T <: Rise](x: TDSL[T]): TDSL[App] = map(map(map(map(x))))
  def *****[T <: Rise](x: TDSL[T]): TDSL[App] = map(map(map(map(map(x)))))
  def ******[T <: Rise](x: TDSL[T]): TDSL[App] = map(map(map(map(map(map(x))))))

  def λ[T <: Rise](f: TDSL[Identifier] => TDSL[T]): TDSL[Lambda] = fun(f)

  // map in LCNF
  def *![T <: Rise](x: TDSL[T]): TDSL[App] = {
    val i = identifier(freshName("e"))
    map(lambda(i, app(x, i)))
  }

  def **![T <: Rise](x: TDSL[T]): TDSL[App] = {
    val i = identifier(freshName("e"))
    map(lambda(i, app(*!(x), i)))
  }

  def ***![T <: Rise](x: TDSL[T]): TDSL[App] = {
    val i = identifier(freshName("e"))
    map(lambda(i, app(**!(x), i)))
  }

  def ****![T <: Rise](x: TDSL[T]): TDSL[App] = {
    val i = identifier(freshName("e"))
    map(lambda(i, app(***!(x), i)))
  }

  def testMultiple(list: List[Rise], gold: Rise) = {
    assert(list.forall(betaEtaEquals(_, gold)))
  }
}
