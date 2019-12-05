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
  def *(x: TDSL[Rise]): TDSL[App] = map(x)
  def **(x: TDSL[Rise]): TDSL[App] = map(map(x))
  def ***(x: TDSL[Rise]): TDSL[App] = map(map(map(x)))
  def ****(x: TDSL[Rise]): TDSL[App] = map(map(map(map(x))))
  def *****(x: TDSL[Rise]): TDSL[App] = map(map(map(map(map(x)))))
  def ******(x: TDSL[Rise]): TDSL[App] = map(map(map(map(map(map(x))))))

  def λ(f: TDSL[Identifier] => TDSL[Expr]): TDSL[Lambda] = fun(f)

  // map in LCNF
  def *!(x: TDSL[Rise]): TDSL[App] = {
    val i = identifier(freshName("e"))
    map(lambda(i, app(x, i)))
  }

  def **!(x: TDSL[Rise]): TDSL[App] = {
    val i = identifier(freshName("e"))
    map(lambda(i, app(*!(x), i)))
  }

  def ***!(x: TDSL[Rise]): TDSL[App] = {
    val i = identifier(freshName("e"))
    map(lambda(i, app(**!(x), i)))
  }

  def ****!(x: TDSL[Rise]): TDSL[App] = {
    val i = identifier(freshName("e"))
    map(lambda(i, app(***!(x), i)))
  }

  def testMultiple(list: List[Rise], gold: Rise): Unit = {
    assert(list.forall(betaEtaEquals(_, gold)))
  }
}
