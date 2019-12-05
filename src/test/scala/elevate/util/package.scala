package elevate

import _root_.lift.core.DSL._
import _root_.lift.core._
import elevate.rise.strategies.normalForm.BENF

package object util {

  // Rise-related utils

  def betaEtaEquals(a: Expr, b: Expr): Boolean = BENF(a).get == BENF(b).get

  val tileSize = 4

  // notation
  def T: Expr = transpose
  def S: Expr = split(tileSize) //slide(3)(1)
  def J: Expr = join
  def *(x: Expr): Expr = map(x)
  def **(x: Expr): Expr = map(map(x))
  def ***(x: Expr): Expr = map(map(map(x)))
  def ****(x: Expr): Expr = map(map(map(map(x))))
  def *****(x: Expr): Expr = map(map(map(map(map(x)))))
  def ******(x: Expr): Expr = map(map(map(map(map(map(x))))))

  def λ(f: Identifier => Expr): Expr = fun(f)

  // map in LCNF
  def *!(x: Expr): Expr = {
    val i = identifier(freshName("e"))
    map(lambda(i, app(x, i)))
  }

  def **!(x: Expr): Expr = {
    val i = identifier(freshName("e"))
    map(lambda(i, app(*!(x), i)))
  }

  def ***!(x: Expr): Expr = {
    val i = identifier(freshName("e"))
    map(lambda(i, app(**!(x), i)))
  }

  def ****!(x: Expr): Expr = {
    val i = identifier(freshName("e"))
    map(lambda(i, app(***!(x), i)))
  }

  def testMultiple(list: List[Expr], gold: Expr) = {
    assert(list.forall(betaEtaEquals(_, gold)))
  }
}
