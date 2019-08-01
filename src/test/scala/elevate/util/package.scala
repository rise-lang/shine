package elevate

//import elevate.lift.strategies.normalForm.BENF
import _root_.lift.core.primitives._
import _root_.lift.core._
import _root_.lift.core.DSL._

package object util {
  //def betaEtaEquals(a: Expr, b: Expr): Boolean = BENF(a).get == BENF(b).get

  // notation
  val tileSize = 4

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
    val i = Identifier(freshName("e"))
    map(Lambda(i, Apply(x, i)))
  }

  def **!(x: Expr): Expr = {
    val i = Identifier(freshName("e"))
    map(Lambda(i, Apply(*!(x), i)))
  }

  def ***!(x: Expr): Expr = {
    val i = Identifier(freshName("e"))
    map(Lambda(i, Apply(**!(x), i)))
  }

  def ****!(x: Expr): Expr = {
    val i = Identifier(freshName("e"))
    map(Lambda(i, Apply(***!(x), i)))
  }
}
