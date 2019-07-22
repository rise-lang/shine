package elevate

import _root_.lift.core._
import _root_.lift.core.DSL._
import _root_.lift.core.primitives._
import elevate.lift.strategies.normalForm._

package object lift {

  def structEq(a: Expr, b: Expr): Boolean = StructuralEquality(
    LCNF(a).get, LCNF(b).get
  )

  // notation
  def T: Expr = transpose
  def S: Expr = split(4)//slide(3)(1)
  def J: Expr = join
  def *(x: Expr): Expr = map(x)
  def **(x: Expr): Expr = map(map(x))
  def ***(x: Expr): Expr = map(map(map(x)))
  def ****(x: Expr): Expr = map(map(map(map(x))))
  def *****(x: Expr): Expr = map(map(map(map(map(x)))))
  def ******(x: Expr): Expr = map(map(map(map(map(map(x))))))
  def λ(f: Identifier => Expr): Expr = fun(f)


}
