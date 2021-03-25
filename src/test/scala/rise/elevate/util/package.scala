package rise.elevate

import rise.elevate.strategies.normalForm.BENF
import _root_.rise.core.primitives._
import _root_.rise.core._
import _root_.rise.core.types._
import _root_.rise.core.DSL._
import elevate.core.strategies.Traversable

package object util {

  // Rise-related utils

  def betaEtaEquals(a: Rise, b: Rise)(implicit ev: Traversable[Rise]): Boolean =
    BENF()(ev)(makeClosed(a)).get =~= BENF()(ev)(makeClosed(b)).get

  val tileSize = 4

  // notation
  def T: ToBeTyped[Rise] = transpose
  def S: ToBeTyped[DepApp[NatKind]] = split(tileSize) //slide(3)(1)
  def J: ToBeTyped[Rise] = join
  def *(x: ToBeTyped[Rise]): ToBeTyped[App] = map(x)
  def **(x: ToBeTyped[Rise]): ToBeTyped[App] = map(map(x))
  def ***(x: ToBeTyped[Rise]): ToBeTyped[App] = map(map(map(x)))
  def ****(x: ToBeTyped[Rise]): ToBeTyped[App] = map(map(map(map(x))))
  def *****(x: ToBeTyped[Rise]): ToBeTyped[App] = map(map(map(map(map(x)))))
  def ******(x: ToBeTyped[Rise]): ToBeTyped[App] = map(map(map(map(map(map(x))))))

  def λ(f: ToBeTyped[Identifier] => ToBeTyped[Expr]): ToBeTyped[Lambda] = fun(f)

  // map in LCNF
  def *!(x: ToBeTyped[Rise]): ToBeTyped[App] = {
    val i = identifier(freshName("e"))
    map(lambda(i, app(x, i)))
  }

  def **!(x: ToBeTyped[Rise]): ToBeTyped[App] = {
    val i = identifier(freshName("e"))
    map(lambda(i, app(*!(x), i)))
  }

  def ***!(x: ToBeTyped[Rise]): ToBeTyped[App] = {
    val i = identifier(freshName("e"))
    map(lambda(i, app(**!(x), i)))
  }

  def ****!(x: ToBeTyped[Rise]): ToBeTyped[App] = {
    val i = identifier(freshName("e"))
    map(lambda(i, app(***!(x), i)))
  }

  def testMultiple(list: List[Rise], gold: Rise)(implicit ev: Traversable[Rise]): Unit = {
    assert(list.forall(betaEtaEquals(_, gold)))
  }
}
