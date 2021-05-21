package benchmarks.eqsat

import rise.core.DSL.Type._
import rise.core.DSL._
import rise.core.Expr
import rise.core.types._
import rise.eqsat.{ProveEquiv, rules}

object tiling {
  def main(args: Array[String]): Unit = {
    def wrap(inner: ToBeTyped[Expr] => ToBeTyped[Expr]): Expr = {
      depFun((n: Nat) => depFun((m: Nat) =>
      depFun((dt1: DataType) => depFun((dt2: DataType) =>
      fun(i => fun(f =>
        inner(f)(i :: (n`.`m`.`dt1)) :: (n`.`m`.`dt2)
      ))))))
    }

    val tileSize = 4
    def T: ToBeTyped[Expr] = rise.core.primitives.transpose
    def S: ToBeTyped[Expr] = rise.core.primitives.split(tileSize)
    def J: ToBeTyped[Expr] = rise.core.primitives.join
    def *(x: ToBeTyped[Expr]): ToBeTyped[Expr] = rise.core.primitives.map(x)
    def **(x: ToBeTyped[Expr]): ToBeTyped[Expr] = *(*(x))
    def ***(x: ToBeTyped[Expr]): ToBeTyped[Expr] = *(**(x))
    def ****(x: ToBeTyped[Expr]): ToBeTyped[Expr] = *(***(x))
    def *****(x: ToBeTyped[Expr]): ToBeTyped[Expr] = *(****(x))
    def ******(x: ToBeTyped[Expr]): ToBeTyped[Expr] = *(*****(x))

    val expr = wrap(f => **(f))
    val golds = Seq(
      // 1 loop
      wrap(f => J o ***(f) o S),
      wrap(f => *(J o **(f) o S)),
      // 2 loops
      wrap(f => J o **(J) o *(T) o ****(f) o *(T) o **(S) o S)
    )

    ProveEquiv.init().runCNF(expr, golds, Seq(
      rules.combinatory.compositionAssoc1,
      rules.combinatory.compositionAssoc2,
      rules.combinatory.compositionIntro,
      rules.combinatory.compositionLeftId,
      rules.combinatory.compositionRightId,
      rules.combinatory.splitJoin(tileSize),
      rules.combinatory.mapFusion,
      rules.combinatory.mapFission,
      rules.combinatory.transposePairAfter,
      rules.combinatory.mapMapFBeforeTranspose,
    ))
  }
}
