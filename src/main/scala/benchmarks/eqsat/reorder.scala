package benchmarks.eqsat

import rise.eqsat.{rules, ProveEquiv}
import rise.core.Expr
import rise.core.DSL._
import rise.core.DSL.Type._
import rise.core.types._

object reorder {
  def main(args: Array[String]): Unit = {
    def wrap(inner: ToBeTyped[Expr] => ToBeTyped[Expr]): Expr = {
      depFun((n: Nat) => depFun((m: Nat) => depFun((o: Nat) =>
      depFun((dt1: DataType) => depFun((dt2: DataType) =>
      fun(i => fun(f =>
        inner(f)(i :: (n`.`m`.`o`.`dt1)) :: (n`.`m`.`o`.`dt2)
      )))))))
    }

    def T: ToBeTyped[Expr] = rise.core.primitives.transpose
    def S: ToBeTyped[Expr] = rise.core.primitives.split(4)
    def J: ToBeTyped[Expr] = rise.core.primitives.join
    def *(x: ToBeTyped[Expr]): ToBeTyped[Expr] = rise.core.primitives.map(x)
    def **(x: ToBeTyped[Expr]): ToBeTyped[Expr] = *(*(x))
    def ***(x: ToBeTyped[Expr]): ToBeTyped[Expr] = *(**(x))
    def ****(x: ToBeTyped[Expr]): ToBeTyped[Expr] = *(***(x))
    def *****(x: ToBeTyped[Expr]): ToBeTyped[Expr] = *(****(x))
    def ******(x: ToBeTyped[Expr]): ToBeTyped[Expr] = *(*****(x))


    val expr = wrap(f => ***(f))
    val gold132 = wrap(f => *(T) o ***(f) o *(T))
    val gold213 = wrap(f => T o ***(f) o T)
    val gold231 = wrap(f => T o *(T) o ***(f) o *(T) o T)
    val gold321 = wrap(f => *(T) o T o *(T) o ***(f) o *(T) o T o *(T))
    val gold312 = wrap(f => *(T) o T o ***(f) o T o *(T))

    ProveEquiv.init().runCNF(expr, Seq(
      gold132, gold213, gold231, gold321, gold312
    ), Seq(
      rules.combinatory.compositionAssoc1,
      rules.combinatory.compositionAssoc2,
      rules.combinatory.compositionIntro,
      rules.combinatory.compositionLeftId,
      rules.combinatory.compositionRightId,
      rules.combinatory.mapFusion,
      rules.combinatory.mapFission,
      rules.combinatory.transposePairAfter,
      rules.combinatory.mapMapFBeforeTranspose,
    ))
  }
}
