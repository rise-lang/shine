package benchmarks.eqsat

import rise.eqsat.{rules, ProveEquiv, BackoffScheduler}
import rise.core.Expr
import rise.core.DSL._
import rise.core.DSL.Type._
import rise.core.types._

object reorder {
  private val reorderRules = Seq(
    rules.combinatory.compositionAssoc1,
    rules.combinatory.compositionAssoc2,
    rules.combinatory.compositionIntro,
    rules.combinatory.compositionLeftId,
    rules.combinatory.compositionRightId,
    rules.combinatory.mapFusion,
    rules.combinatory.mapFission,
    // rules.combinatory.transposePairAfter,
    // rules.combinatory.mapMapFBeforeTranspose,
    rules.combinatory.transposeAroundMapMapF
  )

  private val proveEquiv = ProveEquiv.init()
    .withRunnerTransform(r => r
      .withTimeLimit(java.time.Duration.ofMinutes(5))
      /*.withScheduler(BackoffScheduler.init())*/)

  private def T: ToBeTyped[Expr] = rise.core.primitives.transpose
  private def *(x: ToBeTyped[Expr]): ToBeTyped[Expr] = rise.core.primitives.map(x)
  private def **(x: ToBeTyped[Expr]): ToBeTyped[Expr] = *(*(x))
  private def ***(x: ToBeTyped[Expr]): ToBeTyped[Expr] = *(**(x))
  private def ****(x: ToBeTyped[Expr]): ToBeTyped[Expr] = *(***(x))

  private def run3D(): Unit = {
    def wrap(inner: ToBeTyped[Expr] => ToBeTyped[Expr]): Expr = {
      depFun((n: Nat) => depFun((m: Nat) => depFun((o: Nat) =>
      depFun((dt1: DataType) => depFun((dt2: DataType) =>
      fun(i => fun(f =>
        inner(f)(i :: (n`.`m`.`o`.`dt1)) :: (n`.`m`.`o`.`dt2)
      )))))))
    }

    val expr = wrap(f => ***(f))
    val gold132 = wrap(f => *(T) o ***(f) o *(T))
    val gold213 = wrap(f => T o ***(f) o T)
    val gold231 = wrap(f => T o *(T) o ***(f) o *(T) o T)
    val gold321 = wrap(f => *(T) o T o *(T) o ***(f) o *(T) o T o *(T))
    val gold312 = wrap(f => *(T) o T o ***(f) o T o *(T))

    proveEquiv.runCNF(expr, Seq(
      gold132, gold213, gold231, gold321, gold312
    ), reorderRules)
  }

  private def run4D(): Unit = {
    def wrap(inner: ToBeTyped[Expr] => ToBeTyped[Expr] => ToBeTyped[Expr]): ToBeTyped[Expr] = {
      depFun((n: Nat) => depFun((m: Nat) => depFun((o: Nat) => depFun((p: Nat) =>
      depFun((dt1: DataType) => depFun((dt2: DataType) =>
      fun(i => fun(f =>
        inner(i :: (n`.`m`.`o`.`p`.`dt1))(f) :: (n`.`m`.`o`.`p`.`dt2)
      ))))))))
    }

    val expr: Expr = wrap(i => f => ****(f) $ i)
    val gold1243: Expr = wrap(i => f => (**(T) o ****(f) o **(T)) $ i)
    val gold1324: Expr = wrap(i => f => (*(T) o ****(f) o *(T)) $ i)
    val gold2134: Expr = wrap(i => f => (T o ****(f) o T) $ i)
    val gold4321: Expr = wrap(i => f => (**(T) o *(T) o T o **(T) o *(T) o **(T) o ****(f) o
      **(T) o *(T) o **(T) o T o *(T) o **(T)) $ i)

    proveEquiv.runCNF(expr, Seq(
      gold1243, gold1324, gold2134, gold4321
    ), reorderRules)
  }

  def main(args: Array[String]): Unit = {
    val (time3D, _) = util.time(run3D())
    // val (time4D, _) = util.time(run4D())
    println(s"total 3D time: ${util.prettyTime(time3D)}") // ~2s search on i7 desktop
    // println(s"total 4D time: ${util.prettyTime(time4D)}")
  }
}
