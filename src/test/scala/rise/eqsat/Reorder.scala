package rise.eqsat

import rise.core.Expr
import rise.core.DSL._
import rise.core.DSL.Type._
import rise.core.types._
import rise.elevate.util._
import ProveEquiv.syntax._

class Reorder extends test_util.Tests {
  private val reorderRules = Seq(
    rules.combinatory.compositionAssoc1,
    rules.combinatory.compositionAssoc2,
    rules.combinatory.compositionIntro,
    rules.combinatory.compositionLeftId,
    rules.combinatory.compositionRightId,
    rules.combinatory.mapFusion,
    rules.combinatory.mapFission,
    rules.combinatory.transposePairAfter,
    rules.combinatory.mapMapFBeforeTranspose,
  )

  private def proveEquiv = ProveEquiv.init()
    .withRunnerTransform(r => r.withScheduler(BackoffScheduler.init()))

  test("reorder 2D") {
    def wrap(inner: ToBeTyped[Expr] => ToBeTyped[Expr]): Expr = {
      depFun((n: Nat) => depFun((m: Nat) =>
      depFun((dt1: DataType) => depFun((dt2: DataType) =>
      fun(i => fun(f =>
        inner(f)(i :: (n`.`m`.`dt1)) :: (n`.`m`.`dt2)
      ))))))
    }

    val expr = wrap(f => **(f))
    val gold = wrap(f => T o **(f) o T)

    proveEquiv.runCNF(expr, gold, reorderRules)

    proveEquiv.runBENF(expr, gold, Seq(
      rules.eta, rules.betaExtract, rules.betaNatExtract,
      rules.mapFusion,
      rules.mapFission,
      rules.transposePairAfter, rules.mapMapFBeforeTranspose
    ))
  }

  test("reorder 3D") {
    def wrap(inner: ToBeTyped[Expr] => ToBeTyped[Expr]): Expr = {
      depFun((n: Nat) => depFun((m: Nat) => depFun((o: Nat) =>
      depFun((dt1: DataType) => depFun((dt2: DataType) =>
      fun(i => fun(f =>
        inner(f)(i :: //(n`.`m`.`o`.`dt1)) :: (n`.`m`.`o`.`dt2)
          (n`.`n`.`n`.`dt1)) :: (n`.`n`.`n`.`dt2)
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

    // FIXME: difficulties reaching all of the goals using BENF
    /*
    val benfRules = Seq(
      rules.eta,
      rules.betaExtract, rules.betaNatExtract,
      // rules.beta, rules.betaNat,
      // rules.etaAbstraction,
      // rules.gentleEtaAbstraction,
      rules.mapFusion, rules.mapFission,
      // rules.idAfter, rules.createTransposePair,
      // rules.transposePairAfter,
      rules.transposePairAfter2,
      // rules.transposePairAfter3,
      rules.transposePairAfter4,
      rules.mapMapFBeforeTranspose,// rules.transposeBeforeMapMapF
    )

    ProveEquiv.init().runBENF(expr, Seq(
      gold132, gold213, gold231, gold321, gold312
    ), benfRules)
    */
  }

  test("reorder 4D") {
    def wrap(inner: ToBeTyped[Expr] => ToBeTyped[Expr]): Expr = {
      depFun((n: Nat) => depFun((m: Nat) => depFun((o: Nat) => depFun((p: Nat) =>
      depFun((dt1: DataType) => depFun((dt2: DataType) =>
      fun(i => fun(f =>
        inner(f)(i :: //(n`.`m`.`o`.`p`.`dt1))(f) :: (n`.`m`.`o`.`p`.`dt2)
          (n`.`n`.`n`.`n`.`dt1)) :: (n`.`n`.`n`.`n`.`dt2)
      ))))))))
    }

    val expr = wrap(f => ****(f))
    val gold1243 = wrap(f => **(T) o ****(f) o **(T))
    val gold1324 = wrap(f => *(T) o ****(f) o *(T))
    val gold2134 = wrap(f => T o ****(f) o T)
    val gold4321 = wrap(f => **(T) o *(T) o T o **(T) o *(T) o **(T) o ****(f) o
      **(T) o *(T) o **(T) o T o *(T) o **(T))

    proveEquiv
      .withRunnerTransform(r => r
        .withIterationLimit(5))
      .withEndRules(Seq(
        rules.combinatory.compositionAssoc1,
      ))
      .runCNF(expr, Seq(
        gold1243, gold1324, gold2134, gold4321
      ), reorderRules)

    // FIXME: difficulties reaching all of the goals with BENF
    /*
    proveEquivBENF(expr, Seq(gold1243, gold1324, gold2134, gold4321), Seq(
      rules.eta, rules.beta, rules.betaNat,
      // rules.etaAbstraction,
      rules.mapFusion, rules.mapFission,
      // rules.idAfter, rules.createTransposePair,
      rules.transposePairAfter,
      rules.mapMapFBeforeTranspose
    ))
     */
  }
}
