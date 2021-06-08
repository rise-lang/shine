package rise.eqsat

import rise.core.Expr
import rise.core.DSL._
import rise.core.DSL.Type._
import rise.core.types._
import rise.elevate.util._
import ProveEquiv.syntax._

class Reorder extends test_util.Tests {
  private val reorderRulesCNF = Seq(
    // rules.combinatory.compositionAssoc1,
    rules.combinatory.compositionAssoc2,
    // rules.combinatory.compositionIntro, //.directed(),
    // rules.combinatory.compositionLeftId, //.directed(),
    // rules.combinatory.compositionRightId, //.directed(),
    rules.combinatory.mapFusion,
    rules.combinatory.mapFission,
    rules.combinatory.transposePairAfter,
    rules.combinatory.mapMapFBeforeTranspose,
    rules.combinatory.mapMapFBeforeTranspose1,
    // rules.combinatory.mapMapFBeforeTranspose2,
  )

  private val reorderRulesBENF = Seq(
    rules.eta, //.directed(),
    rules.betaExtract.directed(),
    rules.betaNatExtract.directed(),
    rules.mapFusion, rules.mapFission,
    rules.transposePairAfter, rules.mapMapFBeforeTranspose,
    rules.etaAbstraction,
    // rules.idAfter, rules.createTransposePair,
    // rules.transposePairAfter2,
    // rules.transposePairAfter3,
    // rules.transposePairAfter4,
  )

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

    ProveEquiv.init().runCNF(expr, gold, reorderRulesCNF)
    ProveEquiv.init().runBENF(expr, gold, reorderRulesBENF)
  }

  test("reorder 3D") {
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

    // ~1.5s on laptop with both assoc rules
    // ~750ms on laptop with only one assoc rule
    ProveEquiv.init()
      .withFilter(ASTSizePredicate(50))
      .runCNF(expr, Seq(
        gold132, gold213, gold231, gold321, gold312
      ), reorderRulesCNF)

    // ~13s on laptop
    /*
    ProveEquiv.init()
      .withFilter(ASTSizePredicate(50))
      .runBENF(expr, Seq(
        gold132, gold213, gold231, gold321, gold312
      ), reorderRulesBENF) */
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

    ProveEquiv.init()
      .withFilter(ASTSizePredicate(150))
      /*.withRunnerTransform(r => r
        .withIterationLimit(5))
      .withEndRules(Seq(
        rules.combinatory.compositionAssoc1, // .directed(),
        rules.combinatory.compositionAssoc2
      ))*/
      .runCNF(expr, Seq(
        gold1243, gold1324, gold2134,
        // FIXME: too hard to reach right now
        gold4321
      ), reorderRulesCNF)

    // FIXME: difficulties reaching all of the goals with BENF
    /* proveEquiv.runBENF(expr, Seq(
      gold1243, gold1324, gold2134, gold4321
    ), reorderRulesBENF) */
  }
}
