package rise.eqsat

import rise.core.Expr
import rise.core.DSL._
import rise.core.DSL.Type._
import rise.core.types._
import rise.elevate.util._
import ProveEquiv.syntax._
import PredicateDSL._

class Tiling extends test_util.Tests {
  private val minimalRules = Seq(
    // rules.combinatory.compositionAssoc1,
    rules.combinatory.compositionAssoc2,
    // rules.combinatory.compositionIntro,
    // rules.combinatory.compositionLeftId,
    // rules.combinatory.compositionRightId,
    rules.combinatory.splitJoin(tileSize),
  )

  private val reorderRules = Seq(
    rules.combinatory.mapFusion,
    rules.combinatory.mapFission,
    rules.combinatory.transposePairAfter,
    rules.combinatory.mapMapFBeforeTranspose,
    rules.combinatory.mapMapFBeforeTranspose1,
  )

  test("tile 1D") {
    def wrap(inner: ToBeTyped[Expr] => ToBeTyped[Expr]): Expr = {
      depFun((n: Nat) => depFun((m: Nat) =>
      depFun((dt1: DataType) => depFun((dt2: DataType) =>
      fun(i => fun(f =>
        inner(f)(i :: (n`.`m`.`dt1)) :: (n`.`m`.`dt2)
      ))))))
    }

    val expr = wrap(f => *(f))
    val gold = wrap(f => J o **(f) o S)

    ProveEquiv.init().runCNF(expr, gold, minimalRules)
  }

  test("tile 2D") {
    def wrap(inner: ToBeTyped[Expr] => ToBeTyped[Expr]): Expr = {
      depFun((n: Nat) => depFun((m: Nat) =>
      depFun((dt1: DataType) => depFun((dt2: DataType) =>
      fun(i => fun(f =>
        inner(f)(i :: (n`.`m`.`dt1)) :: (n`.`m`.`dt2)
      ))))))
    }

    val expr = wrap(f => **(f))
    val golds = Seq(
      // 1 loop
      wrap(f => J o ***(f) o S),
      wrap(f => *(J o **(f) o S)),
      // 2 loops
      wrap(f => J o **(J) o *(T) o ****(f) o *(T) o **(S) o S)
    )

    // ~12s on laptop with array dimension predicate
    // ~6s  on laptop with ast size predicate on top
    // ~2s  on laptop with one sided assoc
    ProveEquiv.init()
      .withFilter(ArrayDimensionPredicate(4) && ASTSizePredicate(40))
      .runCNF(expr, golds, minimalRules ++ reorderRules)
  }

  test("tile 3D") {
    def wrap(inner: ToBeTyped[Expr] => ToBeTyped[Expr]): Expr = {
      depFun((n: Nat) => depFun((m: Nat) => depFun((o: Nat) =>
      depFun((dt1: DataType) => depFun((dt2: DataType) =>
      fun(i => fun(f =>
        inner(f)(i :: (n`.`m`.`o`.`dt1)) :: (n`.`m`.`o`.`dt2)
      )))))))
    }

    val expr = wrap(f => ***(f))
    val golds = Seq(
      // 1 loop
      wrap(f => J o ****(f) o S),
      wrap(f => *(J o ***(f) o S)),
      wrap(f => **(J o **(f) o S)),
      // 2 loops
      wrap(f => J o **(J) o *(T) o *****(f) o *(T) o **(S) o S),
      wrap(f => *(J o **(J) o *(T) o ****(f) o *(T) o **(S) o S)),
      // 3 loops
      wrap(f =>
        J o **(J) o ****(J) o
        ***(T) o *(T) o **(T) o
        ******(f) o
        **(T) o *(T) o ***(T) o
        ****(S) o **(S) o S),
    )

    ProveEquiv.init()
      //.withRunnerTransform(r => r.withScheduler(BackoffScheduler.init()))
      .withFilter(ArrayDimensionPredicate(6) && ASTSizePredicate(80))
      .runCNF(expr, golds, minimalRules ++ reorderRules)
  }

  test("tile 4D") {
    def wrap(inner: ToBeTyped[Expr] => ToBeTyped[Expr]): Expr = {
      depFun((n: Nat) => depFun((m: Nat) => depFun((o: Nat) => depFun((p: Nat) =>
      depFun((dt1: DataType) => depFun((dt2: DataType) =>
      fun(i => fun(f =>
        inner(f)(i :: //(n`.`m`.`o`.`p`.`dt1)) :: (n`.`m`.`o`.`p`.`dt2)
          (n`.`n`.`n`.`n`.`dt1)) :: (n`.`n`.`n`.`n`.`dt2)
      ))))))))
    }

    val expr = wrap(f => ****(f))
    val golds = Seq(
      // 1 loop
      wrap(f => J o *****(f) o S),
      wrap(f => *(J o ****(f) o S)),
      wrap(f => **(J o ***(f) o S)),
      wrap(f => ***(J o **(f) o S)),
      // 2 loops
      wrap(f => J o **(J) o *(T) o ******(f) o *(T) o **(S) o S),
      wrap(f => *(J o **(J) o *(T) o *****(f) o *(T) o **(S) o S)),
      wrap(f => **(J o **(J) o *(T) o ****(f) o *(T) o **(S) o S)),
      // 3 loops
      wrap(f =>
        J o **(J) o ****(J) o
        ***(T) o *(T) o **(T) o
        *(******(f)) o
        **(T) o *(T) o ***(T) o
        ****(S) o **(S) o S),
      wrap(f => *(
        J o **(J) o ****(J) o
        ***(T) o *(T) o **(T) o
        ******(f) o
        **(T) o *(T) o ***(T) o
        ****(S) o **(S) o S)),
      // 4 loops
      wrap(f =>
        J o **(J) o ****(J) o ******(J) o
        *****(T) o ***(T) o ****(T) o *(T) o **(T) o ***(T) o
        ****(****(f)) o
        ***(T) o **(T) o *(T) o ****(T) o ***(T) o *****(T) o
        ******(S) o ****(S) o **(S) o S),
    )

    ProveEquiv.init()
      //.withRunnerTransform(r => r.withScheduler(BackoffScheduler.init()))
      .withFilter(ArrayDimensionPredicate(8))
      .runCNF(expr, golds, minimalRules ++ reorderRules)
  }
}
