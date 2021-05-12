package rise.eqsat

import rise.core.Expr
import rise.core.DSL._
import rise.core.DSL.Type._
import rise.core.types._
import Basic.{proveEquivBENF, proveEquivCNF}
import rise.elevate.util._

object Reorder {
  def main(args: Array[String]): Unit = {
    def wrap(inner: ToBeTyped[Expr] => ToBeTyped[Expr]): Expr = {
      depFun((n: Nat) => depFun((m: Nat) => depFun((o: Nat) =>
        depFun((dt1: DataType) => depFun((dt2: DataType) =>
          fun(i => fun(f =>
            inner(f)(i :: (n`.`m`.`o`.`dt1)) :: (n`.`m`.`o`.`dt2)
          )))))))
    }

    val expr = wrap(f => ***!(f))
    val gold132 = wrap(f => *!(T) o ***!(f) o *!(T))
    val gold213 = wrap(f => T o ***!(f) o T)
    val gold231 = wrap(f => T o *!(T) o ***!(f) o *!(T) o T)
    val gold321 = wrap(f => *!(T) o T o *!(T) o ***!(f) o *!(T) o T o *!(T))
    val gold312 = wrap(f => *!(T) o T o ***!(f) o T o *!(T))

    proveEquivCNF(expr, Seq(
      gold132, gold213, gold231, gold321, gold312,
      wrap(f => *!(T) o T o *!(T o T o **!(f)) o T o *!(T))
    ), Seq(
      rules.eta, rules.betaExtract, rules.betaNatExtract,
      // rules.combinatory.compositionElim,
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

class Reorder extends test_util.Tests {
  test("reorder 2D") {
    def wrap(inner: ToBeTyped[Expr] => ToBeTyped[Expr] => ToBeTyped[Expr]): ToBeTyped[Expr] = {
      depFun((n: Nat) => depFun((m: Nat) =>
      depFun((dt1: DataType) => depFun((dt2: DataType) =>
      fun(i => fun(f =>
        inner(i :: (n`.`m`.`dt1))(f) :: (n`.`m`.`dt2)
      ))))))
    }

    val expr: Expr = wrap(i => f => **!(f) $ i)
    val gold: Expr = wrap(i => f => (T o **!(f) o T) $ i)

    proveEquivBENF(expr, gold, Seq(
      rules.eta, rules.beta, rules.betaNat,
      rules.mapFusion, rules.mapFission,
      rules.transposePairAfter, rules.mapMapFBeforeTranspose
    ))
  }

  test("reorder 3D") {
    def wrap(inner: ToBeTyped[Expr] => ToBeTyped[Expr]): Expr = {
      depFun((n: Nat) => depFun((m: Nat) => depFun((o: Nat) =>
      depFun((dt1: DataType) => depFun((dt2: DataType) =>
      fun(i => fun(f =>
        inner(f)(i :: (n`.`m`.`o`.`dt1)) :: (n`.`m`.`o`.`dt2)
      )))))))
    }

    val expr = wrap(f => ***!(f))
    val gold132 = wrap(f => *!(T) o ***!(f) o *!(T))
    val gold213 = wrap(f => T o ***!(f) o T)
    val gold231 = wrap(f => T o *!(T) o ***!(f) o *!(T) o T)
    val gold321 = wrap(f => *!(T) o T o *!(T) o ***!(f) o *!(T) o T o *!(T))
    val gold312 = wrap(f => *!(T) o T o ***!(f) o T o *!(T))

    // FIXME: difficulties reaching all of the goals using BENF
    /*
    val rs = Seq(
      rules.eta, rules.beta, rules.betaNat,
      rules.etaAbstraction,
      // rules.gentleEtaAbstraction,
      /*rules.mapFusion,*/ rules.mapFission,
      // rules.idAfter, rules.createTransposePair,
      rules.transposePairAfter,
      // rules.transposePairAfter2,
      // rules.transposePairAfter3,
      // rules.transposePairAfter4,
      rules.mapMapFBeforeTranspose,// rules.transposeBeforeMapMapF
    )

    proveEquivBENF(expr, wrap(f => *(T) o *(**(f) o T)), rs)
    proveEquivBENF(wrap(f => *(T) o *(**(f) o T)), wrap(f => *(T) o ***(f) o *(T)), rs)
    proveEquivBENF(expr, wrap(f => *(T) o ***(f) o *(T)), rs)
    // proveEquiv(expr, gold132, rs)
    proveEquivBENF(expr, Seq(
      gold132, gold213, gold231, gold321, gold312
    ), rs)
     */
    proveEquivCNF(expr, Seq(
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

  test("reorder 4D") {
    def wrap(inner: ToBeTyped[Expr] => ToBeTyped[Expr] => ToBeTyped[Expr]): ToBeTyped[Expr] = {
      depFun((n: Nat) => depFun((m: Nat) => depFun((o: Nat) => depFun((p: Nat) =>
      depFun((dt1: DataType) => depFun((dt2: DataType) =>
      fun(i => fun(f =>
        inner(i :: (n`.`m`.`o`.`p`.`dt1))(f) :: (n`.`m`.`o`.`p`.`dt2)
      ))))))))
    }

    val expr: Expr = wrap(i => f => ****!(f) $ i)
    val gold1243: Expr = wrap(i => f => (**!(T) o ****!(f) o **!(T)) $ i)
    val gold1324: Expr = wrap(i => f => (*!(T) o ****!(f) o *!(T)) $ i)
    val gold2134: Expr = wrap(i => f => (T o ****!(f) o T) $ i)
    val gold4321: Expr = wrap(i => f => (**!(T) o *!(T) o T o **!(T) o *!(T) o **!(T) o ****!(f) o
      **!(T) o *!(T) o **!(T) o T o  *!(T) o **!(T)) $ i)

    proveEquivCNF(expr, Seq(
      gold1243, gold1324, gold2134, gold4321
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
