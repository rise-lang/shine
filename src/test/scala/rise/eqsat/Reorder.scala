package rise.eqsat

import rise.core.Expr
import rise.core.DSL._
import rise.core.DSL.Type._
import rise.core.types._
import Basic.proveEquiv
import rise.elevate.util._

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

    proveEquiv(expr, gold, Seq(
      rules.eta, rules.beta, rules.betaNat,
      rules.mapFusion, rules.mapFission,
      rules.transposePairAfter, rules.mapMapFBeforeTranspose
    ))
  }

  // FIXME: difficulties reaching all of the goals
  ignore("reorder 3D") {
    def wrap(inner: ToBeTyped[Expr] => ToBeTyped[Expr] => ToBeTyped[Expr]): Expr = {
      depFun((n: Nat) => depFun((m: Nat) => depFun((o: Nat) =>
      depFun((dt1: DataType) => depFun((dt2: DataType) =>
      fun(i => fun(f =>
        inner(i :: (n`.`m`.`o`.`dt1))(f) :: (n`.`m`.`o`.`dt2)
      )))))))
    }

    val expr = wrap(i => f => ***!(f) $ i)
    val gold132 = wrap(i => f => (*!(T) o ***!(f) o *!(T)) $ i)
    val gold213 = wrap(i => f => (T o ***!(f) o T) $ i) // not reached
    val gold231 = wrap(i => f => (T o *!(T) o ***!(f) o *!(T) o T) $ i)
    val gold321 = wrap(i => f => (*!(T) o T o *!(T) o ***!(f) o *!(T) o T o *!(T)) $ i)
    val gold312 = wrap(i => f => (*!(T) o T o ***!(f) o T o *!(T)) $ i)

    proveEquiv(expr, Seq(
      gold132, gold213, gold231, gold321, gold312
    ), Seq(
      rules.eta, rules.beta, rules.betaNat,
      // rules.etaAbstraction,
      rules.mapFusion, rules.mapFission,
      // rules.idAfter, rules.createTransposePair,
      rules.transposePairAfter, rules.mapMapFBeforeTranspose
    ))
  }

  // FIXME: difficulties reaching all of the goals
  ignore("reorder 4D") {
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

    proveEquiv(expr, Seq(gold1243, gold1324, gold2134, gold4321), Seq(
      rules.eta, rules.beta, rules.betaNat,
      // rules.etaAbstraction,
      rules.mapFusion, rules.mapFission,
      // rules.idAfter, rules.createTransposePair,
      rules.transposePairAfter,
      rules.mapMapFBeforeTranspose
    ))
  }
}
