package benchmarks.eqsat

import rise.eqsat.{ASTSizePredicate, BackoffScheduler, CuttingScheduler, ProveEquiv, rules}
import ProveEquiv.syntax._
import rise.core.Expr
import rise.core.DSL._
import rise.core.DSL.Type._
import rise.core.types._

object reorder {
  private val reorderRules = Seq(
    // rules.combinatory.compositionAssoc1,
    rules.combinatory.compositionAssoc2,
    //rules.combinatory.compositionIntro.directed(),
    //rules.combinatory.compositionLeftId.directed(),
    //rules.combinatory.compositionRightId.directed(),
    // rules.combinatory.mapFusion,
    // rules.combinatory.mapFission,
    // rules.combinatory.transposePairAfter,
    // rules.combinatory.mapMapFBeforeTranspose,
    // rules.combinatory.mapMapFBeforeTranspose1,
    rules.combinatory.transposeAroundMapMapF,
    rules.combinatory.transposeAroundMapMapMapF,
    rules.combinatory.transposeAroundMapMapMapMapF
  )

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
    // all below should be implied by gold132 and gold213 modulo associativity and types
    val gold231 = wrap(f => T o *(T) o ***(f) o *(T) o T)
    val gold321 = wrap(f => *(T) o T o *(T) o ***(f) o *(T) o T o *(T))
    val gold312 = wrap(f => *(T) o T o ***(f) o T o *(T))

    ProveEquiv.init()
      .withFilter(ASTSizePredicate(50))
      //.bidirectional()
      //.withRunnerTransform(r => r.withIterationLimit(3))
      //.withEndRules(Seq(
      //  rules.combinatory.compositionAssoc1,
      //  /*rules.combinatory.compositionAssoc2*/))
      .runCNF(expr, Seq(
      gold132, gold213, gold231, gold321, gold312
    ), reorderRules)
  }

  private def run4D(): Unit = {
    def wrap(inner: ToBeTyped[Expr] => ToBeTyped[Expr] => ToBeTyped[Expr]): ToBeTyped[Expr] = {
      depFun((n: Nat) => depFun((m: Nat) => depFun((o: Nat) => depFun((p: Nat) =>
      depFun((dt1: DataType) => depFun((dt2: DataType) =>
      fun(i => fun(f =>
        inner(i :: (n`.`m`.`o`.`p`.`dt1))(f) :: (n`.`m`.`o`.`p`.`dt2)
          // (n`.`n`.`n`.`n`.`dt1))(f) :: (n`.`n`.`n`.`n`.`dt2)
      ))))))))
    }

    val expr: Expr = wrap(i => f => ****(f) $ i)
    val gold1243: Expr = wrap(i => f => (**(T) o ****(f) o **(T)) $ i)
    val gold1324: Expr = wrap(i => f => (*(T) o ****(f) o *(T)) $ i)
    val gold2134: Expr = wrap(i => f => (T o ****(f) o T) $ i)
    // should be implied by above goals modulo associativity and types
    val gold4321: Expr = wrap(i => f => (**(T) o *(T) o T o **(T) o *(T) o **(T) o ****(f) o
      **(T) o *(T) o **(T) o T o *(T) o **(T)) $ i)

    ProveEquiv.init()
      .withFilter(ASTSizePredicate(80))
      /*
      .withRunnerTransform(r => r.withIterationLimit(5))
      .withEndRules(Seq(
        rules.combinatory.compositionAssoc1.directed()))*/
      .runCNF(expr, Seq(
        gold1243, gold1324, gold2134, gold4321
      ), reorderRules)
  }

  def main(args: Array[String]): Unit = {
    val (time3D, _) = util.time(run3D())
    val (time4D, _) = util.time(run4D())
    // ~1ms search in untyped egg prototype
    // ~4s search on i7 desktop without ast size filter
    // ~1s search on i7 desktop with ast size filter
    // ~0.1s search on i7 desktop with specialized rules
    println(s"total 3D time: ${util.prettyTime(time3D)}")
    // ~25s search on i7 with all above
    println(s"total 4D time: ${util.prettyTime(time4D)}")
  }
}
