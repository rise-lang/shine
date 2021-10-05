package benchmarks.eqsat

import rise.core.DSL.Type._
import rise.core.DSL._
import rise.core.Expr
import rise.core.types._
import rise.eqsat.{ArrayDimensionPredicate, ASTSizePredicate, ProveEquiv, rules, Rewrite, FreeAnalysis}
import ProveEquiv.syntax._
import rise.eqsat.PredicateDSL._

object tiling {
  private val tileSize = 4

  private def whenFcontainsF(rw: Rewrite): Rewrite =
    new rise.eqsat.Rewrite(rw.name, rw.searcher, rise.eqsat.ConditionalApplier(
      { case (egraph, _, shc, subst) =>
        val freeOf = egraph.getAnalysis(FreeAnalysis)
        freeOf(subst(rise.eqsat.PatternVar(0), shc)).free.contains(0) },
      Set("f"),
      (Set(FreeAnalysis), Set()),
      rw.applier))

  private val tilingRules = Seq(
    // rules.combinatory.compositionAssoc1,
    // rules.combinatory.compositionAssoc2,
    // rules.combinatory.compositionIntro,
    // rules.combinatory.compositionLeftId,
    // rules.combinatory.compositionRightId,
    // rules.combinatory.mapFusion,
    // rules.combinatory.mapFission,
    // rules.combinatory.transposePairAfter,
    // rules.combinatory.mapMapFBeforeTranspose,
    whenFcontainsF(rules.combinatory.transposeAroundMapMapF),
    whenFcontainsF(rules.combinatory.transposeAroundMapMapF1M),
    whenFcontainsF(rules.combinatory.transposeAroundMapMapF2M),
    whenFcontainsF(rules.combinatory.transposeAroundMapMapF3M),
  )

  private val splitJoinRulesBENF = Seq(
    whenFcontainsF(rules.splitJoin(tileSize)),
    whenFcontainsF(rules.splitJoin1M(tileSize)),
    whenFcontainsF(rules.splitJoin2M(tileSize)),
    whenFcontainsF(rules.splitJoin3M(tileSize)),
    whenFcontainsF(rules.splitJoin4M(tileSize)),
    whenFcontainsF(rules.splitJoin5M(tileSize)),
    whenFcontainsF(rules.splitJoin6M(tileSize)),
  )

  private val reorderRulesBENF = Seq(
    whenFcontainsF(rules.transposeAroundMapMapF),
    whenFcontainsF(rules.transposeAroundMapMapF1M),
    whenFcontainsF(rules.transposeAroundMapMapF2M),
    whenFcontainsF(rules.transposeAroundMapMapF3M),
    whenFcontainsF(rules.transposeAroundMapMapF4M),
    whenFcontainsF(rules.transposeAroundMapMapF5M),
  )

  private val tilingRulesBENF = reorderRulesBENF ++ splitJoinRulesBENF

  private def T: ToBeTyped[Expr] = rise.core.primitives.transpose
  private def S: ToBeTyped[Expr] = rise.core.primitives.split(tileSize)
  private def J: ToBeTyped[Expr] = rise.core.primitives.join
  private def *(x: ToBeTyped[Expr]): ToBeTyped[Expr] = rise.core.primitives.map(x)
  private def **(x: ToBeTyped[Expr]): ToBeTyped[Expr] = *(*(x))
  private def ***(x: ToBeTyped[Expr]): ToBeTyped[Expr] = *(**(x))
  private def ****(x: ToBeTyped[Expr]): ToBeTyped[Expr] = *(***(x))
  private def *****(x: ToBeTyped[Expr]): ToBeTyped[Expr] = *(****(x))
  private def ******(x: ToBeTyped[Expr]): ToBeTyped[Expr] = *(*****(x))

  def run2D(): Unit = {
    def wrap(inner: ToBeTyped[Expr] => ToBeTyped[Expr]): Expr = {
      depFun((n: Nat) => depFun((m: Nat) =>
      depFun((dt1: DataType) => depFun((dt2: DataType) =>
      fun(i => fun(f =>
        inner(f)(i :: (n`.`m`.`dt1)) :: (n`.`m`.`dt2)
      ))))))
    }

    val expr = wrap(f => **(f))
    val golds1 = Seq(
      // 1 loop
      wrap(f => J o ***(f) o S),
      wrap(f => *(J) o ***(f) o *(S)),
      // 2 loops
      wrap(f => J o **(J) o ****(f) o **(S) o S)
    )
    val golds2 = Seq(
      // 1 loop
      wrap(f => J o ***(f) o S),
      wrap(f => *(J) o ***(f) o *(S)),
      // 2 loops
      wrap(f => J o **(J) o *(T) o ****(f) o *(T) o **(S) o S)
    )

    ProveEquiv.init()
      .withFilter(ArrayDimensionPredicate(4) && ASTSizePredicate(50))
      .runBENF(expr, golds1, splitJoinRulesBENF)

    ProveEquiv.init()
      .withFilter(ASTSizePredicate(50))
      .runBENF(golds1, golds2, reorderRulesBENF)
  }

  def run3D(): Unit = {
    def wrap(inner: ToBeTyped[Expr] => ToBeTyped[Expr]): Expr = {
      depFun((n: Nat) => depFun((m: Nat) => depFun((o: Nat) =>
      depFun((dt1: DataType) => depFun((dt2: DataType) =>
      fun(i => fun(f =>
        inner(f)(i :: (n`.`m`.`o`.`dt1)) :: (n`.`m`.`o`.`dt2)
      )))))))
    }

    val expr = wrap(f => ***(f))
    val golds1 = Seq(
      // 1 loop
      wrap(f => J o ****(f) o S),
      wrap(f => *(J) o ****(f) o *(S)),
      wrap(f => **(J) o ****(f) o **(S)),
      // 2 loops
      wrap(f => J o **(J) o *****(f) o **(S) o S),
      wrap(f => *(J) o ***(J) o *****(f) o ***(S) o *(S)),
      // 3 loops
      wrap(f =>
        J o **(J) o ****(J) o
        ******(f) o
        ****(S) o **(S) o S),
    )
    val golds2 = Seq(
      // 1 loop
      wrap(f => J o ****(f) o S),
      wrap(f => *(J) o ****(f) o *(S)),
      wrap(f => **(J) o ****(f) o **(S)),
      // 2 loops
      wrap(f => J o **(J) o *(T) o *****(f) o *(T) o **(S) o S),
      wrap(f => *(J) o ***(J) o **(T) o *****(f) o **(T) o ***(S) o *(S)),
      // 3 loops
      wrap(f =>
        J o **(J) o ****(J) o
        ***(T) o *(T) o **(T) o
        ******(f) o
        **(T) o *(T) o ***(T) o
        ****(S) o **(S) o S),
    )

    ProveEquiv.init()
      .withFilter(ArrayDimensionPredicate(6) && ASTSizePredicate(100))
      .runBENF(expr, golds1, splitJoinRulesBENF)

    ProveEquiv.init()
      .withFilter(ASTSizePredicate(100))
      .runBENF(golds1, golds2, reorderRulesBENF)
  }

  def run4D(): Unit = {
    def wrap(inner: ToBeTyped[Expr] => ToBeTyped[Expr]): Expr = {
      depFun((n: Nat) => depFun((m: Nat) => depFun((o: Nat) => depFun((p: Nat) =>
      depFun((dt1: DataType) => depFun((dt2: DataType) =>
      fun(i => fun(f =>
        inner(f)(i :: (n`.`m`.`o`.`p`.`dt1)) :: (n`.`m`.`o`.`p`.`dt2)
      ))))))))
    }

    val expr = wrap(f => ****(f))
    val golds1 = Seq(
      // 1 loop
      wrap(f => J o *****(f) o S),
      wrap(f => *(J) o *****(f) o *(S)),
      wrap(f => **(J) o *****(f) o **(S)),
      wrap(f => ***(J) o *****(f) o ***(S)),
      // 2 loops
      wrap(f => J o **(J) o ******(f) o **(S) o S),
      wrap(f => *(J) o ***(J) o ******(f) o ***(S) o *(S)),
      wrap(f => **(J) o ****(J) o ******(f) o ****(S) o **(S)),
      // 3 loops
      wrap(f =>
        J o **(J) o ****(J) o
        *(******(f)) o
        ****(S) o **(S) o S),
      wrap(f =>
        *(J) o ***(J) o *****(J) o
        *(******(f)) o
        *****(S) o ***(S) o *(S)),
      // 4 loops
      wrap(f =>
        J o **(J) o ****(J) o ******(J) o
        ****(****(f)) o
        ******(S) o ****(S) o **(S) o S),
    )
    val golds2 = Seq(
      // 1 loop
      wrap(f => J o *****(f) o S),
      wrap(f => *(J) o *****(f) o *(S)),
      wrap(f => **(J) o *****(f) o **(S)),
      wrap(f => ***(J) o *****(f) o ***(S)),
      // 2 loops
      wrap(f => J o **(J) o *(T) o ******(f) o *(T) o **(S) o S),
      wrap(f => *(J) o ***(J) o **(T) o ******(f) o **(T) o ***(S) o *(S)),
      wrap(f => **(J) o ****(J) o ***(T) o ******(f) o ***(T) o ****(S) o **(S)),
      // 3 loops
      wrap(f =>
        J o **(J) o ****(J) o
        ***(T) o *(T) o **(T) o
        *(******(f)) o
        **(T) o *(T) o ***(T) o
        ****(S) o **(S) o S),
      wrap(f =>
        *(J) o ***(J) o *****(J) o
        ****(T) o **(T) o ***(T) o
        *(******(f)) o
        ***(T) o **(T) o ****(T) o
        *****(S) o ***(S) o *(S)),
      // 4 loops
      wrap(f =>
        J o **(J) o ****(J) o ******(J) o
        *****(T) o ***(T) o ****(T) o *(T) o **(T) o ***(T) o
        ****(****(f)) o
        ***(T) o **(T) o *(T) o ****(T) o ***(T) o *****(T) o
        ******(S) o ****(S) o **(S) o S),
    )

    ProveEquiv.init()
      .withFilter(ArrayDimensionPredicate(8) && ASTSizePredicate(200))
      .runBENF(expr, golds1, splitJoinRulesBENF)

    ProveEquiv.init()
      .withFilter(ASTSizePredicate(300))
      .runBENF(golds1, golds2, reorderRulesBENF)
  }

  def main(args: Array[String]): Unit = {
    val (time2D, _) = util.time(run2D())
    val (time3D, _) = util.time(run3D())
    val (time4D, _) = util.time(run4D())
    // ~25s search on i7 desktop with CNF
    // ~1s search on laptop with BENF
    println(s"total 2D time: ${util.prettyTime(time2D)}")
    // ~30s search on laptop with BENF
    // ~1s search on laptop with two-step BENF
    println(s"total 3D time: ${util.prettyTime(time3D)}")
    // ~47s search on laptop with two-step BENF
    println(s"total 4D time: ${util.prettyTime(time4D)}")
  }
}
