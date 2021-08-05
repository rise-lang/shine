package benchmarks.eqsat

import rise.core.DSL.Type._
import rise.core.DSL._
import rise.core.Expr
import rise.core.types._
import rise.eqsat.{ArrayDimensionPredicate, ASTSizePredicate, ProveEquiv, rules, DefaultAnalysis}
import ProveEquiv.syntax._
import rise.eqsat.PredicateDSL._

object tiling {
  private val tileSize = 4

  private def whenFcontainsF(rw: DefaultAnalysis.Rewrite): DefaultAnalysis.Rewrite =
    new rise.eqsat.Rewrite(rw.name, rw.searcher, rise.eqsat.ConditionalApplier(
      { case (egraph, _, shc, subst) => egraph.getMut(subst(rise.eqsat.PatternVar(0), shc)).data.free.contains(0) },
      Set("f"),
      rw.applier), rw.isDirected)

  private val tilingRules = Seq(
    // rules.combinatory.compositionAssoc1,
    rules.combinatory.compositionAssoc2,//.directed(),
    // rules.combinatory.compositionIntro,
    // rules.combinatory.compositionLeftId,
    // rules.combinatory.compositionRightId,
    whenFcontainsF(rules.combinatory.splitJoin(tileSize)),
    whenFcontainsF(rules.combinatory.splitJoin1M(tileSize)),
    whenFcontainsF(rules.combinatory.splitJoin2M(tileSize)),
    whenFcontainsF(rules.combinatory.splitJoin3M(tileSize)),
    whenFcontainsF(rules.combinatory.splitJoin4M(tileSize)),
    // rules.combinatory.mapFusion,
    // rules.combinatory.mapFission,
    // rules.combinatory.transposePairAfter,
    // rules.combinatory.mapMapFBeforeTranspose,
    whenFcontainsF(rules.combinatory.transposeAroundMapMapF),
    whenFcontainsF(rules.combinatory.transposeAroundMapMapF1M),
    whenFcontainsF(rules.combinatory.transposeAroundMapMapF2M),
    whenFcontainsF(rules.combinatory.transposeAroundMapMapF3M),
  )

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
    val golds = Seq(
      // 1 loop
      wrap(f => J o ***(f) o S),
      wrap(f => *(J) o ***(f) o *(S)),
      // 2 loops
      wrap(f => J o **(J) o *(T) o ****(f) o *(T) o **(S) o S)
    )

    ProveEquiv.init()
      .withFilter(ArrayDimensionPredicate(4) && ASTSizePredicate(50))
      .runCNF(expr, golds, tilingRules)
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
      .withFilter(ArrayDimensionPredicate(6) && ASTSizePredicate(100))
      .runCNF(expr, golds, tilingRules)
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
      .withFilter(ArrayDimensionPredicate(8) && ASTSizePredicate(160))
      .runCNF(expr, golds, tilingRules)
  }

  def main(args: Array[String]): Unit = {
    val (time2D, _) = util.time(run2D())
    val (time3D, _) = util.time(run3D())
    /*val (time4D, _) = util.time(run4D())
    println(s"total 2D time: ${util.prettyTime(time2D)}") // ~25s search on i7 desktop
    println(s"total 3D time: ${util.prettyTime(time3D)}")
    println(s"total 4D time: ${util.prettyTime(time4D)}")*/
  }
}
