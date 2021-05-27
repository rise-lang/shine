package benchmarks.eqsat

import rise.core.DSL.Type._
import rise.core.DSL._
import rise.core.Expr
import rise.core.types._
import rise.eqsat.{ProveEquiv, rules, ArrayDimensionPredicate}

object tiling {
  private val tileSize = 4
  private val tilingRules = Seq(
    rules.combinatory.compositionAssoc1,
    rules.combinatory.compositionAssoc2,
    rules.combinatory.compositionIntro,
    rules.combinatory.compositionLeftId,
    rules.combinatory.compositionRightId,
    rules.combinatory.splitJoin(tileSize),
    rules.combinatory.mapFusion,
    rules.combinatory.mapFission,
    // rules.combinatory.transposePairAfter,
    // rules.combinatory.mapMapFBeforeTranspose,
    rules.combinatory.transposeAroundMapMapF
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
      wrap(f => *(J o **(f) o S)),
      // 2 loops
      wrap(f => J o **(J) o *(T) o ****(f) o *(T) o **(S) o S)
    )

    ProveEquiv.init()
      .withFilter(ArrayDimensionPredicate(4))
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
      .withFilter(ArrayDimensionPredicate(6))
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
      .withFilter(ArrayDimensionPredicate(8))
      .runCNF(expr, golds, tilingRules)
  }

  def main(args: Array[String]): Unit = {
    val (time2D, _) = util.time(run2D())
    // val (time3D, _) = util.time(run3D())
    // val (time4D, _) = util.time(run4D())
    println(s"total 2D time: ${util.prettyTime(time2D)}") // ~25s search on i7 desktop
    // println(s"total 3D time: ${util.prettyTime(time3D)}")
    // println(s"total 4D time: ${util.prettyTime(time4D)}")
  }
}
