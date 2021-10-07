package benchmarks.eqsat

import rise.core.DSL.Type._
import rise.core.DSL._
import rise.core.Expr
import rise.core.types._
import rise.eqsat.{ASTSizePredicate, ArrayDimensionPredicate, BENF, CNF, FreeAnalysis, ProveEquiv, Rewrite, StandardConstraintsPredicate, rules}
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

  private val splitRulesCNF = Seq(
    rules.combinatory.splitJoin(tileSize),
    rules.combinatory.splitJoin1M(tileSize),
    rules.combinatory.splitJoin2M(tileSize),
    rules.combinatory.splitJoin3M(tileSize),
    rules.combinatory.splitJoin4M(tileSize),
    rules.combinatory.splitJoin5M(tileSize),
    rules.combinatory.splitJoin6M(tileSize),
  )

  private val reorderRulesCNF = Seq(
    rules.combinatory.transposeAroundMapMapF,
    rules.combinatory.transposeAroundMapMapF1M,
    rules.combinatory.transposeAroundMapMapF2M,
    rules.combinatory.transposeAroundMapMapF3M,
    rules.combinatory.transposeAroundMapMapF4M,
    rules.combinatory.transposeAroundMapMapF5M,
  )

  private val tilingRulesCNF = splitRulesCNF ++ reorderRulesCNF

  private val splitRulesBENF = Seq(
    rules.splitJoin(tileSize),
    rules.splitJoin1M(tileSize),
    rules.splitJoin2M(tileSize),
    rules.splitJoin3M(tileSize),
    rules.splitJoin4M(tileSize),
    rules.splitJoin5M(tileSize),
    rules.splitJoin6M(tileSize),
  )

  private val reorderRulesBENF = Seq(
    rules.transposeAroundMapMapF,
    rules.transposeAroundMapMapF1M,
    rules.transposeAroundMapMapF2M,
    rules.transposeAroundMapMapF3M,
    rules.transposeAroundMapMapF4M,
    rules.transposeAroundMapMapF5M,
  )

  private val tilingRulesBENF = reorderRulesBENF ++ splitRulesBENF

  private def T: ToBeTyped[Expr] = rise.core.primitives.transpose
  private def S: ToBeTyped[Expr] = rise.core.primitives.split(tileSize)
  private def J: ToBeTyped[Expr] = rise.core.primitives.join
  private def *(x: ToBeTyped[Expr]): ToBeTyped[Expr] = rise.core.primitives.map(x)
  private def **(x: ToBeTyped[Expr]): ToBeTyped[Expr] = *(*(x))
  private def ***(x: ToBeTyped[Expr]): ToBeTyped[Expr] = *(**(x))
  private def ****(x: ToBeTyped[Expr]): ToBeTyped[Expr] = *(***(x))
  private def *****(x: ToBeTyped[Expr]): ToBeTyped[Expr] = *(****(x))
  private def ******(x: ToBeTyped[Expr]): ToBeTyped[Expr] = *(*****(x))

  private object dim2 {
    def wrap(inner: ToBeTyped[Expr] => ToBeTyped[Expr]): Expr = {
      depFun((n: Nat) => depFun((m: Nat) =>
      depFun((dt1: DataType) => depFun((dt2: DataType) =>
      fun(i => fun(f =>
        inner(f)(i :: (n`.`m`.`dt1)) :: (n`.`m`.`dt2)
      ))))))
    }

    val expr = wrap(f => **(f))

    def equiv(): () = {
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
        .withFilter(ArrayDimensionPredicate(4) && ASTSizePredicate(50) && StandardConstraintsPredicate)
        .runBENF(expr, golds1, splitRulesBENF)

      ProveEquiv.init()
        .withFilter(ASTSizePredicate(50))
        .runBENF(golds1, golds2, reorderRulesBENF)
    }

    def guidedSearch(normalForm: rise.eqsat.NF, splitRules: Seq[Rewrite], reorderRules: Seq[Rewrite]): () = {
      import rise.eqsat._
      import rise.eqsat.ExtendedPatternDSL._

      def *(n: NatPattern, f: ExtendedPattern): ExtendedPattern =
        app(map :: `?t` ->: (n`.``?dt`) ->: `?t`, f)

      val steps = Seq(
        GuidedSearch.Step.init(normalForm) withRules splitRules withSketch
          nLam(nLam(dtLam(dtLam(lam(lam(
            contains(
              *(`%n`(1) /^ cst(tileSize),
                *(cst(tileSize),
                  *(`%n`(0) /^ cst(tileSize),
                    *(cst(tileSize), %(0))))))
          )))))),
        GuidedSearch.Step.init(normalForm) withRules reorderRules withSketch
          nLam(nLam(dtLam(dtLam(lam(lam(
            contains(
              *(`%n`(1) /^ cst(tileSize),
                *(`%n`(0) /^ cst(tileSize),
                  *(cst(tileSize),
                    *(cst(tileSize), %(0))))))
          ))))))
      )

      val res = GuidedSearch.init()
        .withFilter(ArrayDimensionPredicate(4) && ASTSizePredicate(50) && StandardConstraintsPredicate)
        .run(expr, steps)
      assert(res.exprs.nonEmpty)
      println(Expr.toNamed(res.exprs.head))
    }
  }

  private object dim3 {
    def wrap(inner: ToBeTyped[Expr] => ToBeTyped[Expr]): Expr = {
      depFun((n: Nat) => depFun((m: Nat) => depFun((o: Nat) =>
      depFun((dt1: DataType) => depFun((dt2: DataType) =>
      fun(i => fun(f =>
        inner(f)(i :: (n`.`m`.`o`.`dt1)) :: (n`.`m`.`o`.`dt2)
      )))))))
    }

    val expr = wrap(f => ***(f))

    def equiv(): () = {
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
        .withFilter(ArrayDimensionPredicate(6) && ASTSizePredicate(100) && StandardConstraintsPredicate)
        .runBENF(expr, golds1, splitRulesBENF)

      ProveEquiv.init()
        .withFilter(ASTSizePredicate(100))
        .runBENF(golds1, golds2, reorderRulesBENF)
    }

    def guidedSearch(normalForm: rise.eqsat.NF, splitRules: Seq[Rewrite], reorderRules: Seq[Rewrite]): () = {
      import rise.eqsat._
      import rise.eqsat.ExtendedPatternDSL._

      def *(n: NatPattern, f: ExtendedPattern): ExtendedPattern =
        app(map :: `?t` ->: (n`.``?dt`) ->: `?t`, f)

      val steps = Seq(
        GuidedSearch.Step.init(normalForm) withRules splitRules withSketch
          nLam(nLam(nLam(dtLam(dtLam(lam(lam(
            contains(
              *(`%n`(2) /^ cst(tileSize),
                *(cst(tileSize),
                  *(`%n`(1) /^ cst(tileSize),
                    *(cst(tileSize),
                      *(`%n`(0) /^ cst(tileSize),
                        *(cst(tileSize), %(0))))))))
          ))))))),
        GuidedSearch.Step.init(normalForm) withRules reorderRules withSketch
          nLam(nLam(nLam(dtLam(dtLam(lam(lam(
            contains(
              *(`%n`(2) /^ cst(tileSize),
                *(`%n`(1) /^ cst(tileSize),
                  *(`%n`(0) /^ cst(tileSize),
                    *(cst(tileSize),
                      *(cst(tileSize),
                        *(cst(tileSize), %(0))))))))
          )))))))
      )

      val res = GuidedSearch.init()
        .withFilter(ArrayDimensionPredicate(6) && ASTSizePredicate(100) && StandardConstraintsPredicate)
        .run(expr, steps)
      assert(res.exprs.nonEmpty)
      println(Expr.toNamed(res.exprs.head))
    }
  }

  private object dim4 {
    def wrap(inner: ToBeTyped[Expr] => ToBeTyped[Expr]): Expr = {
      depFun((n: Nat) => depFun((m: Nat) => depFun((o: Nat) => depFun((p: Nat) =>
      depFun((dt1: DataType) => depFun((dt2: DataType) =>
      fun(i => fun(f =>
        inner(f)(i :: (n`.`m`.`o`.`p`.`dt1)) :: (n`.`m`.`o`.`p`.`dt2)
      ))))))))
    }

    val expr = wrap(f => ****(f))

    def equiv(): () = {
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
        .withFilter(ArrayDimensionPredicate(8) && ASTSizePredicate(200) && StandardConstraintsPredicate)
        .runBENF(expr, golds1, splitRulesBENF)

      ProveEquiv.init()
        .withFilter(ASTSizePredicate(300))
        .runBENF(golds1, golds2, reorderRulesBENF)
    }

    def guidedSearch(normalForm: rise.eqsat.NF, splitRules: Seq[Rewrite], reorderRules: Seq[Rewrite]): () = {
      import rise.eqsat._
      import rise.eqsat.ExtendedPatternDSL._

      def *(n: NatPattern, f: ExtendedPattern): ExtendedPattern =
        app(map :: `?t` ->: (n`.``?dt`) ->: `?t`, f)

      val steps = Seq(
        GuidedSearch.Step.init(normalForm) withRules splitRules withSketch
          nLam(nLam(nLam(nLam(dtLam(dtLam(lam(lam(
            contains(
              *(`%n`(3) /^ cst(tileSize),
                *(cst(tileSize),
                  *(`%n`(2) /^ cst(tileSize),
                    *(cst(tileSize),
                      *(`%n`(1) /^ cst(tileSize),
                        *(cst(tileSize),
                          *(`%n`(0) /^ cst(tileSize),
                            *(cst(tileSize), %(0))))))))))
          )))))))),
        GuidedSearch.Step.init(normalForm) withRules reorderRules withSketch
          nLam(nLam(nLam(nLam(dtLam(dtLam(lam(lam(
            contains(
              *(`%n`(3) /^ cst(tileSize),
                *(`%n`(2) /^ cst(tileSize),
                  *(`%n`(1) /^ cst(tileSize),
                    *(`%n`(0) /^ cst(tileSize),
                      *(cst(tileSize),
                        *(cst(tileSize),
                          *(cst(tileSize),
                            *(cst(tileSize), %(0))))))))))
          ))))))))
      )

      val res = GuidedSearch.init()
        .withFilter(ArrayDimensionPredicate(8) && ASTSizePredicate(200) && StandardConstraintsPredicate)
        .run(expr, steps)
      assert(res.exprs.nonEmpty)
      println(Expr.toNamed(res.exprs.head))
    }
  }

  def main(args: Array[String]): Unit = {
    val (time2D, _) = util.time(dim2.equiv())
    val (time3D, _) = util.time(dim3.equiv())
    val (time4D, _) = util.time(dim4.equiv())

    val (time2D_GS_CNF, _) = util.time(dim2.guidedSearch(CNF, splitRulesCNF, reorderRulesCNF))
    val (time2D_GS_BENF, _) = util.time(dim2.guidedSearch(BENF, splitRulesBENF, reorderRulesBENF))
    val (time3D_GS_CNF, _) = util.time(dim3.guidedSearch(CNF, splitRulesCNF, reorderRulesCNF))
    val (time3D_GS_BENF, _) = util.time(dim3.guidedSearch(BENF, splitRulesBENF, reorderRulesBENF))
    val (time4D_GS_CNF, _) = util.time(dim4.guidedSearch(CNF, splitRulesCNF, reorderRulesCNF))
    val (time4D_GS_BENF, _) = util.time(dim4.guidedSearch(BENF, splitRulesBENF, reorderRulesBENF))

    // ~25s search on i7 desktop with CNF
    // ~1s search on laptop with BENF
    println(s"prove equiv 2D (BENF): ${util.prettyTime(time2D)}")
    // ~30s search on laptop with BENF
    // ~1s search on laptop with two-step BENF
    println(s"prove equiv 3D (BENF): ${util.prettyTime(time3D)}")
    // ~47s search on laptop with two-step BENF
    println(s"prove equiv 4D (BENF): ${util.prettyTime(time4D)}")

    println(s"guided search 2D (CNF): ${util.prettyTime(time2D_GS_CNF)}")
    println(s"guided search 2D (BENF): ${util.prettyTime(time2D_GS_BENF)}")
    println(s"guided search 3D (CNF): ${util.prettyTime(time3D_GS_CNF)}")
    println(s"guided search 3D (BENF): ${util.prettyTime(time3D_GS_BENF)}")
    println(s"guided search 4D (CNF): ${util.prettyTime(time4D_GS_CNF)}")
    println(s"guided search 4D (BENF): ${util.prettyTime(time4D_GS_BENF)}")

    // last run on laptop:
    // prove equiv 2D (BENF): 1s 53ms 10µs
    // prove equiv 3D (BENF): 1s 474ms 856µs
    // prove equiv 4D (BENF): 49s 828ms 879µs
    // guided search 2D (CNF): 157ms 341µs
    // guided search 2D (BENF): 131ms 853µs
    // guided search 3D (CNF): 1s 255ms 71µs
    // guided search 3D (BENF): 868ms 789µs
    // guided search 4D (CNF): 1mn 15s 323ms 619µs
    // guided search 4D (BENF): 22s 156ms 414µs
  }
}
