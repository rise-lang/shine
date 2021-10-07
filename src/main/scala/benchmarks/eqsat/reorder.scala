package benchmarks.eqsat

import rise.eqsat.{ASTSizePredicate, BENF, CNF, ProveEquiv, Rewrite, rules}
import ProveEquiv.syntax._
import rise.core.Expr
import rise.core.DSL._
import rise.core.DSL.Type._
import rise.core.types._

object reorder {
  private def whenMapF(astSize: Int, rw: Rewrite): Rewrite = {
    import rise.eqsat._
    new Rewrite(rw.name, rw.searcher, ConditionalApplier(
      { case (egraph, _, shc, subst) =>
        val id = egraph.findMut(subst(PatternVar(0), shc))
        val freeOf = egraph.getAnalysis(FreeAnalysis)
        val smallestOf = egraph.getAnalysis(SmallestSizeAnalysis)
        freeOf(id).free.contains(0) && smallestOf(id)._2 == astSize
        /*
        subst(PatternVar(0), shc) ==
        egraph.add(Var(0), egraph.addType(Type(FunType(Type(DataTypeVar(1)), Type(DataTypeVar(0))))))
         */
      },
      Set("f"),
      (Set(FreeAnalysis, SmallestSizeAnalysis), Set()),
      rw.applier))
  }

  private val reorderRulesCNF = Seq(
    // rules.combinatory.compositionAssoc1,
    // rules.combinatory.compositionAssoc2,
    // rules.combinatory.compositionIntro,
    // rules.combinatory.compositionLeftId,
    // rules.combinatory.compositionRightId,
    // rules.combinatory.mapFusion,
    // rules.combinatory.mapFusion2,
    // rules.combinatory.mapFission,
    // rules.combinatory.transposePairAfter,
    // rules.combinatory.mapMapFBeforeTranspose,
    // rules.combinatory.mapMapFBeforeTranspose1,
    rules.combinatory.transposeAroundMapMapF,
    rules.combinatory.transposeAroundMapMapF1M,
    rules.combinatory.transposeAroundMapMapF2M,
  )

  private val reorderRulesCNF_2 = Seq(
    rules.combinatory.transposeAroundMapMapF,
    rules.combinatory.mapFission,
  )

  private val reorderRulesBENF = Seq(
    rules.transposeAroundMapMapF,
    rules.transposeAroundMapMapF1M,
    rules.transposeAroundMapMapF2M,
  )

  private def T: ToBeTyped[Expr] = rise.core.primitives.transpose
  private def *(x: ToBeTyped[Expr]): ToBeTyped[Expr] = rise.core.primitives.map(x)
  private def **(x: ToBeTyped[Expr]): ToBeTyped[Expr] = *(*(x))
  private def ***(x: ToBeTyped[Expr]): ToBeTyped[Expr] = *(**(x))
  private def ****(x: ToBeTyped[Expr]): ToBeTyped[Expr] = *(***(x))

  private object dim3 {
    def wrap(inner: ToBeTyped[Expr] => ToBeTyped[Expr]): Expr = {
      depFun((n: Nat) => depFun((m: Nat) => depFun((o: Nat) =>
      depFun((dt1: DataType) => depFun((dt2: DataType) =>
      fun(i => fun(f =>
        inner(f)(i :: (n`.`m`.`o`.`dt1)) :: (n`.`m`.`o`.`dt2)
      )))))))
    }

    val expr = wrap(f => ***(f))

    def equiv(run: (Expr, Seq[Expr]) => Unit): Unit = {
      val gold132 = wrap(f => *(T) o ***(f) o *(T))
      val gold213 = wrap(f => T o ***(f) o T)
      // all below should be implied by gold132 and gold213 modulo associativity and types
      val gold231 = wrap(f => T o *(T) o ***(f) o *(T) o T)
      val gold321 = wrap(f => *(T) o T o *(T) o ***(f) o *(T) o T o *(T))
      val gold312 = wrap(f => *(T) o T o ***(f) o T o *(T))

      run(expr, Seq(/* gold132, gold213, gold231, */ gold321 /*, gold312 */))
    }

    def guidedSearch(normalForm: rise.eqsat.NF, rewriteRules: Seq[Rewrite]): () = {
      import rise.eqsat._
      import rise.eqsat.ExtendedPatternDSL._

      def *(n: NatPattern, f: ExtendedPattern): ExtendedPattern =
        app(map :: `?t` ->: (n`.``?dt`) ->: `?t`, f)

      val steps = Seq(
        GuidedSearch.Step.init(normalForm) withRules rewriteRules withSketch
          nLam(nLam(nLam(dtLam(dtLam(lam(lam(
            contains(
              *(`%n`(0),
                *(`%n`(1),
                  *(`%n`(2), %(0)))))
          )))))))
      )

      val res = GuidedSearch.init()
        .withFilter(ASTSizePredicate(60))
        .run(expr, steps)
      assert(res.exprs.nonEmpty)
      println(Expr.toNamed(res.exprs.head))
    }
  }

  private object dim4 {
    def wrap(inner: ToBeTyped[Expr] => ToBeTyped[Expr] => ToBeTyped[Expr]): ToBeTyped[Expr] = {
      depFun((n: Nat) => depFun((m: Nat) => depFun((o: Nat) => depFun((p: Nat) =>
      depFun((dt1: DataType) => depFun((dt2: DataType) =>
      fun(i => fun(f =>
        inner(i :: (n`.`m`.`o`.`p`.`dt1))(f) :: (n`.`m`.`o`.`p`.`dt2)
      ))))))))
    }

    val expr: Expr = wrap(i => f => ****(f) $ i)

    def equiv(run: (Expr, Seq[Expr]) => Unit): Unit = {
      val gold1243: Expr = wrap(i => f => (**(T) o ****(f) o **(T)) $ i)
      val gold1324: Expr = wrap(i => f => (*(T) o ****(f) o *(T)) $ i)
      val gold2134: Expr = wrap(i => f => (T o ****(f) o T) $ i)
      // should be implied by above goals modulo associativity and types
      val gold4321: Expr = wrap(i => f => (**(T) o *(T) o T o **(T) o *(T) o **(T) o ****(f) o
        **(T) o *(T) o **(T) o T o *(T) o **(T)) $ i)

      run(expr, Seq(/* gold1243, gold1324, gold2134, */ gold4321))
    }

    def guidedSearch(normalForm: rise.eqsat.NF, rewriteRules: Seq[Rewrite]): () = {
      import rise.eqsat._
      import rise.eqsat.ExtendedPatternDSL._

      def *(n: NatPattern, f: ExtendedPattern): ExtendedPattern =
        app(map :: `?t` ->: (n`.``?dt`) ->: `?t`, f)

      val steps = Seq(
        GuidedSearch.Step.init(normalForm) withRules rewriteRules withSketch
          nLam(nLam(nLam(nLam(dtLam(dtLam(lam(lam(
            contains(
              *(`%n`(0),
                *(`%n`(1),
                  *(`%n`(2),
                    *(`%n`(3), %(0))))))
          ))))))))
      )

      val res = GuidedSearch.init()
        .withFilter(ASTSizePredicate(80))
        .run(expr, steps)
      assert(res.exprs.nonEmpty)
      println(Expr.toNamed(res.exprs.head))
    }
  }

  def main(args: Array[String]): Unit = {
    val (time3D_CNF_WF, _) = util.time(dim3.equiv { case (e, g) =>
      ProveEquiv.init()
        .withFilter(ASTSizePredicate(60))
        .runCNF(e, g, Seq(
          // rules.combinatory.compositionAssoc2,
          whenMapF(3, rules.combinatory.transposeAroundMapMapF),
          whenMapF(1, rules.combinatory.transposeAroundMapMapF1M)
        ))
    })

    val (time3D_CNF, _) = util.time(dim3.equiv { case (e, g) =>
      ProveEquiv.init()
        .withFilter(ASTSizePredicate(60))
        .runCNF(e, g, reorderRulesCNF)
    })

    val (time3D_BENF, _) = util.time(dim3.equiv { case (e, g) =>
      ProveEquiv.init()
        .withFilter(ASTSizePredicate(60))
        .runBENF(e, g, reorderRulesBENF)
    })

    val (time4D_CNF_WF, _) = util.time(dim4.equiv { case (e, g) =>
      ProveEquiv.init()
        .withFilter(ASTSizePredicate(80))
        .runCNF(e, g, Seq(
          // rules.combinatory.compositionAssoc2,
          whenMapF(5, rules.combinatory.transposeAroundMapMapF),
          whenMapF(3, rules.combinatory.transposeAroundMapMapF1M),
          whenMapF(1, rules.combinatory.transposeAroundMapMapF2M)
        ))
    })

    val (time4D_CNF, _) = util.time(dim4.equiv { case (e, g) =>
      ProveEquiv.init()
        .withFilter(ASTSizePredicate(80))
        .runCNF(e, g, reorderRulesCNF)
    })

    val (time4D_BENF, _) = util.time(dim4.equiv { case (e, g) =>
      ProveEquiv.init()
        .withFilter(ASTSizePredicate(80))
        .runBENF(e, g, reorderRulesBENF)
    })

    val (time3D_GS_CNF, _) = util.time(dim3.guidedSearch(CNF, reorderRulesCNF))
    // val (time3D_GS_CNF_2, _) = util.time(dim3.guidedSearch(CNF, reorderRulesCNF_2))
    val (time3D_GS_BENF, _) = util.time(dim3.guidedSearch(BENF, reorderRulesBENF))
    val (time4D_GS_CNF, _) = util.time(dim4.guidedSearch(CNF, reorderRulesCNF))
    // val (time4D_GS_CNF_2, _) = util.time(dim4.guidedSearch(CNF, reorderRulesCNF_2))
    val (time4D_GS_BENF, _) = util.time(dim4.guidedSearch(BENF, reorderRulesBENF))

    // ~1ms search in untyped egg prototype
    // ~4s search on i7 desktop
    // ~1s search on i7 desktop with ast size filter
    // ~0.1s search on i7 desktop with specialized rules
    println(s"prove equiv 3D (CNF WF): ${util.prettyTime(time3D_CNF_WF)}")
    println(s"prove equiv 3D (CNF): ${util.prettyTime(time3D_CNF)}")
    println(s"prove equiv 3D (BENF): ${util.prettyTime(time3D_BENF)}")
    println(s"prove equiv 4D (CNF WF): ${util.prettyTime(time4D_CNF_WF)}")
    println(s"prove equiv 4D (CNF): ${util.prettyTime(time4D_CNF)}")
    println(s"prove equiv 4D (BENF): ${util.prettyTime(time4D_BENF)}")

    println(s"guided search 3D (CNF): ${util.prettyTime(time3D_GS_CNF)}")
    // println(s"guided search 3D (CNF 2): ${util.prettyTime(time3D_GS_CNF_2)}")
    println(s"guided search 3D (BENF): ${util.prettyTime(time3D_GS_BENF)}")
    println(s"guided search 4D (CNF): ${util.prettyTime(time4D_GS_CNF)}")
    // println(s"guided search 4D (CNF 2): ${util.prettyTime(time4D_GS_CNF_2)}")
    println(s"guided search 4D (BENF): ${util.prettyTime(time4D_GS_BENF)}")

    // last run on laptop:
    // prove equiv 3D (CNF WF): 818ms 96µs
    // prove equiv 3D (CNF): 204ms 234µs
    // prove equiv 3D (BENF): 92ms 660µs
    // prove equiv 4D (CNF WF): 16s 188ms 388µs
    // prove equiv 4D (CNF): 26s 368ms 54µs
    // prove equiv 4D (BENF): 286ms 349µs
    // guided search 3D (CNF): 110ms 44µs
    // guided search 3D (BENF): 24ms 910µs
    // guided search 4D (CNF): 764ms 326µs
    // guided search 4D (BENF): 481ms 819µs
  }
}
