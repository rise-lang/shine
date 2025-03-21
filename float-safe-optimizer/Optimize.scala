package float_safe_optimizer

import rise.eqsat._

object Optimize {
  def apply(e: rise.core.Expr): rise.core.Expr = {
    val expr = Expr.fromNamed(e)
    val (body, annotation, wrapBody) = analyseTopLevel(expr)
    println("eqsat expr", expr)

    val rules = {
      import rise.eqsat.rules._
      Seq(
        // implementation choices:
        reduceSeq,
        mapSeq,
        // satisfying read/write annotations:
        mapSeqArray,
        // simplifications:
        // mapFusion,
        // reduceSeqMapFusion,
        removeTransposePair,
        fstReduction,
        sndReduction,
        reduceSeqOne,
      /* maybe:
        omp.mapPar --> need heuristic vs mapSeq
        toMemAfterMapSeq / storeToMem
        storeToMem
        reduceSeqMapFusion
        mapSeqUnroll/reduceSeqUnroll --> need heuristic
        eliminateMapIdentity

        is it worth the cost?:
        betaExtract
        betaNatExtract
        eta

        not generic enough, use Elevate passes or custom applier?:
        idxReduction_i_n
      */
      )
    }

    LoweringSearch.init().run(BENF, Cost, Seq(body), rules, Some(annotation)) match {
      case Some(resBody) =>
        val res = wrapBody(resBody)
        Expr.toNamed(res)
      case None => throw new Exception("could not find valid low-level expression")
    }
  }

  // TODO: this code might be avoidable by making DPIA+codegen rely on top-level type instead of top-level constructs
  def analyseTopLevel(e: Expr)
  : (Expr, (BeamExtractRW.TypeAnnotation, Map[Int, BeamExtractRW.TypeAnnotation]), Expr => Expr) = {
    import rise.eqsat.RWAnnotationDSL._

    // returns (body, argCount, wrapBody)
    def rec(e: Expr): (Expr, Int, Expr => Expr) = {
      e.node match {
        case NatLambda(e2) =>
          val (b, a, w) = rec(e2)
          (b, a, b => Expr(NatLambda(w(b)), e.t))
        case DataLambda(e2) =>
          val (b, a, w) = rec(e2)
          (b, a, b => Expr(DataLambda(w(b)), e.t))
        case Lambda(e2) =>
          e.t.node match {
            case FunType(Type(_: DataTypeNode[_, _]), _) => ()
            case _ => throw new Exception("top level higher-order functions are not supported")
          }
          val (b, a, w) = rec(e2)
          (b, a + 1, b => Expr(Lambda(w(b)), e.t))
        case _ =>
          if (!e.t.node.isInstanceOf[DataTypeNode[_, _]]) {
            throw new Exception("expected body with data type")
          }
          (e, 0, b => b)
      }
    }

    val (b, a, w) = rec(e)
    (b, (write, List.tabulate(a)(i => i -> read).toMap), w)
  }

  object Cost extends CostFunction[Int] {
    val ordering = implicitly

    override def cost(egraph: EGraph, enode: ENode, t: TypeId, costs: EClassId => Int): Int = {
      import rise.core.primitives._

      val nodeCost = enode match {
        // prefer avoiding mapSeq
        case Primitive(mapSeq()) => 5
        case _ => 1
      }
      enode.children().foldLeft(nodeCost) { case (acc, eclass) => acc + costs(eclass) }
    }
  }
}
