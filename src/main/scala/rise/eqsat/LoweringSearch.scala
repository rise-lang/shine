package rise.eqsat

object LoweringSearch {
  def init(): LoweringSearch = new LoweringSearch(
    filter = StandardConstraintsPredicate
  )
}

// TODO: enable giving a sketch, maybe merge with GuidedSearch?
class LoweringSearch(var filter: Predicate) {
  private def topLevelAnnotation(t: Type): BeamExtractRW.TypeAnnotation = {
    import RWAnnotationDSL._
    t.node match {
      case NatFunType(t) => nFunT(topLevelAnnotation(t))
      case DataFunType(t) => dtFunT(topLevelAnnotation(t))
      case AddrFunType(t) => aFunT(topLevelAnnotation(t))
      case FunType(ta, tb) =>
        if (!ta.node.isInstanceOf[DataTypeNode[_, _]]) {
          throw new Exception("top level higher-order functions are not supported")
        }
        read ->: topLevelAnnotation(tb)
      case _: DataTypeNode[_, _] =>
        write
      case _ =>
        throw new Exception(s"did not expect type $t")
    }
  }

  private def topLevelSubtype(
    aAnnot: (BeamExtractRW.TypeAnnotation, Map[Int, BeamExtractRW.TypeAnnotation]),
    bAnnot: (BeamExtractRW.TypeAnnotation, Map[Int, BeamExtractRW.TypeAnnotation]),
    typ: TypeId,
    egraph: EGraph,
  ) = {
    val (aOut, aIns) = aAnnot
    val (bOut, bIns) = bAnnot
    BeamExtractRW.subtype(aOut, typ, bOut, typ, egraph) &&
    aIns == bIns // TODO: could use subtype here as well in contravariant fashion
  }

  def run[Cost](
    normalForm: NF,
    costFunction: CostFunction[Cost],
    startBeam: Seq[Expr],
    loweringRules: Seq[Rewrite],
    annotations: Option[(BeamExtractRW.TypeAnnotation, Map[Int, BeamExtractRW.TypeAnnotation])] = None
  ): Option[Expr] = {
    println("---- lowering")
    val egraph = EGraph.empty()
    val normBeam = startBeam.map(normalForm.normalize)
    println(s"normalized: $normBeam")

    val expectedAnnotations = annotations match {
      case Some(annotations) => annotations
      case None => (topLevelAnnotation(normBeam.head.t), Map.empty[Int, BeamExtractRW.TypeAnnotation])
    }

    val rootId = normBeam.map(egraph.addExpr)
      .reduce[EClassId] { case (a, b) => egraph.union(a, b)._1 }
    egraph.rebuild(Seq(rootId))

    val r = Runner.init()
      .withTimeLimit(java.time.Duration.ofMinutes(1))
      .withMemoryLimit(4L * 1024L * 1024L * 1024L /* 4GiB */)
      .withNodeLimit(1_000_000)
      .run(egraph, filter, loweringRules, Seq()/*normalForm.directedRules*/, Seq(rootId))
    r.printReport()

    util.printTime("lowered extraction time", {
      val analysisResult = Analysis.oneShot(BeamExtractRW(1, costFunction), egraph)(egraph.find(rootId))
      // : Map[
      //   (BeamExtractRW.TypeAnnotation, Map[Int,BeamExtractRW.TypeAnnotation]),
      //   Seq[(Cost, ExprWithHashCons)]]

      println("analysisResult", analysisResult)
      println("expectedAnnotations", expectedAnnotations)
      val validResults = analysisResult
        .map { case (foundAnnot, foundBeam) => (foundAnnot, foundBeam.head) }
        // first, filter correct subtypes on annotations
        .filter { case (foundAnnot, found) =>
          topLevelSubtype(foundAnnot, expectedAnnotations, found._2.t, egraph)
        }
      println("validResults", validResults)
      validResults
        // then, get the best option
        .minByOption { case (_, found) => found._1 }(costFunction.ordering)
        .map { case (_, found) => ExprWithHashCons.expr(egraph)(found._2) }
      /* without taking subtyping into account:
      tmp.get(expectedAnnotations)
        .map { beam => ExprWithHashCons.expr(egraph)(beam.head._2) }
      */
    })
  }
}
