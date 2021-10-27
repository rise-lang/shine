package rise.eqsat

object LoweringSearch {
  def init(): LoweringSearch = new LoweringSearch(
    filter = StandardConstraintsPredicate
  )
}

// TODO: enable giving a sketch, maybe merge with GuidedSearch?
class LoweringSearch(var filter: Predicate) {
  private def topLevelAnnotation(e: Expr): BeamExtractRW.TypeAnnotation = {
    import RWAnnotationDSL._
    e.node match {
      case NatLambda(e) => nFunT(topLevelAnnotation(e))
      case DataLambda(e) => dtFunT(topLevelAnnotation(e))
      case Lambda(e) => read ->: topLevelAnnotation(e)
      case _ =>
        assert(e.t.node.isInstanceOf[DataTypeNode[_, _]])
        write
    }
  }

  def run(normalForm: NF,
          costFunction: CostFunction[_],
          startBeam: Seq[Expr],
          loweringRules: Seq[Rewrite]): Option[Expr] = {
    println("---- lowering")
    val egraph = EGraph.empty()
    val normBeam = startBeam.map(normalForm.normalize)

    val expectedAnnotation = topLevelAnnotation(normBeam.head)

    val rootId = normBeam.map(egraph.addExpr)
      .reduce[EClassId] { case (a, b) => egraph.union(a, b)._1 }
    egraph.rebuild(Seq(rootId))

    val r = Runner.init()
      .withTimeLimit(java.time.Duration.ofMinutes(1))
      .withMemoryLimit(4L * 1024L * 1024L * 1024L /* 4GiB */)
      .withNodeLimit(1_000_000)
      .run(egraph, filter, loweringRules, normalForm.directedRules, Seq(rootId))
    r.printReport()

    util.printTime("lowered extraction time", {
      val tmp = Analyser.init(egraph, BeamExtractRW(1, costFunction)).data(rootId)
      tmp.get((expectedAnnotation, Map.empty))
        .map { beam => ExprWithHashCons.expr(egraph)(beam.head._2) }
    })
  }
}
