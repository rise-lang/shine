package rise.eqsat

object LoweringSearch {
  def init(): LoweringSearch = new LoweringSearch(
    filter = StandardConstraintsPredicate
  )
}

// TODO: enable giving a sketch, maybe merge with GuidedSearch?
class LoweringSearch(var filter: Predicate) {
  def run(normalForm: NF,
          costFunction: CostFunction[_],
          startBeam: Seq[Expr],
          loweringRules: Seq[Rewrite]): Expr = {
    println("---- lowering")
    val egraph = EGraph.empty()
    val rootId = startBeam.map(normalForm.normalize).map(egraph.addExpr)
      .reduce[EClassId] { case (a, b) => egraph.union(a, b)._1 }
    egraph.rebuild(Seq(rootId))

    Runner.init().run(egraph, filter, loweringRules, normalForm.rules, Seq(rootId))

    util.printTime("lowered extraction time", {
      val tmp = Analyser.init(egraph, BeamExtractRW(1, costFunction)).data(rootId)
      val expectedAnnotation = { // TODO: don't hardcode
        import RWAnnotationDSL._
        read ->: read ->: write
      }
      tmp((expectedAnnotation, Map.empty))
        .map { case (_, e) => ExprWithHashCons.expr(egraph)(e) }
        .head
    })
  }
}
