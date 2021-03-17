package rise.eqsat

import PatternDSL._

object rules {
  type Rule = Rewrite[DefaultAnalysisData]

  // TODO: DeBruijn indices shifting
  val mapFission: Rule = Rewrite.init("map-fission",
    app(app(map, ?("f")), app(app(map, ?("g")), ?("arg"))).compile(),
    app(app(map, lam(app(?("f"), app(?("g"), %(0))))), ?("arg")).compile()
  )
}
