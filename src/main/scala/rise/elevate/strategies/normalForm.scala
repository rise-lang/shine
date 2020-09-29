package rise.elevate.strategies

import elevate.core.Strategy
import elevate.core.strategies.Traversable
import elevate.core.strategies.basic._
import elevate.core.strategies.predicate._
import elevate.core.strategies.traversal.one
import elevate.macros.StrategyMacro.strategy
import rise.core.{primitives => p}
import rise.elevate.Rise
import rise.elevate.rules._
import rise.elevate.rules.algorithmic._
import rise.elevate.rules.traversal.{argumentOf, body, function}
import rise.elevate.strategies.predicate._

// todo think about better names!
object normalForm {

  // Beta-Eta-Normal-Form
  @strategy def BENF()(implicit ev: Traversable[Rise]): Strategy[Rise] =
    normalize(ev)(etaReduction <+ betaReduction)

  // Data-Flow-Normal-Form
  @strategy def DFNF()(implicit ev: Traversable[Rise]): Strategy[Rise] =
    (BENF() `;`
      // there is no argument of a map which is not eta-abstracted, i.e., every argument of a map is a lambda
      normalize(ev)(argumentOf(p.map.primitive, (not(isLambda) `;` etaAbstraction))) `;`
      // a reduce always contains two lambdas declaring y and acc
      normalize(ev)(argumentOf(p.reduce.primitive, (not(isLambda) `;` etaAbstraction))) `;`
      normalize(ev)(argumentOf(p.reduce.primitive, body((not(isLambda) `;` etaAbstraction)))) `;`
      // there is no map(f) without an argument == there is no way to get to a map without visiting two applies
      // same for reduce and three applies
      normalize(ev)(
        one(function(isMap) <+ one(function(isReduce))) `;`                  // there is a map in two hops, i.e, Something(Apply(map, f))
          not(isApply) `;`                                                      // and the current node is not an Apply i.e. Something != Apply
          one((function(isMap) <+ one(function(isReduce))) `;` etaAbstraction)   // eta-abstract the inner Apply
      ))

  // Rewrite-Normal-Form (Fission all maps)
  @strategy def RNF()(implicit ev: Traversable[Rise]): Strategy[Rise] =
    normalize(ev)(DFNF() `;` mapLastFission()) `;` DFNF()

  // Codegen-Normal-Form (Fuse all maps)
  @strategy def CNF()(implicit ev: Traversable[Rise]): Strategy[Rise] =
    normalize(ev)(mapFusion)
}
