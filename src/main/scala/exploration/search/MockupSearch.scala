package exploration.search

import elevate.core.strategies.traversal.{alltd, oncetd}
import elevate.core.{Strategy, Success}
import elevate.heuristic_search.ProblemConstraints
import elevate.rise.rules.lowering.{mapSeq, reduceSeq}
import elevate.rise.{Rise, rules, strategies}
import elevate.rise.rules.traversal.LiftTraversable
import elevate.rise.strategies.normalForm.CNF
import elevate.rise.strategies.tiling

//simple mockup search class
class MockupSearch(val lowering: Strategy[Rise] = CNF `;` alltd(reduceSeq) `;` alltd(mapSeq))
  extends ProblemConstraints[Rise] {

  var value = 12
  //  val traversals = Seq(
//    traversal.oncetd[Rise](core.Strategy[Rise]),
//    traversal.alltd[Rise],
//    traversal.topdown[Rise],
//    traversal.bottomup[Rise]
//  )
  val strategies = Seq(
    rules.algorithmic.splitJoin(8),
    rules.algorithmic.mapLastFission,
    rules.algorithmic.mapFusion,
    rules.algorithmic.liftId,
    rules.algorithmic.idAfter,
    rules.algorithmic.createTransposePair,
    rules.algorithmic.removeTransposePair,
    rules.algorithmic.slideSeqFusion,
    rules.movement.joinBeforeJoin,
    rules.movement.joinBeforeMapF,
    rules.movement.joinBeforeTranspose,
    rules.movement.mapFBeforeSlide,
    rules.movement.mapJoinBeforeJoin,
    rules.movement.mapJoinBeforeTranspose,
    rules.movement.mapTransposeBeforeJoin,
    rules.movement.transposeBeforeSlide,
    rules.movement.transposeBeforeMapMapF,
    rules.movement.transposeBeforeMapJoin,
    tiling.loopInterchange,
    tiling.tileND(32)(32),

    rules.betaReduction,
    rules.etaAbstraction,
    rules.etaReduction

//  rules.inferRise,
//  rules.traversal.LiftTraversable,
//  rules.traversal.argument,
//  rules.traversal.argumentOf,
//  rules.traversal.body,
//  rules.traversal.function
  )

  def N(solution:Rise):Set[Rise] = {
    val neighbours = scala.collection.mutable.Set[Rise]()

    //try strategies and add to neighbourhood set
    strategies.foreach(strategy  => {
        try {
//          print("try: " + strategy)
          val result = alltd(strategy).apply(solution)
          //check success/failure
          if (result.isInstanceOf[Success[Rise]]) {
            println(" - success")
            neighbours.add(result.get)
          }else{
            println(" - failure")
          }
        }catch{
          case e:Throwable => {
            print(" - error: " + e +  "\n")
          }
        }
      })

    //add id to neighbourhood
    neighbours.add(solution)

    neighbours.toSet
  }

  def f(solution:Rise):Double = {
    println("lowering strategy: " + lowering)
    //codegen normal form (CNF)
    val cnf = CNF.apply(solution)

    //lower CNF
    var lowered = alltd(mapSeq).apply(cnf)
    lowered = alltd(reduceSeq).apply(lowered)

    //execute
    val performanceValue = executeC(lowered)
    performanceValue
  }

}

