package exploration.search

import elevate.core
import elevate.core.strategies.traversal
import elevate.core.strategies.traversal.{alltd, bottomup, oncetd}
import elevate.core.{Failure, Strategy, Success}
import elevate.heuristic_search.ProblemConstraints
import elevate.rise.rules.lowering.mapSeq
import elevate.rise.{Rise, rules}
import elevate.rise.rules.traversal.LiftTraversable
import elevate.rise.strategies.normalForm.RNF
import elevate.rise.strategies.normalForm.CNF
import elevate.core.strategies.basic.{`try`, normalize, repeat, repeatNTimes}
import opencl.executor.Executor
import util.{Execute, gen}

import scala.annotation.meta

//simple mockup search class
class MockupSearch extends ProblemConstraints[Rise] {

  var value = 12
  //  val traversals = Seq(
//    traversal.oncetd[Rise](core.Strategy[Rise]),
//    traversal.alltd[Rise],
//    traversal.topdown[Rise],
//    traversal.bottomup[Rise]
//  )
  val strategies = Seq(
    rules.algorithmic.splitJoin(8),
//    rules.algorithmic.mapLastFission,
//    rules.algorithmic.mapFusion,
//    rules.algorithmic.liftId,
//    rules.algorithmic.idAfter,
//    rules.algorithmic.createTransposePair,
//    rules.algorithmic.removeTransposePair,
//    rules.algorithmic.slideSeqFusion,
//    rules.movement.joinBeforeJoin,
//    rules.movement.joinBeforeMapF,
//    rules.movement.joinBeforeTranspose,
//    rules.movement.mapFBeforeSlide,
//    rules.movement.mapJoinBeforeJoin,
//    rules.movement.mapJoinBeforeTranspose,
//    rules.movement.mapTransposeBeforeJoin,
//    rules.movement.transposeBeforeSlide,
//    rules.movement.transposeBeforeMapMapF,
//    rules.movement.transposeBeforeMapJoin,
//    rules.movement.transposeBeforeMapSlide,
//    rules.betaReduction,
//    rules.etaAbstraction,
//    rules.etaReduction,

//  rules.inferRise,
//  rules.traversal.LiftTraversable,
//  rules.traversal.argument,
//  rules.traversal.argumentOf,
//  rules.traversal.body,
//  rules.traversal.function
  )

  def N(solution:Rise):Set[Rise] = {
    val neighbours = scala.collection.mutable.Set[Rise]()

    val rewrite = RNF

    val rnf  = rewrite.apply(solution)
    //try strategies and add to neighbourhood set
    strategies.foreach(strategy  => {
        try {
          print("try: " + strategy)
          val result = oncetd(strategy).apply(solution)
          print(" - no error")
          //check success/failure
          if (result.isInstanceOf[Success[Rise]]) {
            println(" - success")
//            println("result: " + result.get)
            neighbours.add(result.get)
            println("")
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
    //codegen normal form (CNF)
    val cnf = CNF.apply(solution)

    //lower CNF
    val lowered = alltd(mapSeq).apply(cnf)

    //execute
    val performanceValue = executeC(lowered)
    performanceValue
  }

}

