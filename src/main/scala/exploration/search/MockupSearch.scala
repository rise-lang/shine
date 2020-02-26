package exploration.search

import elevate.core.strategies.traversal.{alltd, bottomup, oncetd, topdown}
import elevate.core.{Strategy, Success}
import elevate.heuristic_search.ProblemConstraints
import elevate.rise.rules.lowering.{mapSeq, reduceSeq}
import elevate.rise.{Rise, rules, strategies}
import elevate.rise.rules.traversal.LiftTraversable
import elevate.rise.strategies.normalForm.{CNF, RNF}
import elevate.rise.strategies.{tiling, traversal}

//simple mockup search class
class MockupSearch(val lowering: Strategy[Rise] = CNF `;` alltd(reduceSeq) `;` alltd(mapSeq))
  extends ProblemConstraints[Rise] {

  var value = 12
  val traversalss = Seq(
    elevate.core.strategies.traversal.oncetd,
    elevate.core.strategies.traversal.alltd,
    elevate.core.strategies.traversal.topdown,
    elevate.core.strategies.traversal.bottomup
  )
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
//    tiling.tileND(32)(32),
//
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
          print("try: " + strategy)
          val resultOncetd = (RNF `;` oncetd(strategy))(solution)
          val resultAlltd = (RNF `;` alltd(strategy))(solution)
          val resultTopdown = (RNF `;` topdown(strategy))(solution)
          val resultBottomUp = (RNF `;` bottomup(strategy))(solution)

          //check success/failure
          if (resultOncetd.isInstanceOf[Success[Rise]]) {
            println(" - success")
            neighbours.add(resultOncetd.get)
          }else{
            println(" - failure")
          }
          //check success/failure
          if (resultAlltd.isInstanceOf[Success[Rise]]) {
            println(" - success")
            neighbours.add(resultAlltd.get)
          }else{
            println(" - failure")
          }
          //check success/failure
          if (resultTopdown.isInstanceOf[Success[Rise]]) {
            println(" - success")
            neighbours.add(resultTopdown.get)
          }else{
            println(" - failure")
          }
          //check success/failure
          if (resultBottomUp.isInstanceOf[Success[Rise]]) {
            println(" - success")
            neighbours.add(resultBottomUp.get)
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
    try {
      //codegen normal form (CNF)
      val cnf = CNF.apply(solution)

      //lower CNF
      var lowered = alltd(mapSeq).apply(cnf)
      lowered = alltd(reduceSeq).apply(lowered)

      //execute
      val performanceValue = executeC(lowered)
      performanceValue

    }catch{
      case e: Throwable => {
        println("error: " + e)
        -1
      }
    }
  }

}

