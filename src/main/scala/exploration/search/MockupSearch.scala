package exploration.search

import elevate.core.strategies.traversal.{alltd, bottomup, oncebu, oncetd, topdown}
import elevate.core.{Failure, Strategy, Success}
import elevate.heuristic_search.ProblemConstraints
import elevate.rise.rules.algorithmic.{blockedReduce, fissionReduceMap, fuseReduceMap}
import elevate.rise.rules.movement.{liftReduce, mapFBeforeSlide}
import elevate.rise.{Rise, rules}
import elevate.rise.rules.traversal.LiftTraversable
import elevate.rise.strategies.normalForm.{LCNF, RNF}
import elevate.rise.strategies.tiling
import elevate.rise.strategies.tiling.tileNDList

import scala.collection.mutable

// simple mockup search class
class MockupSearch(val lowering: Strategy[Rise] = elevate.rise.rules.lowering.lowerToC)
  extends ProblemConstraints[Rise] {

  // blocking strategy: to be moved!

  // prepare tiling, keep reduce(+) and map(*) close together, (necessary?)
  val fusedReduceMap: Strategy[Rise] = LCNF `;` oncetd(fuseReduceMap)

  // M.N.m.n.K (or in TVM terms: xo,yo,xi,yi,k)
  val tiledOuterTwo: Strategy[Rise] = fusedReduceMap `;`
    oncetd(tileNDList(List(32,32))) `;` LCNF

  // M.N.m.n.K.k (tile K-loop),
  // fission first to enable blocking the reduction loop
  val splitK: Strategy[Rise] = tiledOuterTwo `;`
    oncetd(fissionReduceMap) `;` oncetd(blockedReduce(4)) `;` LCNF

  // move the split (blocking the reduction loop)
  // to prepare fusing map(*) and reduce(+) again
  val prepareFusion: Strategy[Rise] = splitK `;`
    oncetd(mapFBeforeSlide) `;` LCNF

  // move map(*) into both reduce loops again
  val fusedReduceMapAgain: Strategy[Rise] = prepareFusion `;`
    oncetd(fuseReduceMap) `;` LCNF `;` oncetd(fuseReduceMap) `;` LCNF

  // move outer K loop up M.N.m.K.n.k
  val moveOuterKLoopOnce: Strategy[Rise] = fusedReduceMapAgain `;`
    RNF `;` oncetd(liftReduce) `;` LCNF

  // move outer K loop further up M.N.K.m.n.k
  val moveOuterKLoopTwice: Strategy[Rise] = moveOuterKLoopOnce `;`
    RNF `;` oncetd(liftReduce) `;` LCNF

  // move inner K loop further up M.N.K.m.k.n,
  val moveInnerKLoopOnce: Strategy[Rise] = moveOuterKLoopTwice `;`
    RNF `;` oncebu(liftReduce) `;` LCNF

  val blocking : Strategy[Rise] = moveInnerKLoopOnce `;`
    RNF `;` oncebu(liftReduce)

  val traversalss = Seq(
    elevate.core.strategies.traversal.oncetd,
    elevate.core.strategies.traversal.alltd,
    elevate.core.strategies.traversal.topdown,
    elevate.core.strategies.traversal.bottomup
  )

  val strategies = Seq(
    blocking,
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
//
//    rules.betaReduction,
//    rules.etaAbstraction,
//    rules.etaReduction
//
//  rules.inferRise,
//  rules.traversal.LiftTraversable,
//  rules.traversal.argument,
//  rules.traversal.argumentOf,
//  rules.traversal.body,
//  rules.traversal.function
  )

  val solutions = new mutable.HashMap[Int, Double]()

  def N(solution:Rise):Set[Rise] = {
    val neighbours = scala.collection.mutable.Set[Rise]()

    //try strategies and add to neighbourhood set
    strategies.foreach(strategy  => {
        try {
//          print("try: " + strategy)
          val resultOncetd = oncetd(strategy).apply(solution)
          val resultAlltd = alltd(strategy).apply(solution)
          val resultTopdown = topdown(strategy).apply(solution)
          val resultBottomUp = bottomup(strategy).apply(solution)

          //check success/failure
          if (resultOncetd.isInstanceOf[Success[Rise]]) {
//            println(" - success")
            neighbours.add(resultOncetd.get)
          }else{
//            println(" - failure")
          }
//          //check success/failure
          if (resultAlltd.isInstanceOf[Success[Rise]]) {
//            println(" - success")
            neighbours.add(resultAlltd.get)
          }else{
//            println(" - failure")
          }
          //check success/failure
          if (resultTopdown.isInstanceOf[Success[Rise]]) {
//            println(" - success")
            neighbours.add(resultTopdown.get)
          }else{
//            println(" - failure")
          }
          //check success/failure
          if (resultBottomUp.isInstanceOf[Success[Rise]]) {
//            println(" - success")
            neighbours.add(resultBottomUp.get)
          }else{
//            println(" - failure")
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

  // warning: check size of hashmap
  def f(solution:Rise):Double = {
    try {
      if(solutions.get(solution.hashCode()).isDefined){
        //get elements from hashmap and return buffered runtime
        solutions.get(solution.hashCode()).get
      }else {
        //lower
        val lowered = lowering(solution)

        //execute
        val performanceValue = executeC(lowered)

        //add to hashmap
        solutions.+=(solution.hashCode() -> performanceValue)

        performanceValue
      }
    }catch{
      case e: Throwable => {
        println("error: " + e)
        -1
      }
    }
  }

}

