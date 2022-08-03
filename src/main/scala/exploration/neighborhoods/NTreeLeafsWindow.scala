package exploration.neighborhoods

import elevate.heuristic_search.HeuristicPanel
import elevate.heuristic_search.util.Solution
import elevate.heuristic_search.Runner
import elevate.core._
import elevate.heuristic_search.util._

import rise.elevate.Rise
import exploration.rewriter.everywhere._


import exploration.runner.checkSolutionC

/**
 * Neighborhood is defined by slide-window around leafs (fill with id)
 * defines order on search space
 * All solutions are on leaf layer
 */
case class NTreeLeafsWindow(
                             runner: Runner[Rise],
                             strategies: Seq[Strategy[Rise]],
                             slideWindow: Int = 10,
                             afterRewrite: Option[Strategy[Rise]] = None,
                             checkExpression: Option[Rise => Boolean] = None
                           ) extends HeuristicPanel[Rise] {

  val solutions = new scala.collection.mutable.HashMap[String, Option[Double]]()

  // todo implement this
  override def N(solution: Solution[Rise]): Seq[Solution[Rise]] = {

    val depth = 6 // 5 rewrites, ergo (eppo) 6 layers

    // left
    var left: scala.collection.mutable.Seq[Solution[Rise]] = scala.collection.mutable.Seq.empty[Solution[Rise]]
    var elem = solution
    var visited = scala.collection.mutable.Seq.empty[Seq[RewriteIdentifier[Rise]]]
    var elementsLeft = true

    while (left.size < slideWindow && elementsLeft) {
      visited = visited :+ elem.rewrites()

      // get parent (go up)
      elem = elem.parent()

      // go down from parent
      var condition = true
      while (condition) {
        // check layer of element
        if (elem.layer() == depth - 1) {

          // generate all children not seen yet and add to left
          val children = checkExpression match {
            case Some(checker) =>
              rewriteFunctionWide(strategies)(elem)
                .filter(elem => solutionSmaller(strategies, solutionA = elem, solutionB = solution))
                .filter(elem => checker(elem.expression()))
                .filter(elem => !visited.contains(elem.rewrites()))

            case None =>
              // generate all children not seen yet and add to left
              rewriteFunctionWide(strategies)(elem)
                .filter(elem => solutionSmaller(strategies, solutionA = elem, solutionB = solution))
                .filter(elem => !visited.contains(elem.rewrites()))
          }

          // add to neighbors
          children.reverse.foreach(child => {
            left = (left.reverse :+ child).reverse
          })

          // drop right warning drop left in other case
          if (left.size > slideWindow) {
            left = left.drop(left.size - slideWindow)
          }

          // add to visited
          children.foreach(child => {
            visited = visited :+ child.rewrites()
          })

          // then stop this loop
          condition = false
        } else {
          // check if going down further makes sense

          // get most right child
          val children = checkExpression match {
            case Some(checker) =>
              rewriteFunctionWide(strategies)(elem)
                .filter(elem => solutionSmaller(strategies, solutionA = elem, solutionB = solution))
                .filter(elem => checker(elem.expression()))
                .filter(elem => !visited.contains(elem.rewrites()))
            case None =>
              rewriteFunctionWide(strategies)(elem)
                .filter(elem => solutionSmaller(strategies, solutionA = elem, solutionB = solution))
                .filter(elem => !visited.contains(elem.rewrites()))
          }


          if (children.isEmpty) {
            // stop go up
            // break out of loop
            condition = false

            // check if elements are left
            // if we have no children at top layer stop
            if (elem.layer() == 1) {
              elementsLeft = false
            }

          } else {
            // go down
            elem = children.last // get first element (most left)
          }
        }
      }
    }

    // right
    var right: scala.collection.mutable.Seq[Solution[Rise]] = scala.collection.mutable.Seq.empty[Solution[Rise]]
    elem = solution
    visited = scala.collection.mutable.Seq.empty[Seq[RewriteIdentifier[Rise]]]
    elementsLeft = true

    while (right.size < slideWindow && elementsLeft) {

      // add element to visited
      visited = visited :+ elem.rewrites()

      // get parent (go up)
      elem = elem.parent()

      // go down from parent
      var condition = true
      while (condition) {

        // check layer of element
        if (elem.layer() == depth - 1) {

          // generate all children not seen yet and add to left
          val children = checkExpression match {
            case Some(checker) =>
              rewriteFunctionWide(strategies)(elem)
                .filter(elem => solutionSmaller(strategies, solutionA = solution, solutionB = elem))
                .filter(elem => checker(elem.expression()))
                .filter(elem => !visited.contains(elem.rewrites()))
            case None =>
              rewriteFunctionWide(strategies)(elem)
                .filter(elem => solutionSmaller(strategies, solutionA = solution, solutionB = elem))
                .filter(elem => !visited.contains(elem.rewrites()))
          }

          // add to neighbors
          children.foreach(child => {
            right = right :+ child
          })

          // drop right warning drop left in other case
          if (right.size > slideWindow) {
            right = right.dropRight(right.size - slideWindow)
          }

          // add to visited
          children.foreach(child => {
            visited = visited :+ child.rewrites()
          })

          // then stop this loop
          condition = false
        } else {
          // check if going down further makes sense
          // get most left child
          val children = checkExpression match {
            case Some(checker) =>
              rewriteFunctionWide(strategies)(elem)
                .filter(elem => solutionSmaller(strategies, solutionA = solution, solutionB = elem))
                .filter(elem => checker(elem.expression()))
                .filter(elem => !visited.contains(elem.rewrites()))
            case None =>
              rewriteFunctionWide(strategies)(elem)
                .filter(elem => solutionSmaller(strategies, solutionA = solution, solutionB = elem))
                .filter(elem => !visited.contains(elem.rewrites()))
          }


          if (children.isEmpty) {
            // stop go up
            // break out of loop

            // if we have no children at top layer stop
            condition = false
            if (elem.layer() == 1) {
              elementsLeft = false
            }

          } else {
            // go down
            elem = children.head // get first element (most left)
          }
        }
      }
    }

    val NResult = left.toSeq ++ right.toSeq
    println("Left: " + left.size)
    left.foreach(elem => {
      println(elem.solutionSteps.map(elem => (elem.strategy, elem.location)).mkString("[", ", ", "]"))
    })
    println("Right: " + right.size)
    right.foreach(elem => {
      println(elem.solutionSteps.map(elem => (elem.strategy, elem.location)).mkString("[", ", ", "]"))
    })

    NResult
  }


  override def f(solution: Solution[Rise]): Option[Double] = {
    // buffer performance values in hashmap
    solutions.get(hashSolution(solution)) match {
      case Some(value) => value
      case _ => {
        val performanceValue = runner.execute(solution).performance
        solutions.+=(hashSolution(solution) -> performanceValue)
        performanceValue
      }
    }
  }

  // ignore this for now
  override def importSolution(filename: String): Solution[Rise] = ???

  override def exportSolution(solution: Solution[Rise], filename: String): Unit = ???

  // helper

  // warning id is set to middle here
  def solutionSmaller(
                       strategies: Seq[Strategy[Rise]],
                       solutionA: Solution[Rise],
                       solutionB: Solution[Rise]
                     ): Boolean = {

    // go down path
    // if equal go more down
    // otherwise take smaller one

    var compared = false
    var index = -1

    // find first not equal
    while (!compared && index + 1 < Math.min(solutionA.strategies().size, solutionB.strategies().size)) {
      index += 1
      //        println("index: " + index)
      if (!solutionA.strategies().apply(index).equals(solutionB.strategies().apply(index))) {
        compared = true
      }
    }

    // check special case id
    val indexA = solutionA.strategies().apply(index).equals(elevate.core.strategies.basic.id[Rise]) match {
      case true => strategies.size / 2
      //              case true => strategies.size + 1
      case false => {
        val indexA = strategies.indexOf(solutionA.strategies().apply(index))
        indexA > strategies.size / 2 - 1 match {
          case true => indexA + 1 // shift by one if we are in right half to make space for id
          case false => indexA
        }
      }
    }

    // check special case id
    val indexB = solutionB.strategies().apply(index).equals(elevate.core.strategies.basic.id[Rise]) match {
      case true => strategies.size / 2
      //              case true => strategies.size + 1
      case false =>
        val indexB = strategies.indexOf(solutionB.strategies().apply(index))
        indexB > strategies.size / 2 - 1 match {
          case true => indexB + 1 // shift by one if we are in right half to make space for id
          case false => indexB
        }
    }

    //compare index
    val result = indexA < indexB

    result
  }


}
