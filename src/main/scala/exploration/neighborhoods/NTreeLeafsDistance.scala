package exploration.neighborhoods

import elevate.heuristic_search.HeuristicPanel
import elevate.heuristic_search.util.Solution
import elevate.heuristic_search.Runner
import elevate.core._
import elevate.heuristic_search.util._

import rise.elevate.Rise
import exploration.rewriter.everywhere._

/**
  * Neighborhood is defined by distance in the tree (how many paths/steps take)
  * around leafs (fill with id)
  * All solutions are on leaf layer
  */
case class NTreeLeafsDistance(
                               runner: Runner[Rise],
                               strategies: Seq[Strategy[Rise]],
                               afterRewrite: Option[Strategy[Rise]] = None,
                               checkExpression: Option[Rise => Boolean] = None,
                               depth: Int = 5
                             ) extends HeuristicPanel[Rise] {

  val solutions = new scala.collection.mutable.HashMap[String, Option[Double]]()

  // todo design and implement this
  // new approach requires thinking
  override def N(solution: Solution[Rise]): Seq[Solution[Rise]] = {
    var Neighbors = scala.collection.mutable.Seq.empty[Solution[Rise]]

    println(s"\nsolution: ${solution.rewrites().mkString("[", ", ", "]")}")

    Range(0, depth + 1).foreach(i => {

      //      println(s"[${i}] : ")

      // check if we have to generate elements
      val generateElements = i >= depth match {
        case true => true // no successor, let's rewrite
        case false =>
          // check if successor is id
          val result = !solution.rewrites().apply(i + 1).strategy
            .equals(elevate.core.strategies.basic.id[Rise]) // successor might be id
          //          println("successor is not id: " + result)
          result
      }

      if (generateElements) {
        //        println("generate elements")

        // warning: this only works for trailing elements

        // generate solution from which we start the rewriting
        //        val rewrites = solution.rewrites().take(i)
        //        println("rewrites to make: " + rewrites.mkString("[", ", ", "]"))

        val rewrittenSolution = Solution[Rise](solution.solutionSteps.take(i))
        //        println("rewrittes to make: " + rewrittenSolution2.rewrites().mkString("[", ", ", "]"))

        Neighbors = {
          //          println("resultling expression:  " + rewrittenSolution.rewrites().mkString("[", ", ", "]"))

          val rewritten: Seq[Solution[Rise]] = strategies.flatMap(rule => {
            //            println("\ngenerate neighrbors for: ")
            //            println("rule: " + rule)
            //            println("expression: " + rewrittenSolution.rewrites().mkString("[", ",", "]"))

            val rewrites = checkExpression match {
              case Some(checker) =>
                everywhere(rule).apply(rewrittenSolution.expression()).filter(elem => checker(elem))
              case None =>
                everywhere(rule).apply(rewrittenSolution.expression())
            }

            val solutions: Seq[Solution[Rise]] = Range(0, rewrites.size).zip(rewrites).map(elem => {

              // create new step
              val step = SolutionStep[Rise](
                expression = elem._2,
                strategy = rule,
                location = elem._1
              )

              Solution[Rise](
                solutionSteps = rewrittenSolution.solutionSteps :+ step
              )

            })

            //            println("solutions: ")
            //            solutions.foreach(elem => {
            //              println(elem.rewrites().mkString("[", ", ", "]"))
            //            })


            solutions
          })

          //          println("rewritten: ")
          //          rewritten.foreach(elem => {
          //            println(elem.rewrites().mkString("[", ", ", "]"))
          //          })

          // now check if we can apply the tail to the rewritten ones
          val tailRewrites = solution.rewrites().takeRight(solution.rewrites().size - 1 - i)
          //          println("tailRewrites to make: " + tailRewrites.mkString("[", ", ", "]"))
          val fullyRewrittenNeighbors = tailRewrites.size match {
            case 0 => rewritten
            case _ =>
              rewritten.map(sol => applyRewrites(sol, tailRewrites)).filter(elem => elem.isDefined).map(elem => elem.get)
          }

          //          println("fully rewritten Neighbors: ")
          //          fullyRewrittenNeighbors.foreach(elem => {
          //            println(elem.rewrites().mkString("[", ", ", "]"))
          //          })

          Neighbors ++ fullyRewrittenNeighbors
        }

      } else {
        //        println("don't generate elements")
        // don't generate elements
        // skip?
        // do we have to cover something?
      }
    })

    // add id by hand during rewriting?
    // if we try

    // now make less is more strategy
    // let one out

    // generate children (pull one up by hand)
    // if predecessor is id and not zero
    // remove on heading id and rewrite

    // permute current?
    // do we want to allow this?


    println("Neighbors: ")
    Neighbors.foreach(elem => {
      println(elem.rewrites().mkString("[", ", ", "]"))
    })

    println("\n")

    Neighbors.toSeq
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
  def applyRewrites(
                     solution: Solution[Rise],
                     rewrites: Seq[RewriteIdentifier[Rise]]
                   ): Option[Solution[Rise]] = {

    // apply rewrites to solution
    var result = scala.collection.mutable.Seq.empty[SolutionStep[Rise]]

    // check if we want to optimize this for id strategy (huge amount of locations usually)

    //    println("apply rewrite: " + rewrites.mkString("[", ",", "]"))
    //    println("to solution: " + solution.rewrites().mkString("[", ", ", "]"))

    rewrites.foreach(ri => {

      // get candidates
      val candidates = checkExpression match {
        case Some(checker) =>
          everywhere(ri.strategy).apply(solution.expression()).filter(elem => checker(elem))
        case None =>
          everywhere(ri.strategy).apply(solution.expression())
      }

      // check if location exists
      if (ri.location < candidates.size) {
        // add step to result
        result = result :+ SolutionStep(
          expression = candidates.apply(ri.location),
          strategy = ri.strategy,
          location = ri.location
        )
      } else {
        // return None, we could not reproduce this rewrite
        return None
      }
    })

    Some(Solution[Rise](solution.solutionSteps ++ result))
  }
}

