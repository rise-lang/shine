package exploration.rewriter

import rise.core.DSL.{fun, lf32}
import rise.core.primitives._
import rise.core.types.DataType.{ArrayType, DataTypeIdentifier, f32}
import rise.elevate.rules.lowering.lowerToC
import rise.elevate.rules.algorithmic.fuseReduceMap
import elevate.core._
import elevate.heuristic_search.util.{RewriteIdentifier, Solution, SolutionStep, hashProgram, hashSolution}
import exploration.runner.CExecutor
import rise.autotune.HostCode
import rise.core.equality.{exprAlphaEq, typeAlphaEq, typeErasure}
import rise.core.{App, DepApp, DepLambda, Expr, Identifier, Lambda, Literal, Opaque, Primitive, TypeAnnotation, TypeAssertion}
import rise.elevate.Rise
import rise.elevate.rules.traversal.alternative
import rise.elevate.rules.traversal.default._
import rise.elevate.strategies.normalForm.DFNF
import rise.elevate.strategies.tiling.tile
import rise.elevate.strategies.traversal._

import scala.collection.parallel.CollectionConverters._
import java.io.{File, FileOutputStream, PrintWriter}
import java.io.{File, FileInputStream, FileReader}
import rise.core.DSL.ToBeTyped
import rise.core.Expr

import scala.reflect.runtime._
import scala.tools.reflect.ToolBox
import exploration.runner.checkSolutionC

object everywhere {


  case class ExprWrapper(e: Expr) {
    override def hashCode(): Int = exprAlphaEq(typeErasure).hash(e)

    override def equals(o: Any): Boolean = o match {
      case other: ExprWrapper => exprAlphaEq(typeAlphaEq).apply(this.e)(other.e)
      case other: Expr => exprAlphaEq(typeAlphaEq).apply(this.e)(other)
      case _ => false
    }
  }

  private def mayApply(s: Strategy[Rise], p: Rise): Option[Rise] = {
    s(p) match {
      case Success(p) => Some(p)
      case Failure(_) => None
    }
  }

  type ExpandStrategy = Rise => Seq[Rise]

  // todo check if we can extract the traversal from this
  // todo check if all possible locations are covered by this function
  var counter = 0

  // todo improve performance of this
  def everywhere(s: Strategy[Rise]): ExpandStrategy = { p =>

    counter += 1
    //    println(s"everywhere: [${counter}]")
    import rise.core.types._
    mayApply(s, p).toSeq ++ (p match {
      case App(f, e) => everywhere(s)(f).map(App(_, e)(p.t)) ++ everywhere(s)(e).map(App(f, _)(p.t))
      case Identifier(_) => Nil
      case Lambda(x, e) => everywhere(s)(e).map(Lambda(x, _)(p.t))
      case DepLambda(_, x, e) => x match {
        case n: NatIdentifier =>
          everywhere(s)(e).map(DepLambda(NatKind, n, _)(p.t))
        case n: DataTypeIdentifier =>
          everywhere(s)(e).map(DepLambda(DataKind, n, _)(p.t))
        case n: AddressSpaceIdentifier =>
          everywhere(s)(e).map(DepLambda(AddressSpaceKind, n, _)(p.t))
      }
      case DepApp(kind, f, x) => everywhere(s)(f).map(DepApp(kind, _, x)(p.t))
      case Literal(_) => Nil
      case _: TypeAnnotation => throw new Exception("Type annotations should be gone.")
      case _: TypeAssertion => throw new Exception("Type assertions should be gone.")
      case _: Opaque => throw new Exception("Opaque expressions should be gone.")
      case _: Primitive => Nil
    })
  }

  val lowering = exploration.strategies.blockingExploration.lowering

  // neighbors are on same level (going wide) with slide Window
  def neighbourhoodWide(
                         strategies: Seq[Strategy[Rise]],
                         slideWindow: Int
                       )(solution: Solution[Rise]): Seq[Solution[Rise]] = {


    def solutionSmaller(
                         strategies: Seq[Strategy[Rise]],
                         solutionA: Solution[Rise],
                         solutionB: Solution[Rise]
                       ): Boolean = {

      //      println("compare: ")
      //      println("solution: " + solution.strategies().size)
      //      println("solutionA: " + solutionA.rewrites().mkString("[", ", ", "]"))
      //      println("solutionB: " + solutionB.rewrites().mkString("[", ", ", "]"))
      //
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
      //      val indexA = strategies.indexOf(solutionA.strategies().apply(index))

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
        //          strategies.indexOf(solutionB.strategies().apply(index))
      }

      //      val indexB = strategies.indexOf(solutionB.strategies().apply(index))
      // get index of found strategies
      //      val indexA = strategies.indexOf(solutionA.strategies().apply(index))
      //      val indexB = strategies.indexOf(solutionB.strategies().apply(index))

      //compare index
      val result = indexA < indexB
      //      println("A smaller B: " + result)

      result
    }

    //    println("get neighborhood")
    //    println("solution: " + solution.solutionSteps.map(elem => (elem.strategy, elem.location)).mkString("[", ", ", "]"))
    // todo implement this here

    // todo make this a config option
    val depth = 6 // 5 rewrites, ergo 6 layers

    // left
    var left: scala.collection.mutable.Seq[Solution[Rise]] = scala.collection.mutable.Seq.empty[Solution[Rise]]

    var elem = solution

    // maybe optimize by only adding Seq[Int] instead of full Seq[Solution[Rise]]
    //    var visited = scala.collection.mutable.Seq.empty[Seq[(Strategy[Rise], Int)]]
    var visited = scala.collection.mutable.Seq.empty[Seq[RewriteIdentifier[Rise]]]

    var elementsLeft = true

    while (left.size < slideWindow && elementsLeft) {
      //      println("\n")

      // add element to visited
      //      visited = visited :+ elem.rewrites().map(elem => (elem.strategy, elem.location))
      visited = visited :+ elem.rewrites()

      // get parent (go up)
      elem = elem.parent()

      //      println("elem: " + elem.solutionSteps.map(elem => (elem.strategy, elem.location)).mkString("[", ", ", "]"))
      //      println("visited: " + visited.size)
      //      println("left.size: " + left.size)
      //      println("layer: " + elem.layer())

      // go down from parent
      var condition = true
      //      println("go down")
      while (condition) {
        // check layer of element
        if (elem.layer() == depth - 1) {
          //          println("get children as neighbrohood elems")

          // generate all children not seen yet and add to left
          // todo check if we can filter out permutations of rewrites?
          // use convetion/rule: don't allow id after any rewrite - no intermediate or trailing id
          val children = rewriteFunctionWide(strategies)(elem)
            .filter(elem => solutionSmaller(strategies, solutionA = elem, solutionB = solution))
            .filter(elem => checkSolutionC(lowering)(elem))
            .filter(elem => !visited.contains(elem.rewrites()))


          // add to neighbors
          children.reverse.foreach(child => {
            left = (left.reverse :+ child).reverse
          })

          // drop right warning drop left in other case
          if (left.size > slideWindow) {
            //            println("left is bigger, lets drop elements")
            left = left.drop(left.size - slideWindow)
            //            println("don'!")
          }

          // add to visited
          children.foreach(child => {
            //            visited = visited :+ child.rewrites().map(elem => (elem.strategy, elem.location))
            visited = visited :+ child.rewrites()
          })

          // then stop this loop
          condition = false
        } else {
          // check if going down further makes sense
          //          println("go further down most left thing")

          // get most right child
          // todo filter out visited ones
          val children = rewriteFunctionWide(strategies)(elem)
            .filter(elem => solutionSmaller(strategies, solutionA = elem, solutionB = solution))
            .filter(elem => checkSolutionC(lowering)(elem))
            .filter(elem => !visited.contains(elem.rewrites()))

          if (children.isEmpty) {
            //            println("stop no child left go up further up")
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
            //            println("go down ")
            elem = children.last // get first element (most left)
            //            println("elem: " + elem.solutionSteps.map(elem => (elem.strategy, elem.location)).mkString("[", ", ", "]"))
          }
        }
      }
    }


    //     print neighbors
    //    println("Left: " + left.size)
    //    left.foreach(elem => {
    //      println(elem.solutionSteps.map(elem => (elem.strategy, elem.location)).mkString("[", ", ", "]"))
    //    })

    //    println("\n\nRight\n\n")


    // right
    var right: scala.collection.mutable.Seq[Solution[Rise]] = scala.collection.mutable.Seq.empty[Solution[Rise]]

    elem = solution

    // maybe optimize by only adding Seq[Int] instead of full Seq[Solution[Rise]]
    //    visited = scala.collection.mutable.Seq.empty[Seq[(Strategy[Rise], Int)]]
    visited = scala.collection.mutable.Seq.empty[Seq[RewriteIdentifier[Rise]]]

    elementsLeft = true

    println("right: ")

    while (right.size < slideWindow && elementsLeft) {
      println("\n")

      // add element to visited
      //      visited = visited :+ elem.rewrites().map(elem => (elem.strategy, elem.location))
      visited = visited :+ elem.rewrites()

      // get parent (go up)
      elem = elem.parent()

      println("elem: " + elem.solutionSteps.map(elem => (elem.strategy, elem.location)).mkString("[", ", ", "]"))
      println("visited: " + visited.size)
      println("right.size: " + right.size)
      println("layer: " + elem.layer())
      println("right tmp:")
      right.foreach(elem => println(elem.rewrites().mkString("[", ", ", "]")))

      // go down from parent
      var condition = true
      //      println("go down")
      while (condition) {


        // check layer of element
        if (elem.layer() == depth - 1) {
          println("get children as neighbrohood elems")

          // generate all children not seen yet and add to left
          // todo check if we can filter out permutations of rewrites?
          val children = rewriteFunctionWide(strategies)(elem)
            .filter(elem => solutionSmaller(strategies, solutionA = solution, solutionB = elem))
            .filter(elem => checkSolutionC(lowering)(elem))
            .filter(elem => !visited.contains(elem.rewrites()))


          // add to neighbors
          children.foreach(child => {
            right = right :+ child
          })

          // drop right warning drop left in other case
          if (right.size > slideWindow) {
            println("right is bigger, lets drop elements")
            right = right.dropRight(right.size - slideWindow)
          }

          // add to visited
          children.foreach(child => {
            //            visited = visited :+ child.rewrites().map(elem => (elem.strategy, elem.location))
            visited = visited :+ child.rewrites()
          })

          // then stop this loop
          condition = false
        } else {
          // check if going down further makes sense
          println("go further down most left thing")

          // get most left child
          // todo filter out visited ones
          val children = rewriteFunctionWide(strategies)(elem)
            .filter(elem => solutionSmaller(strategies, solutionA = solution, solutionB = elem))
            .filter(elem => checkSolutionC(lowering)(elem))
            .filter(elem => !visited.contains(elem.rewrites()))

          if (children.isEmpty) {
            println("stop no child left go up further up")
            // stop go up
            // break out of loop


            // if we have no children at top layer stop
            condition = false
            println("elem.layer(): " + elem.layer())
            if (elem.layer() == 1) {
              elementsLeft = false
            }

          } else {
            // go down
            println("go down ")
            elem = children.head // get first element (most left)
            println("elem: " + elem.rewrites().mkString("[", ", ", "]"))
          }
        }

      }
    }

    //    System.exit(0)

    // repeat for left

    // return left ++ right

    val result = left.toSeq ++ right.toSeq
    println("Left: " + left.size)
    left.foreach(elem => {
      println(elem.solutionSteps.map(elem => (elem.strategy, elem.location)).mkString("[", ", ", "]"))
    })
    println("Right: " + right.size)
    right.foreach(elem => {
      println(elem.solutionSteps.map(elem => (elem.strategy, elem.location)).mkString("[", ", ", "]"))
    })

    //     print neighbors
    //    println("N: " + result.size)
    //    result.foreach(elem => {
    //      println(elem.solutionSteps.map(elem => (elem.strategy, elem.location)).mkString("[", ", ", "]"))
    //    })

    //    System.exit(0)

    result
  }

  // neighbors are children and parent of current solution
  def neighborhoodDeep(
                        strategies: Seq[Strategy[Rise]]
                      )(solution: Solution[Rise]): Seq[Solution[Rise]] = {

    // standard case, call method
    rewriteFunction(strategies)(solution)
  }

  def rewriteFunction(strategies: Seq[Strategy[Rise]])(solution: Solution[Rise]): Seq[Solution[Rise]] = {

    // todo check try catch
    // todo add checking here?
    //    val rewritten: Seq[Solution[Rise]] = strategies.toSeq.par.flatMap(rule => {


    val rewritten: Seq[Solution[Rise]] = strategies.flatMap(rule => {
      //      println("try: " + rule)
      val rewrites = everywhere(rule).apply(solution.expression())

      val solutions: Seq[Solution[Rise]] = Range(0, rewrites.size).zip(rewrites).map(elem => {

        // create new step
        val step = SolutionStep[Rise](
          expression = elem._2,
          strategy = rule,
          location = elem._1
        )

        Solution[Rise](
          solutionSteps = solution.solutionSteps :+ step
        )
      })
      solutions
    })

    // only add id rewrite if we have only heading ids
    val output = solution.rewrites().forall(ri => ri.strategy.equals(elevate.core.strategies.basic.id[Rise])) match {
      case true =>
        // add id rewrite at the end
        val idSolution = Solution[Rise](
          solutionSteps = solution.solutionSteps :+ SolutionStep[Rise](
            expression = solution.expression(),
            strategy = elevate.core.strategies.basic.id[Rise],
            location = 0
          )
        )
        rewritten :+ idSolution
      case false => rewritten
    }

    output
  }


  def rewriteFunctionWide(strategies: Seq[Strategy[Rise]])(solution: Solution[Rise]): Seq[Solution[Rise]] = {

    // todo check try catch
    // todo add checking here?
    //    val rewritten: Seq[Solution[Rise]] = strategies.toSeq.par.flatMap(rule => {

    val rewritten: Seq[Solution[Rise]] = strategies.flatMap(rule => {
      //      println("try: " + rule)
      val rewrites = everywhere(rule).apply(solution.expression())

      val solutions: Seq[Solution[Rise]] = Range(0, rewrites.size).zip(rewrites).map(elem => {

        // create new step
        val step = SolutionStep[Rise](
          expression = elem._2,
          strategy = rule,
          location = elem._1
        )

        Solution[Rise](
          solutionSteps = solution.solutionSteps :+ step
        )
      })
      solutions
    })

    // only add id rewrite if we have only heading ids
    val output = solution.rewrites().forall(ri => ri.strategy.equals(elevate.core.strategies.basic.id[Rise])) match {
      case true =>
        // add id rewrite at the end
        val idSolution = Solution[Rise](
          solutionSteps = solution.solutionSteps :+ SolutionStep[Rise](
            expression = solution.expression(),
            strategy = elevate.core.strategies.basic.id[Rise],
            location = 0
          )
        )
        rewritten :+ idSolution
      case false => rewritten
    }

    output
  }


  //  def rewriteFunction(solution: Solution[Rise]): scala.collection.immutable.Seq[Solution[Rise]] = {
  //
  //    // todo check try catch
  //    // todo add checking here?
  //    val rewritten: scala.collection.immutable.Seq[Solution[Rise]] = exploration.strategies.blockingExploration.rules.map(rule => {
  //      //      println("try: " + rule)
  //      everywhere(rule).apply(solution.expression).map(e => Solution(e, solution.strategies :+ rule))
  //    }).flatten
  //
  //    println("rewrite: " + rewritten.size)
  //
  //    rewritten
  //  }


}
