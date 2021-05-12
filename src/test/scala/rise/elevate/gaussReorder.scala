package rise.elevate

import java.io.{File, FileInputStream, FileOutputStream}

import elevate.core._
import elevate.core.strategies.basic.normalize
import rise.elevate.rules.lowering.{lowerToC, parallel, vectorize}
import _root_.util.gen
import elevate.core.strategies.traversal._
import rise.core.DSL.HighLevelConstructs.{padClamp2D, slide2D, zipND}
import rise.core.DSL.{fun, l, lf32, lu8}
import rise.core.primitives._
import rise.core.types._
import rise.elevate.rules.algorithmic._
import rise.elevate.rules.lowering
import rise.elevate.rules.traversal._
import rise.elevate.rules.traversal.default._
import rise.elevate.strategies.algorithmic.reorder
import rise.elevate.strategies.normalForm._
import rise.elevate.strategies.predicate.{isApplied, isMap, isReduce}
import rise.elevate.strategies.traversal
import rise.elevate.strategies.traversal._
import _root_.util.gen.c.function
import _root_.util.writeToTempFile

import scala.language.postfixOps
import scala.sys.process._
import scala.util.Random

// scalastyle:off
class gauss extends test_util.Tests {

  val outermost: (Strategy[Rise]) => (Strategy[Rise]) => Strategy[Rise] =
    traversal.outermost(default.RiseTraversable)
  val innermost: (Strategy[Rise]) => (Strategy[Rise]) => Strategy[Rise] =
    traversal.innermost(default.RiseTraversable)

  //// MM INPUT EXPRESSION /////////////////////////////////////////////////////
  val N = 1024
  val M = 1024

  val mulPair = fun(pair => fst(pair) * snd(pair))

  val gauss: Rise = {
    fun(ArrayType(N, ArrayType(M, int)))(in =>
      fun(ArrayType(5, ArrayType(5, int)))(weights =>
        in |> padClamp2D(2) // in: NxM -> (N+4) x (M+4)
          |> slide2D(5, 1) // -> MxN of 5x5 slides
          |> map(map(fun(sector => // sector:5x5
          zip(sector |> join)(weights |> join) |> map(mulPair) |> reduce(add)(l(0)) |> fun(x => x/l(256))
        )))
      )
    )
  }
  val lowering = DFNF() `;` lowerToC

  // -- CPU ---------------------------------------------------------------

  val cpu: Strategy[Rise] = DFNF()(default.RiseTraversable) `;`
    normalize.apply(fuseReduceMap `@` topDown[Rise])

  val splitReduce = splitStrategy(1024)   `@` innermost(isApplied(isApplied(isApplied(isReduce))))
  val reordering = reorder(List(1,2,4,3)) // expected to work
  //val reordering = reorder(List(1,2)) // Works, but I have no idea what's going on

  val vectorization = vectorize(64) `@` innermost(isApplied(isApplied(isMap)))
  test("vectorize") {
    println("gauss: \n" + gauss)
    val splitted = splitReduce.apply(gauss)
    println("splitted: \n" + splitted.get)
    val reordered = reordering.apply(splitted.get)
    println("reordered: \n" + reordered.get)
    val vectorized = vectorization.apply(reordered.get)
    println("vectorized: \n" + vectorized.get)
  }
}
