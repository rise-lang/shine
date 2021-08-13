package rise.elevate.strategies

import elevate.core._
import elevate.core.strategies.{Traversable, basic}
import rise.core.primitives._
import rise.elevate.rules.algorithmic._
import elevate.core.strategies.traversal._
import elevate.core.strategies.basic._
import elevate.core.macros._
import rise.elevate.Rise
import rise.elevate.rules.traversal._
import rise.elevate.strategies.algorithmic._
import rise.elevate.strategies.normalForm._
import rise.elevate.strategies.predicate.isReduce

object traversal {

  // applying a strategy to an expression applied to a lift `map`. Example:
  // ((map λe14. (transpose ((map (map e12)) e14))) e13) // input expr
  //  (map λe14. (transpose ((map (map e12)) e14)))      // result of `function`
  //       λe14. (transpose ((map (map e12)) e14))       // result of `argument`
  //             (transpose ((map (map e12)) e14))       // result of 'body' -> here we can apply s

  def fmap: Strategy[Rise] => Strategy[Rise] =
    transformer("fmap", s => function(argumentOf(map.primitive, body(s))))

  // fmap applied for expressions in rewrite normal form:
  // fuse -> fmap -> fission
  def fmapRNF(implicit ev: Traversable[Rise]): Strategy[Rise] => Strategy[Rise] = transformer("fmapRNF",
    s => DFNF() `;` mapFusion `;`
         DFNF() `;` fmap(s) `;`
         DFNF() `;` one(mapFullFission))

  // applying a strategy to an expression nested in one or multiple lift `map`s
  def mapped(implicit ev: Traversable[Rise]): Strategy[Rise] => Strategy[Rise] =
    transformer("mapped", s => s <+ (e => fmapRNF(ev)(mapped(ev)(s))(e)))

  // moves along RNF-normalized expression
  // e.g., expr == ***f o ****g o *h
  // move(0)(s) == s(***f o ****g o *h)
  // move(1)(s) == s(****g o *h)
  // move(2)(s) == s(*h)
  def moveTowardsArgument(i: Int): Strategy[Rise] => Strategy[Rise] =
    transformer("moveTowardsArgument", s => applyNTimes(i)((e: Strategy[Rise]) => argument(e))(s))

  // TRAVERSAL DSL as described in ICFP'20 /////////////////////////////////////
  implicit class AtHelper[P](s: Strategy[P]) {
    def at(traversal: Strategy[P] => Strategy[P]): Strategy[P] =
      traversal(s)
    def `@`(traversal: Strategy[P] => Strategy[P]): Strategy[P] = // scalastyle:ignore
      traversal(s)
  }

  def outermost(using ev: Traversable[Rise] = default.RiseTraversable): Strategy[Rise] => Strategy[Rise] => Strategy[Rise] =
    combinator("outermost", {
      predicate => s => topDown(predicate `;` s)
    })

  def innermost(using ev: Traversable[Rise] = default.RiseTraversable): Strategy[Rise] => Strategy[Rise] => Strategy[Rise] =
    combinator("innermost", {
      predicate => s => bottomUp(predicate `;` s)
    })

  def everywhere: Strategy[Rise] => Strategy[Rise] =
    transformer("everywhere", s => basic.normalize(s)(using default.RiseTraversable))

  def check: Strategy[Rise] => Strategy[Rise] => Strategy[Rise] =
    combinator("check", predicate => predicate `;` _)

  def mapNest(d: Int): Strategy[Rise] = strategy("mapNest", p => (d match {
    case x if x == 0 => Success(p)
    case x if x < 0  => Failure(mapNest(d))
    case _ => fmap(mapNest(d-1))(p)
  }))

  def blocking(using ev: Traversable[Rise]): Strategy[Rise] = {
    basic.id `@` outermost(using ev)(mapNest(2))
    basic.id `@` outermost(using ev)(isReduce)
  }
}
