package rise.elevate.rules

import arithexpr.arithmetic.Cst
import elevate.core.{Failure, RewriteResult, Strategy, Success}
import elevate.macros.RuleMacro.rule
import rise.core.{App, Expr, Lambda}
import rise.core.DSL._
import rise.core.primitives.{add, split, zip, let => _, _}
import rise.core.types.AddressSpace
import rise.core.types.DataType.ArrayType
import rise.elevate.Rise
import rise.elevate.rules.traversal._
import rise.elevate.strategies.predicate.{isApplied, isPrimitive}
import rise.openCL.primitives._
import elevate.core._
import rise.elevate.strategies.traversal._
import elevate.core.strategies.basic.{`try`, repeatNTimes}

object workEfficientScan {
  private def log(base: Int, x: Double) = Math.log10(x) / Math.log10(base)

  private def innermost = rise.elevate.strategies.traversal.innermost(default.RiseTraversable)
  private def isAbstractScan = isApplied(isApplied(isApplied(isPrimitive(scan))))

  // Given a call to scan with an array of (constant) 2^n elements, applies the
  // blockScanGivenSize rule
  @rule def blockScan(factor:Int = 2, skipDepth: Int = 0): Strategy[Rise] = {
    case e @ App(App(App(scan(), _), _), input)
      if (input.t match {
        case ArrayType(Cst(x), rise.core.types.DataType.f32) if log(factor, x.toDouble) % 1 == 0 => true
        case _ => false
      })
    =>
      val blockSize = input.t.asInstanceOf[ArrayType].size.asInstanceOf[Cst].c.toInt
      val numIterations = log(factor, blockSize).toInt - skipDepth

      val rule =
        repeatNTimes(numIterations)(expandBlockScan(factor) `@` innermost(isAbstractScan)) `;`
        (lowerScanToSequential `@` innermost(isAbstractScan))

//      val rule =
//        repeatNTimes(numIterations)(expandBlockScan `@` innermost(isAbstractScan)) `;`
//          oclLower

      rule(e)
  }

  // Performs one iteration of block expansion
  @rule def expandBlockScan(factor:Int): Strategy[Rise] = {
    case e @ App(App(App(scan(), f), init), in) =>
      def sum = oclReduceSeq(AddressSpace.Private)(f)(init)

      val input = preserveType(in)
      val rewritten = let(input |> split(factor) |> mapLocal(0)(sum) |> oclToMem(AddressSpace.Local))
        .be(next => {
          val upsweep = scan(f)(init)(next) |> oclToMem(AddressSpace.Local)
          zip(upsweep)(input |> split(factor)) |>
            mapLocal(0)(fun(pair => oclScanSeqUnroll(AddressSpace.Private)(f)(pair._1)(pair._2))) |>
            join
        })
      Success(rewritten !: e.t)
  }

  @rule def absExpandBlockScan: Strategy[Rise] = {
    case e @ App(App(App(scan(), f), init), in) =>
      def sum = reduce(f)(init)

      val input = preserveType(in)
      val rewritten = let(input |> split(2) |> map(sum) |> toMem)
        .be(next => {
          val upsweep = scan(f)(init)(next) |> toMem
          zip(upsweep)(input |> split(2)) |>
            map(fun(pair => scan(f)(pair._1)(pair._2))) |>
            join
        })
      Success(rewritten !: e.t)
  }

  // Lowers the remaining scan to sequential
  @rule def lowerScanToSequential: Strategy[Rise] = {
    case e @ App(App(App(scan(), f), init), in) =>
      Success(oclScanSeqUnroll(AddressSpace.Private)(f)(init)(in) !: e.t)
  }

  def oclLower: Strategy[Rise] = everywhere(
    `try` (`toMem -> oclToMem`) `;`
    `try` (`scan -> oclScanSeqUnroll`) `;`
    `try` (`reduce -> oclReduceSeqUnroll`) `;`
    `try` (`map -> mapLocal`(0))
  )

  def `toMem -> oclToMem`: Strategy[Rise] = toMemRule
  @rule def toMemRule: Strategy[Rise] = {
    case e @ toMem()  =>
      Success(oclToMem(AddressSpace.Local) !: e.t)
  }
  def `scan -> oclScanSeqUnroll`: Strategy[Rise] = scanRule
  @rule def scanRule: Strategy[Rise] = {
    case e @ App(App(App(scan(), f), init), in) =>
      Success(oclScanSeqUnroll(AddressSpace.Private)(f)(init)(in) !: e.t)
  }

  def `reduce -> oclReduceSeqUnroll`: Strategy[Rise] = reduceRule
  @rule def reduceRule: Strategy[Rise] = {
    case m@reduce() => Success(reduceSeqUnroll(AddressSpace.Private) !: m.t)
  }

  def `map -> mapLocal`(dim: Int): Strategy[Rise] = mapLocalRule(dim)
  @rule def mapLocalRule(dim: Int = 0): Strategy[Rise] = {
    case m@map() => Success(rise.openCL.DSL.mapGlobal(dim) !: m.t)
  }
}