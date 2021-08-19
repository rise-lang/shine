package rise.elevate.rules

import arithexpr.arithmetic.Cst
import elevate.core.{Failure, RewriteResult, Strategy, Success}
import elevate.macros.RuleMacro.rule
import rise.core.{App, Expr, Lambda}
import rise.core.DSL._
import rise.core.primitives.{add, split, zip, let => _, _}
import rise.core.types.{AddressSpace, ArrayType}
import rise.elevate.Rise
import rise.elevate.rules.traversal.default
import rise.elevate.strategies.predicate.{isApplied, isPrimitive}
import rise.openCL.primitives._

object workEfficientScan {
  private def log2(x: Double) = Math.log10(x) / Math.log10(2.0)
  private def innermost = rise.elevate.strategies.traversal.innermost(default.RiseTraversable)
  private def isAbstractScan = isApplied(isApplied(isApplied(isPrimitive(scan))))

  // Given a call to scan with an array of (constant) 2^n elements, applies the
  // blockScanGivenSize rule
  @rule def blockScan: Strategy[Rise] = {
    case e @ App(App(App(scan(), _), _), input)
      if (input.t match {
        case ArrayType(Cst(x), rise.core.types.f32) if log2(x.toDouble) % 1 == 0 => true
        case _ => false
      })
    =>
      val size = input.t.asInstanceOf[ArrayType].size.asInstanceOf[Cst].c.toInt
      blockScanGivenSize(size)(e)
  }

  // Recursively applies the block-expansion rules, and then lowers the remaining
  // inner scan
  @rule def blockScanGivenSize(blockSize: Int): Strategy[Rise] = {
    initExp => {


      val numIterations = log2(blockSize).toInt

      val init: RewriteResult[Rise] = Success(initExp)

      // For each iteration
      (0 until numIterations).foldLeft(
        init
      )((exp, _) => {
        // Apply the expand block scan rule to the innermost abstract scan we have
        exp.flatMapSuccess(
        innermost(isAbstractScan)(
          rise.elevate.rules.workEfficientScan.expandBlockScan
        ))
      }).flatMapSuccess( // Lower the innermost abstract scan
        innermost(isAbstractScan)(rise.elevate.rules.workEfficientScan.lowerToSequential)
      )
    }
  }

  // Performs one iteration of block expansion
  @rule def expandBlockScan: Strategy[Rise] = {
    case e @ App(App(App(scan(), _), init), in) =>
      def sum = oclReduceSeq(AddressSpace.Private)(add)(lf32(0.0f))

      val input = preserveType(in)
      val rewritten = let(input |> split(2) |> mapLocal(0)(sum) |> oclToMem(AddressSpace.Local))
        .be(next => {
          val upsweep = scan(add)(init)(next) |> oclToMem(AddressSpace.Local)
          zip(upsweep)(input |> split(2)) |>
            mapLocal(0)(fun(pair => oclScanSeqUnroll(AddressSpace.Private)(add)(pair._1)(pair._2)))|>
            join
        })
      Success(rewritten !: e.t)
  }

  // Lowers the remaining scan to sequential
  @rule def lowerToSequential: Strategy[Rise] = {
    case e @ App(App(App(scan(), _), init), in) =>
      Success(oclScanSeqUnroll(AddressSpace.Private)(add)(init)(in) !: e.t)
  }
}