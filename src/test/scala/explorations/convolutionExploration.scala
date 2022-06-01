package explorations

import apps.separableConvolution2D
import apps.separableConvolution2D.{base, baseVecU, binomialWeights2d}
import rise.core.DSL.HighLevelConstructs.{padClamp2D, slide2D, unslide2D}
import rise.core.DSL.{ToBeTyped, depFun, fun, lf32}
import rise.core.Expr
import rise.core.primitives.{add, join, map, reduce, zip}
import rise.core.types.{AddressSpace, Nat}
import rise.openCL.DSL.{mapLocal, mapWorkGroup, toLocal}
import rise.openCL.primitives.oclReduceSeqUnroll
import rise.core.DSL._
import rise.core.DSL.Type._
import HighLevelConstructs._
import apps.separableConvolution2D
import arithexpr.arithmetic.RangeMul
import elevate.core.Success
import rise.autotune
import rise.autotune.{HostCode, Median, Timeouts, tuningParam, wrapOclRun}
import rise.core._
import rise.core.primitives._
import rise.core.types._
import rise.openCL.DSL._
import rise.openCL.primitives.oclReduceSeqUnroll
import shine.OpenCL.Module.translateToString
import shine.OpenCL.{GlobalSize, LocalSize}
import util.gen

object convolutionExploration {

  private val weights2d = binomialWeights2d
  val convolution = base(weights2d)

  //  val convolution = baseVecU(binomialWeights2d)

  // can't execute -> prepare problem for thomas?

  def main(args: Array[String]): Unit = {
    println("convolution: " + convolution)


    val lowering = exploration.strategies.convolutionStrategies.loweringStrategy
    //    val lowering = convolution

    println("start lowering")
    val lowered = lowering.apply(convolution)
    //    val lowered = Success(convolution)

    println("lowered: " + lowered)

    // generate code?

    println("try to generate code")
    val code = gen.opencl.hosted("fun").fromExpr(lowered.get)
    println("code: " + translateToString(code))


    // now wrap ocl
    val eTuning: Expr =
      tuningParam("ls0", RangeMul(1, 1024, 2), (ls0: Nat) =>
        tuningParam("ls1", RangeMul(1, 1024, 2), (ls1: Nat) =>
          tuningParam("gs0", RangeMul(1, 1024, 2), (gs0: Nat) =>
            tuningParam("gs1", RangeMul(1, 1024, 2), (gs1: Nat) =>
              wrapOclRun(LocalSize(ls0, ls1), GlobalSize(gs0, gs1))(lowered.get)
            ))))

    println("eTuning: " + eTuning)

    // now generate code


    //    riseExploration(convolution, "exploration/configuration/convolution.json")

  }

}
