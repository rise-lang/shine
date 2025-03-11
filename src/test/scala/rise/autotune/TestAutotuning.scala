package rise.autotune

import arithexpr.arithmetic._
import rise.autotune
import rise.core.DSL._
import rise.core._
import rise.core.primitives.{let => _, _}
import rise.core.types.{NatIdentifier, _}
import shine.OpenCL.{GlobalSize, LocalSize}

class TestAutotuning extends test_util.Tests {

  test("collect parameters") {
    val params = autotune.constraints.collectParameters(util.expressions.convolution.convolutionOclGsLsWrap)
    assert(params.find(IsTuningParameter("vec")).get.range == RangeAdd(1, 32, 1))
    assert(params.find(IsTuningParameter("tile")).get.range == RangeAdd(4, 32, 1))
    assert(params.find(IsTuningParameter("ls0")).get.range == RangeUnknown)
    assert(params.find(IsTuningParameter("ls1")).get.range == RangeUnknown)
    assert(params.find(IsTuningParameter("gs0")).get.range == RangeUnknown)
    assert(params.find(IsTuningParameter("gs1")).get.range == RangeUnknown)
    assert(params.size == 6)
  }

  test("substitute parameters") {
    val e: Expr = util.expressions.convolution.convolution(32)
    val constraints = autotune.constraints.collectConstraints(e,
      autotune.constraints.collectParameters(e))
    println("constraints: \n" + constraints)

    val badParameters1 = Map(
      TuningParameter("vec") -> (5: Nat),
      TuningParameter("tile") -> (15: Nat)
    )
    assert(!autotune.constraints.checkConstraints(constraints, badParameters1))

    val badParameters2 = Map(
      TuningParameter("vec") -> (4: Nat),
      TuningParameter("tile") -> (13: Nat)
    )
    assert(!autotune.constraints.checkConstraints(constraints, badParameters2))

    /* FIXME: there is no `n >= tile` constraint collected
    val badParameters3 = Map(
      TuningParameter("vec") -> (8: Nat),
      TuningParameter("tile") -> (64: Nat)
    )
    assert(!autotune.checkConstraints(constraints, badParameters3))
    */

    val goodParameters = Map(
      TuningParameter("vec") -> (4: Nat),
      TuningParameter("tile") -> (16: Nat)
    )
    assert(autotune.constraints.checkConstraints(constraints, goodParameters))
    rise.core.substitute.natsInExpr(goodParameters.toMap[Nat, Nat], e)
  }

  test("wrapOclRun") {
    val wrapped = wrapOclRun(LocalSize(1), GlobalSize(32))(util.expressions.convolution.convolution)
    assert(util.expressions.convolution.convolutionOcl.toExpr =~= wrapped)

    val e = (wrapped: ToBeTyped[Expr]) (32)
    assert(util.expressions.convolution.convolutionOcl(32).toExpr =~= e.toExpr)
  }

  // FIXME: make running this optional when hypermapper not installed?
  ignore("search") {
    // test full tuning run
    val e: Expr = util.expressions.convolution.convolutionOcl

    val tuner = Tuner(
      name = "convolution",
      hostCode = util.hostcode.convolution(32),
      inputSizes = Seq(32),
      output = "autotuning/convolution", // folder to store output files in
      saveToFile = true
    )

    val tuningResult = autotune.search(tuner)(e)

    println("tuningResult: \n")
    tuningResult.samples.foreach(elem => println(elem))

    val bestSample = autotune.getBest(tuningResult.samples)
    println("bestSample: \n" + bestSample)
  }

  // test Hypermapper constraints support
  // needs access to hypermapper_dev repository
  ignore("search experimental") {
    val e: Expr = util.expressions.convolution.convolutionOclGsLs(1024)

    val tuner = Tuner(
      hostCode = util.hostcode.convolution(1024),
      samples = 100,
      name = "RISE",
      output = "autotuning",
      timeouts = Timeouts(5000, 5000, 5000),
      executionIterations = 10,
      speedupFactor = 100,
      configFile = None,
      hmConstraints = false
    )

    val tuningResult = autotune.search(tuner)(e)

    println("tuningResult: \n")
    tuningResult.samples.foreach(elem => println(elem))

    val bestSample = autotune.getBest(tuningResult.samples)
    println("bestSample: \n" + bestSample)

    autotune.saveSamples("autotuning/RISE.csv", tuningResult)
  }
}
