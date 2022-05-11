package apps.autotuning

import apps.gemv.ocl._
import apps.mm.dot
import apps.separableConvolution2D.mulT
import apps.tvmGemm.{innermost, outermost, par}
import arithexpr.arithmetic.RangeMul
import rise.autotune
import rise.autotune._
import rise.core.DSL.Type._
import rise.core.DSL._
import rise.core._
import rise.core.types._
import shine.OpenCL.{GlobalSize, LocalSize}
import elevate.core._
import elevate.core.strategies.basic._
import elevate.core.strategies.traversal._
import exploration.runner.CExecutor
import rise.core.DSL._
import rise.core.primitives._
import rise.core.types.DataType._
import rise.elevate._
import rise.elevate.rules.algorithmic._
import rise.elevate.rules.lowering._
import rise.elevate.rules.traversal._
import rise.elevate.rules.traversal.default._
import rise.elevate.strategies.algorithmic.reorder
import rise.elevate.strategies.lowering._
import rise.elevate.strategies.normalForm._
import rise.elevate.strategies.predicate._
import rise.elevate.strategies.tiling._
import rise.elevate.strategies.traversal
import rise.elevate.strategies.traversal._
import elevate.heuristic_search.util.{IOHelper, Solution}

import scala.collection.immutable
import scala.collection.mutable.ListBuffer
import arithexpr.arithmetic.{ArithExpr, RangeUnknown}
import rise.autotune.configFileGeneration._
import rise.autotune.constraints._
import rise.autotune.execution._
import rise.core.DSL.Type.NatFunctionWrapper
import rise.core._
import rise.core.types._
import rise.openCL.DSL.oclRun
import shine.OpenCL.{GlobalSize, LocalSize}

import java.io.{File, FileOutputStream, PrintWriter}
import java.util
import scala.collection.mutable.ListBuffer
import scala.language.postfixOps
import scala.sys.process._

class mmTVMTuning extends test_util.Tests {

  // tvm gemm
  val N = 1024

  // todo check input vars, maybe dep fun
  val mm: Expr = //infer(
    fun(ArrayType(N, ArrayType(N, f32)))(a =>
      fun(ArrayType(N, ArrayType(N, f32)))(b =>
        a |> map(fun(ak =>
          transpose(b) |> map(fun(bk =>
            zip(ak)(bk) |>
              map(fun(x => fst(x) * snd(x))) |>
              reduce(add)(lf32(0.0f))
          ))
        ))
      ))

  // -- BASELINE ---------------------------------------------------------------

  val baseline: Strategy[Rise] = DFNF()(default.RiseTraversable) `;`
    (fuseReduceMap `@` topDown[Rise])

  // -- BLOCKING ---------------------------------------------------------------

  val isFullyAppliedReduce: Strategy[Rise] = isApplied(isApplied(isApplied(isReduce)))

  val isFullyAppliedMap: Strategy[Rise] = isApplied(isApplied(isMap))

  // -- LOOP PERMUTATION -------------------------------------------------------

  val loopPerm: Strategy[Rise] = baseline `;`
    (tile() `@` outermost(mapNest(2))) `;;`
    (reduceMapFission() `@` outermost(isApplied(isApplied(isReduceSeq)))) `;;`
    (tunable("split", splitStrategy) `@` innermost(isFullyAppliedReduce)) `;;`
    reorder(List(1, 2, 5, 3, 6, 4)) `;;`
    (tunable("vec", vectorize) `@` innermost(isFullyAppliedMap))

  // just apply loopPerm with tuned values

  val mm2: Rise = loopPerm.apply(mm).get

  val mm_par = par.apply(mm)
  val gold = lowerToC.apply(mm_par.get).get


  // arguments for stuff
  //  Map[String, Int]
  //  val testMap[String, List[Int]]
  // get from ech

  // tune parameterized strategies

  def inject(e: Expr, tuningParameterMap: Map[String, Int], permutationMap: Map[String, List[Int]]): Either[String, Expr] = {

    val tileX = tuningParameterMap.get("tileX").get
    val tileY = tuningParameterMap.get("tileY").get
    val split = tuningParameterMap.get("split").get
    val reordering = permutationMap.get("reordering").get
    val vec = tuningParameterMap.get("vec").get

    val loopPerm: Strategy[Rise] = baseline `;`
      (tile(tileX, tileY) `@` outermost(mapNest(2))) `;;`
      (reduceMapFission() `@` outermost(isApplied(isApplied(isReduceSeq)))) `;;`
      (splitStrategy(split) `@` innermost(isFullyAppliedReduce)) `;;`
      reorder(reordering) `;;`
      (vectorize(vec) `@` innermost(isFullyAppliedMap))

    loopPerm.apply(e) match {
      case Success(expression) => Right(expression)
      case Failure(value) => Left("Error")
    }
  }



  // yo
  //  mm2

  // use CExecutor?

  // how to inject tps?

  test("parse permutationvar") {
    //    val request = "(1, 2, 3),1024.0,1.0,16.0,32.0,(5, 4, 3, 0, 1, 2),(1, 2, 4), 1024.0, 32.0,(1, 2, 3)".replaceAll(" +", "")
    val request = "1024.0,1.0,16.0,32.0,(5, 4, 3, 0, 1, 2)".replaceAll(" +", "")
    //    val request = "1024.0,1.0,16.0,32.0,(5, 4, 3, 0, 1, 2)".r

    val header = "tuned_tile51,tuned_tile52,tuned_vec101,tuned_split100,tuned_reorder".split(",").map(x => x.trim())

    //    val test = """\[[^\]]+\]""".r
    println("request: " + request)

    //    case class Uwe
    //    case class Test
    type defaultTPs = Seq[Int]
    type permutationTP = Seq[Int]

    //    val test5 = request.replaceAll("""(\()([^\)]*)(\)(,?))""", "")
    //    val test5 = request.replaceAll("""(\()([^\)]*)(\)(,?))""", "")

    val pattern = """(\()([^\)]*)(\))""".r


    // use this as parsing object
    case class ParsedValues(tps: Seq[Int], perm: Seq[Seq[Int]])

    // perms
    val test6 = pattern.findAllIn(request)

    //    test6

    //    val pattern(request2) = request

    //    println("request2: " + request2)

    println("matched: " + test6.mkString(","))

    val test7 = request.split("""\([^\)]*\)""").toSeq

    val it = request.replaceAll(""" +""", "").split(",").iterator

    // parse beginning
    //

    var output = scala.collection.Seq.empty[String]
    //    var output = new ListBuffer[String]

    while (it.hasNext) {
      var value = scala.collection.Seq.empty[String]
      val elem = it.next().replaceAll(",", "")
      println("elem: " + elem)
      elem match {
        case x if x.contains("(") => {
          value = value ++ scala.collection.Seq(x.replaceAll("""\(""", ""))
          // add until )
          var inPerm = true
          while (it.hasNext && inPerm) {
            val perm2 = it.next().replaceAll(",", "")
            println("perm2: " + perm2)

            perm2 match {
              case y if y.contains(")") =>
                value = value ++ scala.collection.Seq(y.replaceAll("""\)""", ""))
                inPerm = false
              case _ => value = value ++ scala.collection.Seq(perm2)
            }
          }
        }
        case y => value = value ++ scala.collection.Seq(y)
      }


      //        .map { case (h, p) =>
      //        NatIdentifier(h) -> (p.toFloat.toInt: Nat)
      //      }.toMap

      println("value: " + value)

      output = output ++ scala.collection.Seq(value.mkString(","))
    }


    val paramMap = header.zip(output)
    println("paramMap: ")
    paramMap.foreach(println)
    println("\n")

    println("output: ")
    output.foreach(println)

    println("test7: ")
    test7.foreach(println)

    // tps
    val test5 = request.replaceAll("""(\()([^\)]*)(\))""", "").split(",").filter(elem => elem.size != 0).toSeq


    println("filtered: " + test5)
    println("matched: " + test6)
    println("matched: " + test6.mkString(","))
    println("test6: " + test6.groupNames)

    //    val parse: Either[defaultTPs, permutationTP]
    //    request.toSeq.foreach(elem => elem match {
    //      case
    //    })

    //    val headerLine = request.split("""\(|,\(|\),|\),\(,|\)""")
    val headerLine = request.split("""\(|\)""")

    println("header: ")
    headerLine.foreach(println)

    val test = headerLine.toSeq.map(elem => elem.split(",").toSeq).toSeq
    test.foreach(elem => println("elem: " + elem + " " + elem.size))

    println("\n")

    val test2 = headerLine.toSeq.filter(elem => elem.size != 0).map(elem => elem.split(",").toSeq).toSeq
    test2.foreach(elem => println("elem: " + elem + " " + elem.size))
  }


  test("test tvm gemm") {

    val params = autotune.constraints.collectParameters(mm2)
    val constr = autotune.constraints.collectConstraints(mm2, params)

    println("mm: \n" + mm2)
    println("params: " + params)
    println("constr: " + constr.mkString(", "))


    val tuner = Tuner(
      hostCode = HostCode("", "", ""),
      samples = 100,
      name = "rs_cot_1024",
      output = "autotuning/mm_128",
      timeouts = Timeouts(5000, 5000, 1000),
      executionIterations = 10,
      speedupFactor = 100,
      configFile = Some("autotuning/config/mm/rs_cot_128.json"),
      hmConstraints = true
    )


    val configFile = autotune.configFileGeneration.generateJSON(params, constr, tuner)

    println("configFile: \n" + configFile)


    // inject parameters
    val params2: Map[Nat, Nat] = Map(
      TuningParameter("tuned_tile51") -> (32: Nat),
      TuningParameter("tuned_tile52") -> (32: Nat),
      TuningParameter("tuned_vec101") -> (32: Nat),
      TuningParameter("tuned_split100") -> (4: Nat),
    )

    // problem here: Replacement of tuning params does not work
    val eSub = rise.core.substitute.natsInExpr(params2, mm2)

    // execute
    // problem2: Permutation variable

    println("eSub: \n" + eSub)

    val executor = CExecutor(
      lowering = lowerToC,
      goldExpression = gold,
      iterations = 10,
      inputSize = 1024,
      threshold = 1000,
      output = "autotuning"
    )


    val strategies = immutable.Seq.empty[Strategy[Rise]]

    println("now execute")
    val result = executor.execute(Solution(eSub, strategies))._2


    println("result: " + result)
  }

  def executeMM(e: Expr, s0: Nat) = {

    //    val result = autotune.execution.execute(
    //      expression = eSub,
    //      hostCode = HostCode(init(128, 128), compute, finish),
    //      timeouts = Timeouts(5000, 5000, 5000),
    //      executionIterations = 100,
    //      speedupFactor = 100,
    //      execution = Median
    //    )
    //    println("result: " + result.runtime)

  }

  //  test("exeute gemv version") {
  //    executeGemv(gemvBlastNTuning, 64)
  //    executeGemv(gemvBlastTTuning, 64)
  //    executeGemv(gemvFusedTuning, 64) // ignore s0 in this case
  //    executeGemv(gemvFusedAMDTuning, 128)
  //    executeGemv(gemvKeplerBestTuning, 128)
  //  }
  //
  //  test("tune gemv version") {
  //    runTuning(gemvBlastNTuning)
  //    runTuning(gemvBlastTTuning)
  //    runTuning(gemvFusedTuning) // ignore s0 in this case
  //    runTuning(gemvFusedAMDTuning)
  //    runTuning(gemvKeplerBestTuning)
  //  }


  def runTuning(e: Expr) = {
    //    //    val version = autotuning.parseName(configFile)
    //
    //    val tuner = Tuner(
    //      hostCode = HostCode(init(1024, 1024), compute, finish),
    //      inputSizes = Seq(1024, 1024, 1024),
    //      samples = 20,
    //      name = "gemv",
    //      output = s"autotuning/gemv",
    //      timeouts = Timeouts(10000, 10000, 10000),
    //      executionIterations = 10,
    //      speedupFactor = 100,
    //      configFile = None,
    //      hmConstraints = true,
    //      runtimeStatistic = Minimum,
    //      saveToFile = true
    //    )
    //
    //    autotune.search(tuner)(e)
  }
}