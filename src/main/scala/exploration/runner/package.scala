package exploration

import arithexpr.arithmetic.{ArithExpr, RangeMul}
import elevate.core.Strategy
import elevate.heuristic_search.util.{Solution, hashProgram}
import rise.autotune
import rise.autotune.configFileGeneration.generateJSON
import rise.autotune.constraints.collectConstraints
import rise.autotune.{HostCode, Median, Timeouts, Tuner, getInputs, tuningParam, wrapOclRun}
import rise.core.Expr
import rise.core.types.{Nat, NatIdentifier}
import rise.elevate.Rise
import shine.C.AST.{BinaryExpr, Block, DeclStmt, ExprStmt, ForLoop, Literal, Stmt, Stmts}
import shine.OpenCL.{GlobalSize, LocalSize}
import util.gen

package object runner {

  def performanceModel(expression: Rise): Double = {
    val p = gen.openmp.function("riseFun").fromExpr(expression)

    // todo check case of multiple functions
    val function = p.functions(0)

    val result = price(function.code.body)

    result
  }


  def priceExpr(expr: shine.C.AST.Expr): Double = {
    // todo implement this

    1.0
  }

  // determine number of iterations
  def countForLoop(stmt: Stmt, expr: shine.C.AST.Expr, expr1: shine.C.AST.Expr): Double = {

    // todo check initial value
    // initial value (mostly 0)

    // todo check assignment

    val bound = expr match {
      case BinaryExpr(one, two, three) => three match {
        case Literal(str) => str.toInt
        case _ => 0
      }
      case _ => 0
    }

    //    println("bound: " + bound)

    //    512
    bound
  }

  def price(stmt: Stmt): Double = {
    //    println()

    stmt match {

      // just call
      case Block(value) => // recursive for each statement of block
        //        println("block: " + value)
        value.map(elem => price(elem)).reduceLeft(_ + _)

      case Stmts(stmt, stmt1) => // recursive for each elem
        //        println("Stmts: " + stmt + " - " + stmt1)
        price(stmt) + price(stmt1)

      // count iterations
      case ForLoop(stmt, expr, expr1, stmt1) => // count iterations, then recursive on body
        //        println("for loop: " + stmt + " - " + expr + " - " + expr1 + " - " + stmt1)
        price(stmt) + countForLoop(stmt, expr, expr1) * (priceExpr(expr) + priceExpr(expr1) + price(stmt1))

      // count
      case DeclStmt(decl) =>
        //        println("declStmt: " + decl)
        1.0 // count
      case ExprStmt(expr) =>
        //        println("exprStmt: " + expr)
        1.0 // count

      case _ => 0.0
      //
      //      // ignore
      //      case Comment(str) => // ignore
      //      // not sure for now
      //      case IfThenElse(expr, stmt, maybeStmt) => // ignore for now
      //      case break: Break => // ignore for now
      //      case Code(str) => // ignore for now
      //      case value: Return => // ignore for now
      //      case GOTO(str) => // ignore for now
      //      case continue: Continue => // ignore for now
      //      case SynchronizeWarp() => // ignore for now
      //      case SynchronizeThreads() => // ignore for now
      //      case WhileLoop(expr, stmt) => // ignore for now
      //      case Barrier(local, global) => // ignore for now
    }
  }

  def checkExpressionC(
                        lowering: Strategy[Rise]
                      )(expression: Rise): Boolean = {

    // generate code here

    // ignore numerical parameters for now

    // lower solution
    try {
      this.synchronized {

        // lower expression
        val lowered = lowering.apply(expression)

        // generate code
        val p = gen.openmp.function("riseFun").fromExpr(lowered.get)
      }

      true
    } catch {
      case e: Throwable => false
    }
  }

  def checkSolutionC(lowering: Strategy[Rise])(solution: Solution[Rise]): Boolean = {

    // generate code here

    // ignore numerical parameters for now

    // lower solution
    try {
      this.synchronized {

        // lower expression
        val lowered = lowering.apply(solution.expression())

        // generate code
        val p = gen.openmp.function("riseFun").fromExpr(lowered.get)
      }

      true
    } catch {
      case e: Throwable => false
    }


  }

  var duration: Long = 0
  var duration2: Long = 0
  var duration3: Long = 0

  def checkSolution(lowering: Strategy[Rise], solution: Solution[Rise]): Boolean = {

    // todo check expression using checking function

    val (e, lowered, loweredOclReplaced) = this.synchronized {


      // lower expression
      val e: Expr = solution.expression()
      val lowered = lowering.apply(e)
      val loweredOcl: Expr = wrapOclRun(LocalSize(1, 1), GlobalSize(1, 1))(lowered.get)


      // replace each parameter with constant 1
      val params = autotune.constraints.collectParameters(loweredOcl)
      val paramMap: Map[NatIdentifier, Nat] = params.map(param => {
        param -> (1: Nat)
      }).toMap[NatIdentifier, Nat]
      //    val loweredOclReplaced = rise.core.substitute.natsInExpr(paramMap.toMap[Nat, Nat], loweredOcl)
      (e, lowered, rise.core.substitute.natsInExpr(paramMap.toMap[Nat, Nat], loweredOcl))
    }

    // try to generate code
    try {

      this.synchronized {
        gen.opencl.hosted("fun").fromExpr(loweredOclReplaced)
      }

      // check if all values were filtered out
      object mvHostCode {
        // scalastyle:off
        val init: (Int, Int) => String = (N, M) => {
          s"""
             |const int N = ${N};
             |const int M = ${M};
             |
             |srand(time(NULL));
             |
             |Buffer inputM = createBuffer(ctx, M * N * sizeof(float), HOST_WRITE | DEVICE_READ);
             |Buffer inputV = createBuffer(ctx, N * sizeof(float), HOST_WRITE | DEVICE_READ);
             |Buffer outputV = createBuffer(ctx, M * sizeof(float), HOST_READ | DEVICE_WRITE);
             |
             |float* inM = hostBufferSync(ctx, inputM, N * M * sizeof(float), HOST_WRITE);
             |for (int i = 0; i < N * M ; i++) {
             |  inM[i] = (float)(rand());
             |}
             |
             |float* inV = hostBufferSync(ctx, inputV, N * sizeof(float), HOST_WRITE);
             |for (int i = 0; i < N; i++) {
             |  inV[i] = (float)(rand());
             |}
             |
             |""".stripMargin
        }

        val compute =
          s"""
             |fun_run(ctx, &fun, outputV, M, N, inputM, inputV);
             |""".stripMargin

        val finish =
          s"""
             |// TODO: could check output here
             |
             |destroyBuffer(ctx, inputM);
             |destroyBuffer(ctx, inputV);
             |destroyBuffer(ctx, outputV);
             |""".stripMargin
        // scalastyle:on
      }


      this.synchronized {

        val tuner = Tuner(
          hostCode = HostCode(mvHostCode.init(1024, 1024), mvHostCode.compute, mvHostCode.finish),
          inputSizes = Seq(1024, 1024),
          samples = 100,
          name = "mv",
          output = "exploration/",
          timeouts = Timeouts(100000, 100000, 100000),
          executionIterations = 10,
          runtimeStatistic = Median,
          speedupFactor = 100,
          None,
          //      Some("/home/jo/development/rise-lang/shine/autotuning/scal/scal.json"),
          //      hmConstraints = true,
          hmConstraints = false,
          saveToFile = false
        )
        // now wrap ocl
        val loweredOclTuning: Expr =
          tuningParam("ls0", RangeMul(1, 1024, 2), (ls0: Nat) =>
            tuningParam("ls1", RangeMul(1, 1024, 2), (ls1: Nat) =>
              tuningParam("gs0", RangeMul(1, 1024, 2), (gs0: Nat) =>
                tuningParam("gs1", RangeMul(1, 1024, 2), (gs1: Nat) =>
                  wrapOclRun(LocalSize(ls0, ls1), GlobalSize(gs0, gs1))(lowered.get)
                ))))


        // get parameters
        val parameters = autotune.constraints.collectParameters(loweredOclTuning)

        // get constraints and inject inputs into constraints
        val inputs = getInputs(e)
        val inputMap = (inputs zip tuner.inputSizes).toMap
        val constraints = collectConstraints(loweredOclTuning, parameters)
          .map(constraint => constraint.substitute(inputMap.asInstanceOf[Map[ArithExpr, ArithExpr]]))

        val startJson = System.currentTimeMillis()

        generateJSON(parameters, constraints, tuner)

      }
      true
    } catch {
      case e: Throwable =>
        //          println("false")
        //          println(e)
        false
    }
  }
}

