// ignore this for now

//package exploration.explorationUtil
//
//import rise.core.DSL.ToBeTyped
//import rise.core.Expr
//
//import scala.reflect.runtime._
//import scala.tools.reflect.ToolBox
//
//import rise.core.DSL.{fun, lf32}
//import rise.core.primitives._
//import rise.core.types.DataType.{ArrayType, DataTypeIdentifier, f32}
//import rise.elevate.rules.lowering.lowerToC
//import rise.elevate.strategies.traversal.everywhere
//import rise.elevate.rules.algorithmic.fuseReduceMap
//import elevate.core._
//import elevate.heuristic_search.util.{Solution, hashSolution}
//import rise.autotune.HostCode
//import rise.core.equality.{exprAlphaEq, typeAlphaEq, typeErasure}
//import rise.core.{App, DepApp, DepLambda, Expr, Identifier, Lambda, Literal, Opaque, Primitive, TypeAnnotation, TypeAssertion}
//import rise.elevate.Rise
//import rise.elevate.rules.traversal.alternative
//import rise.elevate.rules.traversal.default._
//import rise.elevate.strategies.normalForm.DFNF
//import rise.elevate.strategies.tiling.tile
//import rise.elevate.strategies.traversal._
//
//import java.io.{File, FileOutputStream, PrintWriter}
//import java.io.{File, FileInputStream, FileReader}
//
//
//import rise.core.DSL.ToBeTyped
//import rise.core.Expr
//
//import scala.reflect.runtime._
//import scala.tools.reflect.ToolBox
//
//object IO {
//
//
//  val importSolution: (String) => Solution[Rise] = (filename) => {
//
//    println("read in")
//    val expr = scala.io.Source.fromFile("exploration/mm.rise").mkString
//    println("finsihed")
//
//    val mirror = universe.runtimeMirror(getClass.getClassLoader)
//    val tb = mirror.mkToolBox()
//
//    val code =
//      s"""
//    import rise.core.DSL.{fun, lf32}
//    import rise.core.primitives._
//    import rise.core.types._
//    import rise.core.equality.{exprAlphaEq, typeAlphaEq, typeErasure}
//    import rise.core._
//
//${expr}
//"""
//
//
//    //    val parseStart = System.currentTimeMillis()
//    println("prase")
//    val tree = tb.parse(code)
//    println("finsihed")
//    //    val parse = System.currentTimeMillis() - parseStart
//
//    //    val evalStart = System.currentTimeMillis()
//    println("eval")
//    val test = tb.eval(tree)
//    println("finished")
//    //    val evalTime = System.currentTimeMillis() - evalStart
//
//    val mm_expr = test.asInstanceOf[ToBeTyped[Expr]]
//
//
//    println("later")
//
//    Solution(mm_expr, null)
//  }
//
//  val exportSolution: (Solution[Rise], String) => Unit = (solution, filename) => {
//
//    val hash = hashSolution(solution)
//    val string = rise.core.showScala.expr(solution.expression)
//
//    // make file override
//
//    val pwExpr = new PrintWriter(new FileOutputStream(new File(filename + "/" + hash + ".rise"), false))
//    pwExpr.write(string)
//    pwExpr.close
//
//    val pwStrat = new PrintWriter(new FileOutputStream(new File(filename + "/" + hash + ".strategy"), false))
//    pwStrat.write(solution.strategies.mkString("\n"))
//    pwStrat.close
//
//  }
//
//  val importExport = (importSolution, exportSolution)
//


//
//
//
//  def main(args: Array[String]): Unit = {
//    val name = "mm"
//    val kind = "ocl"
//    val expr =
//      """
//
//// todo make import generic
//import rise.core.DSL.{fun, lf32}
//import rise.core.primitives._
//import rise.core.types.DataType.{ArrayType, DataTypeIdentifier, f32}
//import rise.core.equality.{exprAlphaEq, typeAlphaEq, typeErasure}
//import rise.core.{App, DepApp, DepLambda, Expr, Identifier, Lambda, Literal, Opaque, Primitive, TypeAnnotation, TypeAssertion}
//
//      val N = 1 << 9
//
//      // define matrix-matrix multiplication in RISE
//        fun(ArrayType(N, ArrayType(N, f32)))(a =>
//          fun(ArrayType(N, ArrayType(N, f32)))(b =>
//            a |> map(fun(ak =>
//              b |> transpose |> map(fun(bk =>
//                zip(ak)(bk) |>
//                  map(fun(x => fst(x) * snd(x))) |>
//                  reduce(add)(lf32(0.0f))))))))
//
//      """
//
//    val mirror = universe.runtimeMirror(getClass.getClassLoader)
//    val tb = mirror.mkToolBox()
//    val code =
//      s"""
//$expr
//"""
//    val parseStart = System.currentTimeMillis()
//    val tree = tb.parse(code)
//    val parse = System.currentTimeMillis() - parseStart
//
//    val evalStart = System.currentTimeMillis()
//    val test = tb.eval(tree)
//    val evalTime = System.currentTimeMillis() - evalStart
//
//    val mm_expr = test.asInstanceOf[ToBeTyped[Expr]]
//
//
//    println("test: \n" + test)
//    println("mm_expr: \n" + mm_expr)
//
//    println("parse: " + parse)
//    println("evalTime: " + evalTime)
//  }
//}


