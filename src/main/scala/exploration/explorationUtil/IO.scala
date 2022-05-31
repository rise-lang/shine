package exploration.explorationUtil

import rise.core.DSL.ToBeTyped
import rise.core.Expr

import scala.reflect.runtime._
import scala.tools.reflect.ToolBox


object IO {


  def main(args: Array[String]): Unit = {
    val name = "mm"
    val kind = "ocl"
    val expr =
      """

// todo make import generic
import rise.core.DSL.{fun, lf32}
import rise.core.primitives._
import rise.core.types.DataType.{ArrayType, DataTypeIdentifier, f32}
import rise.core.equality.{exprAlphaEq, typeAlphaEq, typeErasure}
import rise.core.{App, DepApp, DepLambda, Expr, Identifier, Lambda, Literal, Opaque, Primitive, TypeAnnotation, TypeAssertion}

      val N = 1 << 9

      // define matrix-matrix multiplication in RISE
        fun(ArrayType(N, ArrayType(N, f32)))(a =>
          fun(ArrayType(N, ArrayType(N, f32)))(b =>
            a |> map(fun(ak =>
              b |> transpose |> map(fun(bk =>
                zip(ak)(bk) |>
                  map(fun(x => fst(x) * snd(x))) |>
                  reduce(add)(lf32(0.0f))))))))

      """

    val mirror = universe.runtimeMirror(getClass.getClassLoader)
    val tb = mirror.mkToolBox()
    val code =
      s"""
$expr
"""
    val parseStart = System.currentTimeMillis()
    val tree = tb.parse(code)
    val parse = System.currentTimeMillis() - parseStart

    val evalStart = System.currentTimeMillis()
    val test = tb.eval(tree)
    val evalTime = System.currentTimeMillis() - evalStart

    val mm_expr = test.asInstanceOf[ToBeTyped[Expr]]


    println("test: \n" + test)
    println("mm_expr: \n" + mm_expr)

    println("parse: " + parse)
    println("evalTime: " + evalTime)
  }
}


