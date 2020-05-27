package rise.core

import rise.core.types._
import rise.core.DSL._
import rise.OpenCL.DSL._

class showScalaTest extends rise.test_util.Tests {
  def prefixImports(code: String): String =
    s"""
      |import rise.core._
      |import rise.core.types._
      |import rise.core.semantics._
      |import rise.core.DSL._
      |import rise.OpenCL.DSL._
      |import AddressSpace._
      |import arithexpr.arithmetic._
      |
      |${code}
      |""".stripMargin

  private val dotElemWeights = fun((weights, elem) =>
    oclReduceSeqUnroll(AddressSpace.Private)(fun((acc, pair) => {
      val pixel = pair._1
      val weight = pair._2
      acc + (pixel * weight)
    }))(l(0.0f))(zip(join(elem))(weights))
  )

  test("show dotElemWeights as an example") {
    import scala.reflect.runtime.universe
    import scala.tools.reflect.ToolBox

    val typedDotElemWeights = infer(dotElemWeights)

    val untypedScala = prefixImports(showScala.expr(dotElemWeights))
    val typedScala = prefixImports(showScala.expr(typedDotElemWeights))

    println(untypedScala)
    println(typedScala)

    val toolbox = universe.runtimeMirror(getClass.getClassLoader).mkToolBox()
    val expr = toolbox.eval(toolbox.parse(untypedScala)).asInstanceOf[Expr]
    val typedExpr = toolbox.eval(toolbox.parse(typedScala)).asInstanceOf[Expr]

    println(expr)
    println(typedExpr)

    assert(expr == dotElemWeights)
    assert(typedExpr == typedDotElemWeights)
  }
}
