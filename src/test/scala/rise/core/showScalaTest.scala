package rise.core

import rise.core.dsl._
import rise.core.exprs.Expr
import rise.core.exprs.primitives._
import rise.core.types._
import rise.core.util.show.showScala
import rise.opencl.primitives.oclReduceSeqUnroll

class showScalaTest extends test_util.Tests {
  def prefixImports(code: String): String =
    s"""
       |import rise.core.exprs._
       |import rise.core.exprs.primitives._
       |import rise.core.types._
       |import rise.core.types.AddressSpace._
       |import rise.core.util._
       |import rise.core.semantics._
       |import rise.opencl.dsl._
       |import rise.opencl.primitives.oclReduceSeqUnroll
       |import arithexpr.arithmetic._
       |
       |$code
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

    val typedDotElemWeights = dotElemWeights.toExpr

    val untypedScala = prefixImports(showScala.expr(dotElemWeights))
    val typedScala = prefixImports(showScala.expr(typedDotElemWeights))

    println(untypedScala)
    println(typedScala)

    val toolbox = universe.runtimeMirror(getClass.getClassLoader).mkToolBox()
    val expr = toolbox.eval(toolbox.parse(untypedScala)).asInstanceOf[Expr]
    val typedExpr = toolbox.eval(toolbox.parse(typedScala)).asInstanceOf[Expr]

    println(expr)
    println(typedExpr)

    assert(expr == dotElemWeights.toUntypedExpr)
    assert(typedExpr == typedDotElemWeights)
  }
}
