package shine.openmp

import rise.core.DSL._
import rise.core.types._
import rise.openMP.primitives._
import util.gen

class canonicalForm extends test_util.Tests {

  test("mapPar generated canonical loop form") {
    val expr: rise.core.Expr =
      depFun((n: Nat) =>
        fun(ArrayType(n, f32))(input =>
          input |> mapPar(fun(x => x + lf32(1)))))

    val code = gen.openmp.function.asStringFromExpr(expr)

    println(code)

    checkCanonicalLoopFrom(code)
  }

  def checkCanonicalLoopFrom(code: String): Unit = {
    val forLoopPattern = "\\bfor\\s*\\((.*);(.*);(.*)\\)".r

    forLoopPattern.findFirstMatchIn(code) match {
      case None => assert(false, "No for loop found")
      case Some(m) =>
        assert(checkInitExpr(m.group(1)), s"init expr '${m.group(1)}' is not valid")
        assert(checkTestExpr(m.group(2)), s"test expr '${m.group(2)}' is not valid")
        assert(checkIncExpr(m.group(3)), s"inc expr '${m.group(3)}' is not valid")
    }
  }

  def checkInitExpr(expr: String): Boolean = {
    val assignPattern = "^(\\s*)\\w*(\\s*)=(\\s*)\\w*(\\s*)$".r
    val integerAssignPattern = "^(\\s*)int(\\s*)\\w*(\\s*)=(\\s*)\\w*(\\s*)$".r

    assignPattern.findFirstMatchIn(expr).isDefined ||
      integerAssignPattern.findFirstMatchIn(expr).isDefined
  }

  def checkTestExpr(expr: String): Boolean = {
    val pattern = "^(\\s*)\\w*(\\s*)[<>][=]?(\\s*)\\w*(\\s*)$".r
    pattern.findFirstMatchIn(expr).isDefined
  }

  def checkIncExpr(expr: String): Boolean = {
    val incDecPattern = "^\\s*\\+\\+\\w*\\s*$|^\\s*--\\w*\\s*$|^\\s*\\w*\\+\\+\\s*$|^\\s*\\w*--\\s*$".r
    val compoundPatterns = "^\\s*\\w*\\s*\\+=\\s*\\w*\\s*$|^\\s*\\w*\\s*-=\\s*\\w*\\s*$".r
    val assignPatters = "^\\s*\\w*\\s*=\\s*\\w*\\s*\\+\\s*\\w*\\s*$|^\\s*\\w*\\s*=\\s*\\w*\\s*-\\s*\\w*\\s*$".r

    incDecPattern.findFirstMatchIn(expr).isDefined ||
      compoundPatterns.findFirstMatchIn(expr).isDefined ||
        assignPatters.findFirstMatchIn(expr).isDefined
  }

}
