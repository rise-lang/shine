
import Core._
import DSL._
import Compiling._
import Core.PhraseType.->
import ExpPatterns._
import apart.arithmetic._
import opencl.generator.OpenCLPrinter

object gemv extends App {

  val N = SizeVar("N")
  val M = SizeVar("M")
  val dataT = float
  val xsT = ExpType(ArrayType(N, dataT))
  val ysT = ExpType(ArrayType(M, dataT))
  val matT = ExpType(ArrayType(M, ArrayType(N, dataT)))

  val mult = λ( x => x._1 * x._2 )
  val add = λ( x => λ( a => x + a))
  val scal = λ(xs => λ(alpha => map(λ( x => alpha * x ), xs) ) )
  val dot = λ(xs => λ(ys => reduce(add, 0.0f) o map(mult) $ zip(xs, ys) ) )

  val high_level =
    λ(matT)(mat => λ(xsT)(xs => λ(ysT)(ys =>
      λ(ExpType(dataT))(alpha => λ(ExpType(dataT))(beta =>

        map(λ( x => x._1 + x._2 )) $
        zip(map(λ(row => alpha * dot(row)(xs)), mat), scal(ys)(beta))

    ) ) ) ) )

  TypeChecker(high_level)

  println("High-Level:\n" + PrettyPrinter(high_level))

}
