
import Core._
import DSL._
import Compiling._
import Core.PhraseType.->
import ExpPatterns._
import apart.arithmetic._
import opencl.generator.OpenCLPrinter

object dot extends App {

  val N = SizeVar("N")
  val xsT = ExpType(ArrayType(N, float))
  val ysT = ExpType(ArrayType(N, float))

  val mult = λ( x => x._1 * x._2 )
  val add = λ( x => λ( a => x + a))

  val high_level = λ(xsT)(xs => λ(ysT)(ys =>
    reduce(add, 0.0f) o map(mult) $ zip(xs, ys)
  ) )

  TypeChecker(high_level)

  println("High-Level:\n" + PrettyPrinter(high_level))

}
