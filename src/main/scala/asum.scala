
import Core._
import DSL._
import Compiling._
import Core.PhraseType.->
import ExpPatterns._
import apart.arithmetic._
import opencl.generator.OpenCLPrinter

object asum extends App {

  val N = SizeVar("N")
  val inputT = ExpType(ArrayType(N, float))

  val abs = λ( x => `if`(x < 0.0f, -x, x))
  val add = λ( x => λ( a => x + a))

  val high_level = λ(inputT)(input =>
    reduce(add, 0.0f) o map(abs) $ input
  )

  TypeChecker(high_level)

  println("High-Level:\n" + PrettyPrinter(high_level))

}
