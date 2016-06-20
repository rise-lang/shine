
import Core._
import DSL._
import Compiling._
import Core.PhraseType.->
import ExpPatterns._
import apart.arithmetic._
import opencl.generator.OpenCLPrinter

object scal extends App {

  val N = SizeVar("N")
  val dataT = float
  val inputT = ExpType(ArrayType(N, dataT))

  val high_level = λ(inputT)(input => λ(ExpType(dataT))(alpha =>
    map(λ( x => alpha * x ), input)
  ) )

  TypeChecker(high_level)

  println("High-Level:\n" + PrettyPrinter(high_level))

}
