
import Core._
import DSL._
import Compiling._
import Core.PhraseType.->
import ExpPatterns._
import apart.arithmetic._
import opencl.generator.OpenCLPrinter

object dot extends App {

  val reorderWithStride = (s: ArithExpr) => {
    (i: ArithExpr, t: DataType) => {
      val n = ir.Type.getLength(DataType.toType(t)) /^ s
      (i / n) + s * (i % n)
    }
  }

  val N = SizeVar("N")
  val xsT = ExpType(ArrayType(N, float))
  val ysT = ExpType(ArrayType(N, float))

  def printOpenCLKernel1(name: String,
                         lambda: Phrase[ExpType ->(ExpType -> ExpType)]) = {
    TypeChecker(lambda)
    println(name + ":\n" + PrettyPrinter(lambda))

    println(s"-- $name --")
    println(OpenCLPrinter()((new ToOpenCL(localSize = 128, globalSize = N)) (
      lambda, identifier("xs", xsT), identifier("ysY", ysT))))
    println("----------------")
  }

  def printOpenCLKernel2(name: String,
                         lambda: Phrase[ExpType -> ExpType]) = {
    println(name + ":\n" + PrettyPrinter(lambda))
    println(TypeChecker(lambda))

    println(s"-- $name --")
    println(OpenCLPrinter()((new ToOpenCL(localSize = 128, globalSize = N)) (
      lambda, identifier("in", xsT))))
    println("----------------")
  }

  val mult = λ( x => x._1 * x._2 )
  val add = λ( x => λ( a => x + a))

  val high_level = λ(xsT)(xs => λ(ysT)(ys =>
    reduce(add, 0.0f) o map(mult) $ zip(xs, ys)
  ) )

  printOpenCLKernel1("High-Level", high_level)

  val dotSimple = λ(xsT)(xs => λ(ysT)(ys =>
    join() o mapWorkgroup(
      mapLocal(
        reduceSeq(add, 0.0f) o mapSeq(mult)
      ) o split(4)
    ) o split(1024) $ zip(xs, ys)
  ) )

  printOpenCLKernel1("dotSimple", dotSimple)

  val dotCPU1 = λ(xsT)(xs => λ(ysT)(ys =>
    join() o mapWorkgroup(
      mapLocal(
        reduceSeq(λ( x => λ( a => mult(x) + a)), 0.0f)
      ) o split(2048)
    ) o split(2048*128) $ zip(xs, ys)
  ) )

  printOpenCLKernel1("dotCPU1", dotCPU1)

  val dotCPU2 = λ(xsT)(in =>
    join() o mapWorkgroup(
      mapLocal(
        reduceSeq(λ( x => λ( a => x + a)), 0.0f)
      ) o split(128)
    ) o split(128) $ in
  )

  printOpenCLKernel2("dotCPU2", dotCPU2)

  val dotProduct1 = λ(xsT)(xs => λ(ysT)(ys =>
    join() o mapWorkgroup(
      mapLocal(
        reduceSeq(λ( x => λ( a => mult(x) + a)), 0.0f)
      ) o split(2048) o gather(reorderWithStride(128))
    ) o split(2048*128) $ zip(xs, ys)
  ) )

  printOpenCLKernel1("dotProduct1", dotProduct1)

  val dotProduct2 = λ(xsT)(in =>
    join() o mapWorkgroup(
      iterate(6,
        toLocal(mapLocal(reduceSeq(add, 0.0f))) o split(2)) o
      toLocal(mapLocal(reduceSeq(add, 0.0f))) o split(2)
    ) o split(128) $ in
  )

  printOpenCLKernel2("dotProduct2", dotProduct2)

}
