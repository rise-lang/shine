
import Core._
import DSL._
import Compiling._
import Core.OperationalSemantics.VectorData
import Core.PhraseType.->
import ExpPatterns._
import apart.arithmetic._
import opencl.generator.OpenCLPrinter

object asum extends App {

  val reorderWithStride = (s: ArithExpr) => {
      (i: ArithExpr, t: DataType) => {
        val n = ir.Type.getLength(DataType.toType(t)) /^ s
        (i / n) + s * (i % n)
      }
    }

  val N = SizeVar("N")
  val inputT = ExpType(ArrayType(N, float))

  val abs = λ( x => `if`(x < 0.0f, -x, x))
  val add = λ( x => λ( a => x + a))

  val high_level = λ(inputT)(input =>
    reduce(add, 0.0f) o map(abs) $ input
  )

  TypeChecker(high_level)

  println(high_level)

  println(VisitAndRebuild.copy(high_level))

  println("High-Level:\n" + PrettyPrinter(high_level))


  println("-- high-level --")
  println(OpenCLPrinter()((new ToOpenCL(localSize = 128, globalSize = N))(
    high_level, identifier("input", inputT))))
  println("----------------")


  val intelDerivedNoWarp1 = λ(inputT)(input =>
    mapWorkgroup(
      /*asScalar() o */ mapLocal(
        reduceSeq(λ(x => λ(a => abs(x) + a)), /* asVector(4) */ 0.0f)
      ) o split(8192) /* o asVector(4) */
    ) o split(32768) $ input
  )
  TypeChecker(intelDerivedNoWarp1)

  println("-- Intel Derived No Warp 1 --")
  println(OpenCLPrinter()((new ToOpenCL(localSize = 128, globalSize = N))(
    intelDerivedNoWarp1, identifier("input", inputT))))
  println("----------------")

  val intelDerived2 = λ(inputT)(input =>
    mapWorkgroup(
      mapLocal(
        reduceSeq(add, 0.0f)
      ) o split(2048)
    ) o split(2048) $ input
  )
  TypeChecker(intelDerived2)

  println("-- Intel Derived 2 --")
  println(OpenCLPrinter()((new ToOpenCL(localSize = 128, globalSize = N))(
    intelDerived2, identifier("input", inputT))))
  println("----------------")

  val nvidiaDerived1 = λ(inputT)(input =>
    mapWorkgroup(
      mapLocal(
        reduceSeq(add, 0.0f)
      ) o split(2048) o gather(reorderWithStride(128))
    ) o split(2048 * 128) $ input
  )
  TypeChecker(nvidiaDerived1)

  println("-- Nvidia Derived 1 --")
  println(OpenCLPrinter()((new ToOpenCL(localSize = 128, globalSize = N))(
    nvidiaDerived1, identifier("input", inputT))))
  println("----------------")

  val nvidiaDerived2 = λ(inputT)(input =>
    mapWorkgroup(
      toLocal(iterate(6,
        mapLocal(reduceSeq(add, 0.0f)) o
        split(2)
      ))
      o
      toLocal(mapLocal(
        reduceSeq(add, 0.0f)
      )) o split(128)
    ) o split(8192) $ input
  )
  TypeChecker(nvidiaDerived2)

  println("-- Nvidia Derived 2 --")
  println(OpenCLPrinter()((new ToOpenCL(localSize = ?, globalSize = N))(
    nvidiaDerived2, identifier("input", inputT))))
  println("----------------")

  val amdDerived1 = λ(inputT)(input =>
    mapWorkgroup(
      asScalar() o mapLocal(
        reduceSeq(λ( x => λ( a => x + a)), vectorize(2, 0.0f))
      ) o split(2048) o gather(reorderWithStride(64)) o asVector(2)
    ) o split(4096 * 128) $ input
  )
  TypeChecker(amdDerived1)

  println("-- AMD Derived --")
  println(OpenCLPrinter()((new ToOpenCL(localSize = 128, globalSize = N))(
    amdDerived1, identifier("input", inputT))))
  println("----------------")

}
