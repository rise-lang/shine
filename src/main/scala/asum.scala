
import idealised._
import idealised.Core._
import idealised.DSL.untyped._
import idealised.OpenCL.Core.ToOpenCL
import idealised.OpenCL.DSL._
import apart.arithmetic._
import opencl.generator.OpenCLPrinter

import scala.language.implicitConversions

object asum extends App {

  val N = SizeVar("N")
  val inputT = ExpType(ArrayType(N, float))

  val abs = λ(x => `if`(x < 0.0f, -x, x))
  val add = λ(x => λ(a => x + a))

  val high_level_ = λ(inputT)(input =>
    reduce(add, 0.0f) o map(abs) $ input
  )

  val high_level = TypeInference(high_level_)
  TypeChecker(high_level)

  println("High-Level:\n" + PrettyPrinter(high_level))


  val intelDerivedNoWarp1_ = λ(inputT)(input =>
    mapWorkgroup(
      /*asScalar() o */ mapLocal(
        reduceSeq(λ(x => λ(a => abs(x) + a)), /* asVector(4) */ 0.0f)
      ) o split(8192) /* o asVector(4) */
    ) o split(32768) $ input
  )
  val intelDerivedNoWarp1 = TypeInference(intelDerivedNoWarp1_)
  TypeChecker(intelDerivedNoWarp1)

  println("-- Intel Derived No Warp 1 --")
  println(OpenCLPrinter()((new ToOpenCL(localSize = 128, globalSize = N)) (
    intelDerivedNoWarp1, identifier("input", inputT))))
  println("----------------")

  val intelDerived2_ = λ(inputT)(input =>
    mapWorkgroup(
      mapLocal(
        reduceSeq(add, 0.0f)
      ) o split(2048)
    ) o split(2048) $ input
  )
  val intelDerived2 = TypeInference(intelDerived2_)
  TypeChecker(intelDerived2)

  println("-- Intel Derived 2 --")
  println(OpenCLPrinter()((new ToOpenCL(localSize = 128, globalSize = N)) (
    intelDerived2, identifier("input", inputT))))
  println("----------------")

  val nvidiaDerived1_ = λ(inputT)(input =>
    mapWorkgroup(
      mapLocal(
        reduceSeq(add, 0.0f)
      ) o split(2048) o gather(reorderWithStridePhrase(128))
    ) o split(2048 * 128) $ input
  )
  val nvidiaDerived1 = TypeInference(nvidiaDerived1_)
  Core.xmlPrinter.toFile("/tmp/p.xml", nvidiaDerived1)
  TypeChecker(nvidiaDerived1)

  println("-- Nvidia Derived 1 --")
  println(OpenCLPrinter()((new ToOpenCL(localSize = 128, globalSize = N))(nvidiaDerived1)))
  println("----------------")

  val nvidiaDerived2_ = λ(inputT)(input =>
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
  val nvidiaDerived2 = TypeInference(nvidiaDerived2_)
  TypeChecker(nvidiaDerived2)

  println("-- Nvidia Derived 2 --")
  println(OpenCLPrinter()((new ToOpenCL(localSize = ?, globalSize = N)) (
    nvidiaDerived2, identifier("input", inputT))))
  println("----------------")

  val amdDerived1_ = λ(inputT)(input =>
    mapWorkgroup(
      asScalar() o mapLocal(
        reduceSeq(λ(x => λ(a => x + a)), vectorize(2, 0.0f))
      ) o split(2048) o gather(reorderWithStridePhrase(64)) o asVector(2)
    ) o split(4096 * 128) $ input
  )
  val amdDerived1 = TypeInference(amdDerived1_)
  TypeChecker(amdDerived1)

  println("-- AMD Derived --")
  println(OpenCLPrinter()((new ToOpenCL(localSize = 128, globalSize = N)) (
    amdDerived1, identifier("input", inputT))))
  println("----------------")
}
