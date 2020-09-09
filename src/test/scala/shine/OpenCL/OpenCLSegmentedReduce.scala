package shine.OpenCL

import rise.core.DSL._
import rise.core.types._
import rise.openCL.DSL._
import util.gen

class OpenCLSegmentedReduce extends shine.test_util.Tests {

  private def xsT(N: NatIdentifier) = ArrayType(N, int)
  private def isT(N: NatIdentifier, K: NatIdentifier) = ArrayType(N, IndexType(K))

  val add = fun(x => fun(a => x + a))

  test("OpenCL Segmented Reduce Test") {

    val oclSegmentedReduceTest = nFun(n => nFun(k => fun(xsT(k))(hist => fun(isT(n, k))(is => fun(xsT(n))(xs =>
      zip(is)(xs) |>
      oclSegmentedReduce(rise.core.types.AddressSpace.Global)(add)(
        hist |>
          mapSeq(fun(x => x))
      ) |>
        mapSeq(fun(x => x))
    )))))

    gen.OpenCLKernel(oclSegmentedReduceTest)
  }

}
