package shine.OpenCL

import rise.core.DSL._
import rise.core.Expr
import rise.core.types._
import rise.openCL.DSL._
import util.gen

class OpenCLSegmentedReduce extends shine.test_util.Tests {

  private def xsT(N: NatIdentifier) = ArrayType(N, int)
  private def isT(N: NatIdentifier, K: NatIdentifier) = ArrayType(N, IndexType(K))

  val add = fun(x => fun(a => x + a))
  def id: Expr = fun(x => x)

  test("OpenCL Segmented Reduce Test") {

    val oclSegmentedReduceTest = nFun(n => nFun(k => fun(xsT(k))(hist => fun(isT(n, k))(is => fun(xsT(n))(xs =>
      zip(is)(xs) |>
        split(1024) |>
        mapWorkGroup(
          mapLocal(id) >> toLocal >>
          oclSegmentedReduce(rise.core.types.AddressSpace.Local)(add)(
            hist |>
              mapLocal(id)
          ) >> mapSeq(id)
        )
    )))))

    gen.OpenCLKernel(oclSegmentedReduceTest)
  }

}
