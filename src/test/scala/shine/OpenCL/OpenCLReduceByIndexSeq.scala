package shine.OpenCL

import rise.core.DSL._
import rise.core.Expr
import rise.core.TypeLevelDSL.implN
import rise.core.types._
import rise.openCL.DSL._
import util.gen

class OpenCLReduceByIndexSeq extends shine.test_util.TestsWithExecutor {

  private def xsT(N: NatIdentifier) = ArrayType(N, int)
  private def isT(N: NatIdentifier, K: NatIdentifier) = ArrayType(N, IndexType(K))
  private def histosT(N: NatIdentifier, M: NatIdentifier) = ArrayType(N, ArrayType(M, int))

  private val add = fun(x => fun(a => x + a))
  def id: Expr = fun(x => x)

  val n = 1000
  val k = 21

  val values = new Array[Int](n)
  val indices = new Array[Int](n)
  val result = new Array[Int](k)

  val r = new scala.util.Random
  var index = 0

  for(i <- 0 until n) {
    index = r.nextInt(k)
    indices(i) = index
    values(i) = i
    result(index) = result(index) + i
  }

  test("Reduce By Index Seq Test") {

    val reduceHistos = implN(n => implN(numBins => fun(histosT(n, numBins))(histos =>
      histos |> // n.numBins.int
        oclReduceSeq(rise.core.types.AddressSpace.Global)(
          fun(acc_histo => // numBins.int
            fun(cur_histo => // numBins.int
              zip(acc_histo)(cur_histo) |> // 2.numBins.int
                mapGlobal(fun(x => fst(x) + snd(x)))
            )
          )
        )(
          generate(fun(IndexType(numBins))(_ => l(0))) |> // numBins.int
            mapSeq(id) //numBins.int
        ) |>
        mapSeq(id)
    )))

    val reduceByIndexSeqTest = nFun(n => nFun(k => fun(isT(n, k))(is => fun(xsT(n))(xs =>
      zip(is)(xs) |>
      split(50) |>
      mapGlobal(
        oclReduceByIndexSeq(rise.core.types.AddressSpace.Global)(add)(
          generate(fun(IndexType(k))(_ => l(0))) |>
            mapSeq(id)
        ) >>
        mapSeq(id)
      ) |>
      toGlobal |>
      reduceHistos
    ))))

    val output = runKernel(reduceByIndexSeqTest)(LocalSize(1), GlobalSize(1))(n, k, indices, values)

    println("\nResult: ")
    print(output.deep.mkString(" "))

    println("\nExpected: ")
    for(i <- 0 until k) {
      print(result(i) + " ")

      assert(output(i) == result(i))
    }
  }

  def runKernel(kernel: Expr)(
    localSize: LocalSize,
    globalSize: GlobalSize)(
    n: Int,
    k: Int,
    indices: Array[Int],
    values: Array[Int]
  ): Array[Int] = {
    val runKernel = gen
      .OpenCLKernel(kernel)
      .as[ScalaFunction `(` Int `,` Int `,` Array[Int] `,` Array[Int]`)=>` Array[Int]]
    val (output, _) = runKernel(localSize, globalSize)(n `,` k `,` indices `,` values)
    output
  }

}
