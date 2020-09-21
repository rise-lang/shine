package shine.OpenCL

import rise.core.DSL._
import rise.core.Expr
import rise.core.TypeLevelDSL.implN
import rise.core.types._
import rise.openCL.DSL._
import util.gen

class OpenCLSegmentedReduce extends shine.test_util.TestsWithExecutor {

  private def xsT(N: NatIdentifier) = ArrayType(N, int)
  private def isT(N: NatIdentifier, K: NatIdentifier) = ArrayType(N, IndexType(K))
  private def histosT(N: NatIdentifier, M: NatIdentifier) = ArrayType(N, ArrayType(M, int))

  val add = fun(x => fun(a => x + a))
  def id: Expr = fun(x => x)

  val n = 8192
  val k = 10
  val values = new Array[Int](n)
  val indices = new Array[Int](n)
  val bins = new Array[Int](k)
  val result = new Array[Int](k)

  private var counter = n

  val r = new scala.util.Random

  for(i <- 0 until k - 1) {
    bins(i) = r.nextInt(counter + 1)
    counter = counter - bins(i)
  }

  bins(k - 1) = counter

  println("")
  println("bins:")
  for(i <- 0 until k) {
    print(bins(i) + " ")
  }
  println("")

  counter = 0

  for(i <- 0 until k) {
    for(j <- 0 until bins(i)) {
      indices(counter + j) = i
      values(counter + j) = j + 1
    }

    counter = counter + bins(i)
  }


  test("OpenCL Segmented Reduce Test") {

    val reduceHistos = implN(n => implN(k => fun(histosT(n, k))(histos =>
      histos |> // n.numBins.int
        oclReduceSeq(rise.core.types.AddressSpace.Global)(
          fun(acc_histo => // numBins.int
            fun(cur_histo => // numBins.int
              zip(acc_histo)(cur_histo) |> // 2.numBins.int
                mapGlobal(fun(x => fst(x) + snd(x)))
            )
          )
        )(
          generate(fun(IndexType(k))(_ => l(0))) |> // numBins.int
            mapSeq(id) //numBins.int
        ) |>
        mapSeq(id)
    )))

    val oclSegmentedReduceTest = nFun(n => nFun(k => fun(isT(n, k))(is => fun(xsT(n))(xs =>
      zip(is)(xs) |>
        split(1024) |>
        mapWorkGroup(
          oclSegmentedReduce(rise.core.types.AddressSpace.Local)(add)(
            generate(fun(IndexType(k))(_ => l(0))) |>
              mapLocal(id)
          ) >> mapLocal(id)
        ) |>
        toGlobal |>
        reduceHistos
    ))))

    val output = runKernel(oclSegmentedReduceTest)(LocalSize(32), GlobalSize(32))(n, k, indices, values)

    println("\nResult: ")
    print(output.deep.mkString(" "))

    println("\nExpected: ")
    for(i <- 0 until k) {
      result(i) = bins(i) * (bins(i) + 1) / 2
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
