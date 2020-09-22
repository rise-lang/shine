package shine.OpenCL

import rise.core.DSL._
import rise.core.Expr
import rise.core.TypeLevelDSL.implN
import rise.core.types._
import rise.openCL.DSL._
import util.gen

class OpenCLReduceByIndexLocal extends shine.test_util.TestsWithExecutor {

  private def xsT(N: NatIdentifier) = ArrayType(N, int)
  private def f_xsT(N: NatIdentifier) = ArrayType(N, f32)
  private def isT(N: NatIdentifier, K: NatIdentifier) = ArrayType(N, IndexType(K))
  private def histosT(N: NatIdentifier, M: NatIdentifier) = ArrayType(N, ArrayType(M, int))
  private def f_histosT(N: NatIdentifier, M: NatIdentifier) = ArrayType(N, ArrayType(M, f32))

  private val add = fun(x => fun(a => x + a))
  def id: Expr = fun(x => x)

  val n = 1000
  val k = 21

  val indices = new Array[Int](n)

  val values = new Array[Int](n)
  val result = new Array[Int](k)

  val f_values = new Array[Float](n)
  val f_result = new Array[Float](k)

  val r = new scala.util.Random
  var index = 0

  for(i <- 0 until n) {
    index = r.nextInt(k)
    indices(i) = index
    values(i) = i
    f_values(i) = i
    result(index) = result(index) + i
    f_result(index) = f_result(index) + i
  }

  test("Reduce By Index Local Test (Multiple Histograms, Int)") {

    val reduceHistos = implN(n => implN(numBins => fun(histosT(n, numBins))(histos =>
      histos |> // n.numBins.int
        oclReduceSeq(rise.core.types.AddressSpace.Global)(
          fun(acc_histo => // numBins.int
            fun(cur_histo => // numBins.int
              zip(acc_histo)(cur_histo) |> // 2.numBins.int
                mapLocal(fun(x => fst(x) + snd(x)))
            )
          )
        )(
          generate(fun(IndexType(numBins))(_ => l(0))) |> // numBins.int
            mapLocal(id) //numBins.int
        ) |>
        mapLocal(id)
    )))

    val reduceByIndexLocalTest = nFun(n => nFun(k => fun(isT(n, k))(is => fun(xsT(n))(xs =>
      zip(is)(xs) |>
        split(50) |>
        mapWorkGroup(
          oclReduceByIndexLocal(rise.core.types.AddressSpace.Local)(add)(
            generate(fun(IndexType(k))(_ => l(0))) |>
              mapLocal(id)
          ) >>
          mapLocal(id)
        ) |>
        toGlobal |>
        reduceHistos
    ))))

    val output = runKernel(reduceByIndexLocalTest)(LocalSize(50), GlobalSize(1000))(n, k, indices, values)

    println("\nResult: ")
    print(output.deep.mkString(" "))

    println("\nExpected: ")
    for(i <- 0 until k) {
      print(result(i) + " ")

      assert(output(i) == result(i))
    }
  }

  test("Reduce By Index Local Test (Multiple Histograms, Float)") {

    //TODO: Add address space parameter to AtomicBinOp for float case

    val reduceHistos = implN(n => implN(numBins => fun(f_histosT(n, numBins))(histos =>
      histos |> // n.numBins.int
        oclReduceSeq(rise.core.types.AddressSpace.Global)(
          fun(acc_histo => // numBins.int
            fun(cur_histo => // numBins.int
              zip(acc_histo)(cur_histo) |> // 2.numBins.int
                mapLocal(fun(x => fst(x) + snd(x)))
            )
          )
        )(
          generate(fun(IndexType(numBins))(_ => l(0.0f))) |> // numBins.int
            mapLocal(id) //numBins.int
        ) |>
        mapLocal(id)
    )))

    val reduceByIndexLocalTest = nFun(n => nFun(k => fun(isT(n, k))(is => fun(f_xsT(n))(xs =>
      zip(is)(xs) |>
        split(50) |>
        mapWorkGroup(
          oclReduceByIndexLocal(rise.core.types.AddressSpace.Global)(add)(
            generate(fun(IndexType(k))(_ => l(0.0f))) |>
              mapLocal(id)
          ) >>
            mapLocal(id)
        ) |>
        toGlobal |>
        reduceHistos
    ))))

    val output = runKernel(reduceByIndexLocalTest)(LocalSize(50), GlobalSize(1000))(n, k, indices, f_values)

    println("\nResult: ")
    print(output.deep.mkString(" "))

    println("\nExpected: ")
    for(i <- 0 until k) {
      print(f_result(i) + " ")

      assert(output(i) == f_result(i))
    }
  }

  def runKernel[T](kernel: Expr)(
    localSize: LocalSize,
    globalSize: GlobalSize)(
    n: Int,
    k: Int,
    indices: Array[Int],
    values: Array[T]
  ): Array[T] = {
    val runKernel = gen
      .OpenCLKernel(kernel)
      .as[ScalaFunction `(` Int `,` Int `,` Array[Int] `,` Array[T]`)=>` Array[T]]
    val (output, _) = runKernel(localSize, globalSize)(n `,` k `,` indices `,` values)
    output
  }

}
