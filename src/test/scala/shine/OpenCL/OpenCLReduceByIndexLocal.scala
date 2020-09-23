package shine.OpenCL

import rise.core.DSL._
import rise.core.Expr
import rise.core.types._
import rise.openCL.DSL._
import util.gen

class OpenCLReduceByIndexLocal extends shine.test_util.TestsWithExecutor {

  private def xsT(N: NatIdentifier) = ArrayType(N, int)
  private def xsfT(N: NatIdentifier) = ArrayType(N, f32)
  private def isT(N: NatIdentifier, K: NatIdentifier) = ArrayType(N, IndexType(K))
  private def histsT(N: NatIdentifier, M: NatIdentifier) = ArrayType(N, ArrayType(M, int))
  private def histsfT(N: NatIdentifier, M: NatIdentifier) = ArrayType(N, ArrayType(M, f32))

  private val add = fun(x => fun(a => x + a))
  def id: Expr = fun(x => x)

  val n = 1000
  val k = 21

  val indices = new Array[Int](n)

  val values = new Array[Int](n)
  val result = new Array[Int](k)

  val fValues = new Array[Float](n)
  val fResult = new Array[Float](k)

  val r = new scala.util.Random
  var index = 0

  for(i <- 0 until n) {
    index = r.nextInt(k)
    indices(i) = index
    values(i) = i
    fValues(i) = i
    result(index) = result(index) + i
    fResult(index) = fResult(index) + i
  }

  test("Reduce By Index Local Test (Single Histogram, Int)") {

    val singleHistogramInt = nFun(n => nFun(k => fun(isT(n, k))(is => fun(xsT(n))(xs =>
      zip(is)(xs) |> // n.(idx(k) x int)
        oclReduceByIndexLocal(rise.core.types.AddressSpace.Local)(add)(
          generate(fun(IndexType(k))(_ => l(0))) |>
            mapLocal(id) // k.int
        ) |>
        mapLocal(id) // k.int
    ))))

    val output = runKernel(singleHistogramInt)(LocalSize(1000), GlobalSize(1000))(n, k, indices, values)

    checkResult(output, result)
  }

  test("Reduce By Index Local Test (Single Histogram, Float)") {

    val singleHistogramFloat = nFun(n => nFun(k => fun(isT(n, k))(is => fun(xsfT(n))(xs =>
      zip(is)(xs) |> // n.(idx(k) x float)
        oclReduceByIndexLocal(rise.core.types.AddressSpace.Local)(add)(
          generate(fun(IndexType(k))(_ => l(0.0f))) |>
            mapLocal(id) // k.int
        ) |>
        mapLocal(id) // k.int
    ))))

    val output = runKernel(singleHistogramFloat)(LocalSize(1000), GlobalSize(1000))(n, k, indices, fValues)

    checkResult(output, fResult)
  }

  test("Reduce By Index Local Test (Multiple Histograms, Int)") {

    val multipleHistogramsInt = nFun(n => nFun(k => fun(isT(n, k))(is => fun(xsT(n))(xs =>
      zip(is)(xs) |> // n.(idx(k) x int)
        split(50) |> // n/50.50.(idx(k) x int)
        mapWorkGroup(
          // 50.(idx(k) x int)
          oclReduceByIndexLocal(rise.core.types.AddressSpace.Local)(add)(
            generate(fun(IndexType(k))(_ => l(0))) |>
              mapLocal(id) // k.int
          ) >>
          mapLocal(id) // k.int
        )
    )))) // n/50.k.int

    val reduceHistsInt = nFun(m => nFun(k => fun(histsT(m, k))(hists =>
      hists |> // m.k.int
        oclReduceSeq(rise.core.types.AddressSpace.Local)(
          fun(acc_histo => // k.int
            fun(cur_histo => // k.int
              zip(acc_histo)(cur_histo) |> // k.(int x int)
                mapLocal(fun(x => fst(x) + snd(x))) // (int x int)
            )
          )
        )(
          generate(fun(IndexType(k))(_ => l(0))) |>
            mapLocal(id) // k.int
        ) |>
        mapLocal(id) // k.int
    )))

    val tempOutput = runKernel(multipleHistogramsInt)(LocalSize(50), GlobalSize(1000))(n, k, indices, values)

    val m = tempOutput.length
    val threads = if (k > 1024) 1024 else k

    print("\nReducing all subhistograms...")

    val finalOutput = runSecondKernel(reduceHistsInt)(LocalSize(threads), GlobalSize(threads))(m, k, tempOutput)

    checkResult(finalOutput, result)
  }

  test("Reduce By Index Local Test (Multiple Histograms, Float)") {

    val multipleHistogramsFloat = nFun(n => nFun(k => fun(isT(n, k))(is => fun(xsfT(n))(xs =>
      zip(is)(xs) |> // n.(idx(k) x float)
        split(50) |> // n/50.50.(idx(k) x float)
        mapWorkGroup(
          // 50.(idx(k) x float)
          oclReduceByIndexLocal(rise.core.types.AddressSpace.Local)(add)(
            generate(fun(IndexType(k))(_ => l(0.0f))) |>
              mapLocal(id) // k.float
          ) >>
            mapLocal(id) // k.float
        )
    )))) // n/50.k.float

    val reduceHistsFloat = nFun(m => nFun(k => fun(histsfT(m, k))(hists =>
      hists |> // m.k.float
        oclReduceSeq(rise.core.types.AddressSpace.Local)(
          fun(acc_histo => // k.float
            fun(cur_histo => // k.float
              zip(acc_histo)(cur_histo) |> // k.(float x float)
                mapLocal(fun(x => fst(x) + snd(x))) // (float x float)
            )
          )
        )(
          generate(fun(IndexType(k))(_ => l(0.0f))) |>
            mapLocal(id) // k.float
        ) |>
        mapLocal(id) // k.float
    )))

    val tempOutput = runKernel(multipleHistogramsFloat)(LocalSize(50), GlobalSize(1000))(n, k, indices, fValues)

    val m = tempOutput.length
    val threads = if (k > 1024) 1024 else k

    print("\nReducing all subhistograms...")

    val finalOutput = runSecondKernel(reduceHistsFloat)(LocalSize(threads), GlobalSize(threads))(m, k, tempOutput)

    checkResult(finalOutput, fResult)
  }

  def checkResult[T](output: Array[T], expected: Array[T]): Unit = {
    println("\nResult: ")
    print(output.deep.mkString(" "))

    println("\nExpected: ")
    for(i <- 0 until k) {
      print(expected(i) + " ")

      assert(output(i) == expected(i))
    }
    println("")
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

  def runSecondKernel[T](kernel: Expr)(
    localSize: LocalSize,
    globalSize: GlobalSize)(
    m: Int,
    k: Int,
    input: Array[T]
  ): Array[T] = {
    val runKernel = gen
      .OpenCLKernel(kernel)
      .as[ScalaFunction `(` Int `,` Int `,` Array[T]`)=>` Array[T]]
    val (output, _) = runKernel(localSize, globalSize)(m `,` k `,` input)
    output
  }

}
