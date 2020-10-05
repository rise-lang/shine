package shine.OpenCL

import rise.core.DSL._
import rise.core.Expr
import rise.core.types._
import rise.openCL.DSL._
import util.gen

class OpenCLReduceByIndexSeq extends shine.test_util.TestsWithExecutor {

  private def xsT(N: NatIdentifier) = ArrayType(N, int)
  private def isT(N: NatIdentifier, K: NatIdentifier) = ArrayType(N, IndexType(K))
  private def histsT(N: NatIdentifier, M: NatIdentifier) = ArrayType(N, ArrayType(M, int))

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

  private val maxLocalSize = 256

  test("Reduce By Index Seq Test") {

    val reduceByIndexSeqTest = nFun(n => nFun(k => fun(isT(n, k))(is => fun(xsT(n))(xs =>
      zip(is)(xs) |> // n.(idx(k) x int)
      split(50) |> // n/50.50.(idx(k) x int)
      mapLocal(
        // 50.(idx(k) x int)
        oclReduceByIndexSeq(rise.core.types.AddressSpace.Local)(add)(
          generate(fun(IndexType(k))(_ => l(0))) |>
            mapSeq(id) // k.int
        ) >>
        mapSeq(id) // k.int
      )
    )))) // n/50.k.int

    val reduceHists = nFun(m => nFun(k => fun(histsT(m, k))(hists =>
      hists |> // m.k.int
        transpose |> // k.m.int
        mapGlobal(
          // m.int
          oclReduceSeq(rise.core.types.AddressSpace.Private)(
            fun(a => fun(x => a + x)) // int
          )(l(0)) // int
        ) // k.int
    )))

    val tempOutput = runKernel(reduceByIndexSeqTest)(LocalSize(50), GlobalSize(50))(n, k, indices, values)

    val finalOutput = finalReduce(tempOutput, reduceHists)

    checkResult(finalOutput, result)
  }

  def nextPowerOf2(n: Int): Int = {
    val highestOneBit = Integer.highestOneBit(n)
    if (n == highestOneBit) return n
    highestOneBit << 1
  }

  def finalReduce[T](tempOutput: Array[T], kernel: Expr): Array[T] = {
    val m = tempOutput.length / k
    val gSize = nextPowerOf2(k)
    val lSize = if (gSize > maxLocalSize) maxLocalSize else gSize

    print("\nReducing all subhistograms...")

    runSecondKernel(kernel)(LocalSize(gSize), GlobalSize(lSize))(m, k, tempOutput)
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
