package shine.OpenCL

import rise.core.DSL._
import rise.core.Expr
import rise.core.types._
import rise.openCL.DSL._
import util.gen

class OpenCLSegReduceAtomic extends shine.test_util.TestsWithExecutor {

  private def xsT(N: NatIdentifier) = ArrayType(N, int)
  private def isT(N: NatIdentifier, K: NatIdentifier) = ArrayType(N, IndexType(K))
  private def histsT(N: NatIdentifier, M: NatIdentifier) = ArrayType(N, ArrayType(M, int))

  private val add = fun(x => fun(a => x + a))
  def id: Expr = fun(x => x)

  val n = 8192
  val k = 10

  val values = new Array[Int](n)
  val indices = new Array[Int](n)
  val result = new Array[Int](k)

  val r = new scala.util.Random
  var index = 0

  for(i <- 0 until n) {
    indices(i) = r.nextInt(k)
  }

  scala.util.Sorting.quickSort(indices)

  for(i <- 0 until n) {
    index = indices(i)
    values(i) = index + 1
    result(index) = result(index) + index + 1
  }

  test("OpenCL Segmented Reduce Atomic Test") {

    val oclSegmentedReduceAtomicTest = nFun(n => nFun(k => fun(isT(n, k))(is => fun(xsT(n))(xs =>
      zip(is)(xs) |> // n.(idx(k) x int)
        split(1024) |> // n/1024.1024.(idx(k) x int)
        mapWorkGroup(
          // 1024.(idx(k) x int)
          oclSegReduceAtomic(rise.core.types.AddressSpace.Local)(add)(
            generate(fun(IndexType(k))(_ => l(0))) |>
              mapLocal(id) // k.int
          ) >>
            mapLocal(id) // k.int
        )
    )))) // n/1024.k.int

    val reduceHists = nFun(m => nFun(k => fun(histsT(m, k))(hists =>
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

    val tempOutput = runKernel(oclSegmentedReduceAtomicTest)(LocalSize(32), GlobalSize(256))(n, k, indices, values)

    val m = tempOutput.length
    val threads = if (k > 1024) 1024 else k

    print("\nReducing all subhistograms...")

    val finalOutput = runSecondKernel(reduceHists)(LocalSize(threads), GlobalSize(threads))(m, k, tempOutput)

    checkResult(finalOutput, result)
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
