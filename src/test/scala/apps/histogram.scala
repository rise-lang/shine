package apps

import rise.core.DSL._
import rise.core.Expr
import rise.core.types._
import rise.core._
import rise.openCL.DSL._
import shine.OpenCL.{GlobalSize, LocalSize}
import util.{Time, TimeSpan, gen}

class histogram extends shine.test_util.TestsWithExecutor {

  private def isT(N: NatIdentifier, K: NatIdentifier) = ArrayType(N, IndexType(K))
  private def xsT(N: NatIdentifier) = ArrayType(N, int)
  private def histsT(N: NatIdentifier, M: NatIdentifier) = ArrayType(N, ArrayType(M, int))

  private val add = fun(x => fun(a => x + a))
  def id: Expr = fun(x => x)

  val n = 8192
  val k = 99

  val indices = new Array[Int](n)
  val values: Array[Int] = Array.fill[Int](n)(1)

  val result = new Array[Int](k)

  val r = new scala.util.Random
  var index = 0

  for(i <- 0 until n) {
    index = r.nextInt(k)
    indices(i) = index
    result(index) = result(index) + 1
  }

  private val reduceHists = nFun(m => nFun(k => fun(histsT(m, k))(hists =>
    hists |> // m.k.int
      transpose |> // k.m.int
      mapLocal(
        // m.int
        oclReduceSeq(AddressSpace.Local)(
          fun(a => fun(x => a + x)) // int
        )(l(0))
      ) // k.int
  )))

  test("Sequential Histogram") {
    val sequentialHistogram = nFun(n => nFun(k => fun(isT(n, k))(is =>
      generate(fun(IndexType(n))(_ => l(1))) |>
        fun(xs =>
          zip(is)(xs) |>
            oclReduceByIndexSeq(AddressSpace.Local)(add)(
              generate(fun(IndexType(k))(_ => l(0))) |>
                mapSeq(id)
            )
        ) |>
        mapSeq(id)
    )))

    val output = runKernel(sequentialHistogram)(LocalSize(1), GlobalSize(1))(n, k, indices)

    checkResult(output._1, output._2)
  }

  test("Reduce by Index: Each thread accumulates into its own histogram") {
    val threads = 32
    val chunkSize = n / threads

    val reduceByIndexIndividualHistograms = nFun(n => nFun(k => fun(isT(n, k))(is =>
      generate(fun(IndexType(n))(_ => l(1))) |>
        fun(xs =>
          zip(is)(xs) |>
            split(chunkSize) |>
              mapLocal(
                oclReduceByIndexSeq(AddressSpace.Local)(add)(
                  generate(fun(IndexType(k))(_ => l(0))) |>
                    mapSeq(id)
                ) >>
                mapSeq(id)
              )
        )
    )))

    val tempOutput = runKernel(reduceByIndexIndividualHistograms)(LocalSize(threads), GlobalSize(threads))(n, k, indices)

    val finalOutput = finalReduce(tempOutput._1, reduceHists)

    checkResult(finalOutput._1, tempOutput._2, finalOutput._2)
  }

  test("Reduce by Index: Multiple threads accumulate into one histogram") {
    val lSize = 1024
    val gSize = n

    val reduceByIndexSharedHistograms = nFun(n => nFun(k => fun(isT(n, k))(is =>
      generate(fun(IndexType(n))(_ => l(1))) |>
        fun(xs =>
          zip(is)(xs) |>
            split(lSize) |>
            mapWorkGroup(
              oclReduceByIndexLocal(AddressSpace.Local)(add)(
                generate(fun(IndexType(k))(_ => l(0))) |>
                  mapLocal(id)
              ) >>
              mapLocal(id)
            )
        )
    )))

    val tempOutput = runKernel(reduceByIndexSharedHistograms)(LocalSize(lSize), GlobalSize(gSize))(n, k, indices)

    val finalOutput = finalReduce(tempOutput._1, reduceHists)

    checkResult(finalOutput._1, tempOutput._2, finalOutput._2)
  }

  test("Reduce by Index: All threads accumulate into one histogram") {
    val threads = 512

    val reduceByIndexSingleHistogram = nFun(n => nFun(k => fun(isT(n, k))(is =>
      generate(fun(IndexType(n))(_ => l(1))) |>
        fun(xs =>
          zip(is)(xs) |>
            oclReduceByIndexLocal(AddressSpace.Local)(add)(
              generate(fun(IndexType(k))(_ => l(0))) |>
                mapLocal(id)
            ) |>
            mapLocal(id)
        )
    )))

    val output = runKernel(reduceByIndexSingleHistogram)(LocalSize(threads), GlobalSize(threads))(n, k, indices)

    checkResult(output._1, output._2)
  }

  test("Segmented Reduction: Atomic operation instead of second reduction") {
    val lSize = 64
    val gSize = n / 32
    val chunkSize = 32 * lSize

    //TODO: See TODO below
    val sortedIndices = indices.sorted

    val segmentedReductionAtomic = nFun(n => nFun(k => fun(isT(n, k))(is => fun(xsT(n))(xs =>
      zip(is)(xs) |>
        split(chunkSize) |>
        mapWorkGroup(
          oclSegReduceAtomic(AddressSpace.Local)(add)(
            generate(fun(IndexType(k))(_ => l(0))) |>
              mapLocal(id)
          ) >>
            mapLocal(id)
        )
    )
    )))

    val tempOutput = runKernel2(segmentedReductionAtomic)(LocalSize(lSize), GlobalSize(gSize))(n, k, sortedIndices, values)

    val finalOutput = finalReduce(tempOutput._1, reduceHists)

    checkResult(finalOutput._1, tempOutput._2, finalOutput._2)
  }

  test("Segmented Reduction: Tree-based second reduction") {
    val lSize = 64
    val gSize = n / 32
    val chunkSize = 32 * lSize

    //TODO: The input array has to be sorted before it can used by the segmented reduction algorithm.
    //      Usually this would be part of the algorithm in Rise however, as there isn't a sorting algorithm
    //      in Rise yet, this has to be done by a function call in Scala.
    //      This way of sorting the input array is pretty slow which is why it isn't added to the
    //      elapsed time of the kernel call. Therefore the runtime of this test case is considerably
    //      faster than it normally would be.
    val sortedIndices = indices.sorted

    val segmentedReductionTree = nFun(n => nFun(k => fun(isT(n, k))(is => fun(xsT(n))(xs =>
          zip(is)(xs) |>
            split(chunkSize) |>
              mapWorkGroup(
                oclSegReduce(AddressSpace.Local)(add)(
                  generate(fun(IndexType(k))(_ => l(0))) |>
                    mapLocal(id)
                ) >>
                mapLocal(id)
              )
        )
    )))

    val tempOutput = runKernel2(segmentedReductionTree)(LocalSize(lSize), GlobalSize(gSize))(n, k, sortedIndices, values)

    val finalOutput = finalReduce(tempOutput._1, reduceHists)

    checkResult(finalOutput._1, tempOutput._2, finalOutput._2)
  }

  def finalReduce(tempOutput: Array[Int], kernel: Expr): (Array[Int], TimeSpan[Time.ms]) = {
    val m = tempOutput.length / k
    val threads = if (k > 1024) 1024 else k

    print("\nReducing all subhistograms...")

    runSecondKernel(kernel)(LocalSize(threads), GlobalSize(threads))(m, k, tempOutput)
  }

  def checkResult(output: Array[Int], firstTime: TimeSpan[Time.ms], secondTime: TimeSpan[Time.ms] = null): Unit = {
    println("\nResult: ")
    print(output.deep.mkString(" "))
    println("")

    for(i <- 0 until k) {
      assert(output(i) == result(i))
    }

    println("Result matches with expected values.")

    if (secondTime != null) {
      val total = firstTime + secondTime
      println("Runtime of first kernel: " + firstTime)
      println("Runtime of second kernel: " + secondTime)
      println("Total runtime: " + total)
    }
    else {
      println("Runtime: " + firstTime)
    }

  }

  def runKernel(kernel: Expr)(
    localSize: LocalSize,
    globalSize: GlobalSize)(
    n: Int,
    k: Int,
    indices: Array[Int]
  ): (Array[Int], TimeSpan[Time.ms]) = {
    import shine.OpenCL._
    val runKernel = gen
      .OpenCLKernel(kernel)
      .as[ScalaFunction `(` Int `,` Int `,` Array[Int] `)=>` Array[Int]]
    runKernel(localSize, globalSize)(n `,` k `,` indices)
    }

  def runKernel2(kernel: Expr)(
    localSize: LocalSize,
    globalSize: GlobalSize)(
    n: Int,
    k: Int,
    indices: Array[Int],
    values: Array[Int]
  ): (Array[Int], TimeSpan[Time.ms]) = {
    import shine.OpenCL._
    val runKernel = gen
      .OpenCLKernel(kernel)
      .as[ScalaFunction `(` Int `,` Int `,` Array[Int] `,` Array[Int] `)=>` Array[Int]]
    runKernel(localSize, globalSize)(n `,` k `,` indices `,` values)
  }

  def runSecondKernel(kernel: Expr)(
    localSize: LocalSize,
    globalSize: GlobalSize)(
    m: Int,
    k: Int,
    input: Array[Int]
  ): (Array[Int], TimeSpan[Time.ms]) = {
    import shine.OpenCL._
    val runKernel = gen
      .OpenCLKernel(kernel)
      .as[ScalaFunction `(` Int `,` Int `,` Array[Int]`)=>` Array[Int]]
    runKernel(localSize, globalSize)(m `,` k `,` input)
  }

}
