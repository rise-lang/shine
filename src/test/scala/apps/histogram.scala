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
  val k = 128

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
      split(m) |>
      mapWorkGroup(
        transpose >> // k.m.int
        mapLocal(
          // m.int
          oclReduceSeq(AddressSpace.Private)(
            fun(a => fun(x => a + x)) // int
          )(l(0))
        )
      ) // k.int
  )))

  private val reduceHistsGlobal = nFun(m => nFun(k => fun(histsT(m, k))(hists =>
    hists |> // m.k.int
        transpose |> // k.m.int
          mapGlobal(
            // m.int
            oclReduceSeq(AddressSpace.Private)(
              fun(a => fun(x => a + x)) // int
            )(l(0))
          )
      ) // k.int
  ))

  /*private val reduceHistsSegmented = nFun(m => nFun(k => fun(histsT(m, k))(hists =>
    hists |> // m.k.int
      transpose |> // k.m.int
      mapGlobal(
        // m.int
        oclReduceSeq(AddressSpace.Private)(
          fun(a => fun(x => a + x)) // int
        )(l(0))
      )
  ) // k.int
  ))*/

  // Might only work for square matrices
  /*private val transposeEfficient = nFun(m => nFun(n => fun(histsT(m, n))(hists =>
    tile(16, 16, hists) |>
      transpose |>
      mapWorkGroup(
        fun(tile =>
          tile |>
            join |>
            mapLocal(id) |>
            toLocal |>
            split(16) |> // tileWidth
            transpose |>
            join |>
            mapLocal(id) |>
            split(16) // tileHeight
        )
      )
  )))*/

  def tile(tileHeight: Nat, tileWidth: Nat, matrix: Expr): Expr = {
    matrix |>
    split(tileHeight) |>
    map(
      transpose >>
      split(tileWidth) >>
      map(transpose)
    )
  }

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
    val lSize = 256
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

    val finalOutput = finalReduce(tempOutput._1, reduceHistsGlobal)

    checkResult(finalOutput._1, tempOutput._2, finalOutput._2)
  }

  test("Reduce by Index: All threads accumulate into one histogram") {
    val threads = 256

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

  test("Segmented Reduction: Tree-based second reduction") {
    val chunkSizeLocal = 32
    val lSize = 32
    val gSize = n / chunkSizeLocal
    val chunkSizeWorkgroup = chunkSizeLocal * lSize

    //TODO: The input array has to be sorted before it can used by the segmented reduction algorithm.
    //      Usually this would be part of the algorithm in Rise however, as there isn't a sorting algorithm
    //      in Rise yet, this has to be done by a function call in Scala.
    //      This way of sorting the input array is pretty slow which is why it isn't added to the
    //      elapsed time of the kernel call. Therefore the runtime of this test case is considerably
    //      faster than it normally would be.
    val sortedIndices = indices.sorted

    val segmentedReductionTree = nFun(n => nFun(k => fun(isT(n, k))(is => fun(xsT(n))(xs =>
          zip(is)(xs) |>
            split(chunkSizeWorkgroup) |>
              mapWorkGroup(
                oclSegReduce(chunkSizeLocal)(AddressSpace.Local)(add)(
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

  test("Segmented Reduction: Atomic operation instead of second reduction") {
    val chunkSizeLocal = 8
    val lSize = 128
    val gSize = n / chunkSizeLocal
    val chunkSizeWorkgroup = chunkSizeLocal * lSize

    //TODO: See TODO above
    val sortedIndices = indices.sorted

    val segmentedReductionAtomic = nFun(n => nFun(k => fun(isT(n, k))(is => fun(xsT(n))(xs =>
      zip(is)(xs) |>
        split(chunkSizeWorkgroup) |>
        mapWorkGroup(
          oclSegReduceAtomic(chunkSizeLocal)(AddressSpace.Local)(add)(
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

  //FIXME: This throws a segfault error on CPUs
  test("Generate value array on device") {
    val chunkSizeLocal = 8
    val lSize = 128
    val gSize = n / chunkSizeLocal
    val chunkSizeWorkgroup = chunkSizeLocal * lSize

    val sortedIndices = indices.sorted

    val valueArrayOnDevice = nFun(n => nFun(k => fun(isT(n, k))(is =>
      generate(fun(IndexType(n))(_ => l(1))) |>
        fun(xs =>
          zip(is)(xs) |>
          split(chunkSizeWorkgroup) |>
          mapWorkGroup(
            oclSegReduceAtomic(chunkSizeLocal)(AddressSpace.Local)(add)(
              generate(fun(IndexType(k))(_ => l(0))) |>
                mapLocal(id)
            ) >>
              mapLocal(id)
          )
      )
    )))

    val tempOutput = runKernel(valueArrayOnDevice)(LocalSize(lSize), GlobalSize(gSize))(n, k, sortedIndices)

    val finalOutput = finalReduce(tempOutput._1, reduceHists)

    checkResult(finalOutput._1, tempOutput._2, finalOutput._2)
  }

  def finalReduce(tempOutput: Array[Int], kernel: Expr): (Array[Int], TimeSpan[Time.ms]) = {
    val m = tempOutput.length / k
    val threads = 2048

    print("\nReducing all subhistograms...")

    runSecondKernel(kernel)(LocalSize(512), GlobalSize(threads))(m, k, tempOutput)
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
