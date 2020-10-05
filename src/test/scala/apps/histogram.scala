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
  val k = 64

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

  private val maxLocalSize = 256

  /*private val reduceHistsOld = nFun(m => nFun(k => fun(histsT(m, k))(hists =>
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
  )))*/

  private val reduceHistsGlobal = nFun(m => nFun(k => fun(histsT(m, k))(hists =>
    hists |> // m.k.int
        transpose |> // k.m.int
          mapGlobal(
            // m.int
            oclReduceSeq(AddressSpace.Private)(
              fun(a => fun(x => a + x)) // int
            )(l(0)) // int
          ) // k.int
  )))

  private val reduceHistsWorkGroup = nFun(m => nFun(k => nFun(chunk => fun(histsT(m, k))(hists =>
    hists |> // m.k.int
      transpose |> // k.m.int
      mapWorkGroup(
        fun(bin => // m.int
          bin |>
            split(chunk) |> // m/chunk.chunk.int
            mapLocal(
              // chunk.int
              oclReduceSeq(AddressSpace.Private)(
                fun(a => fun(x => a + x)) // int
              )(l(0))
            ) |> // m/chunk.int
            toLocal |>
            split(1) |> // m/chunk.1.int
            transpose |> // 1.m/chunk.int
            mapLocal(
              oclReduceSeq(AddressSpace.Private)(
                fun(a => fun(x => a + x)) // int
              )(l(0))
            )
        )
      ) // k.int
  ))))

  // Might only work for square matrices
  /*private val transposeEfficient = nFun(m => nFun(n => fun(histsT(m, n))(hists =>
    tile(16, 16, hists) |>
      transpose |>
      mapWorkGroup(
        fun(t =>
          t |>
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
  )))

  def tile(tileHeight: Nat, tileWidth: Nat, matrix: Expr): Expr = {
    matrix |>
    split(tileHeight) |>
    map(
      transpose >>
      split(tileWidth) >>
      map(transpose)
    )
  }*/

  test("Sequential Histogram") {
    val sequentialHistogram = nFun(n => nFun(k => fun(isT(n, k))(is =>
      generate(fun(IndexType(n))(_ => l(1))) |>
        fun(xs => // n.int
          zip(is)(xs) |> //n.(idx x int)
            oclReduceByIndexSeq(AddressSpace.Local)(add)(
              generate(fun(IndexType(k))(_ => l(0))) |>
                mapSeq(id) // k.int
            )
        ) |>
        mapSeq(id) // k.int
    )))

    val output = runKernel(sequentialHistogram)(LocalSize(1), GlobalSize(1))(n, k, indices)

    checkResult(output._1, output._2)
  }

  test("Reduce by Index: Each thread accumulates into its own histogram (global)") {
    val lSize = 128
    val gSize = 1024
    val chunkSize = n / gSize

    val individualHistogramsGlobal = nFun(n => nFun(k => nFun(chunk => fun(isT(n, k))(is =>
      generate(fun(IndexType(n))(_ => l(1))) |>
        fun(xs => // n.int
          zip(is)(xs) |> // n.(idx x int)
            split(n/chunk) |> // chunk.n/chunk.(idx x int)
            transpose |> // n/chunk.chunk.(idx x int)
            mapGlobal(
              // chunk.(idx x int)
              oclReduceByIndexSeq(AddressSpace.Global)(add)(
                generate(fun(IndexType(k))(_ => l(0))) |>
                  mapSeq(id) // k.int
              ) >>
                mapSeq(id) // k.int
            ) // n/chunk.k.int
        )
    ))))

    val tempOutput =
      runKernelChunk(individualHistogramsGlobal)(LocalSize(lSize), GlobalSize(gSize))(n, k, chunkSize, indices)

    val finalOutput = finalReduceGlobal(tempOutput._1)

    checkResult(finalOutput._1, tempOutput._2, finalOutput._2)
  }

  test("Reduce by Index: Each thread accumulates into its own histogram (local)") {
    val lSize = 128
    val lChunkSize = 8
    val wgChunkSize = lSize * lChunkSize
    val wgSize = n / wgChunkSize
    val gSize = lSize * wgSize

    val individualHistogramsLocal = nFun(n => nFun(k => nFun(wgChunk => nFun(lChunk => fun(isT(n, k))(is =>
      generate(fun(IndexType(n))(_ => l(1))) |>
        fun(xs => // n.int
          zip(is)(xs) |> // n.(idx x int)
            split(wgChunk) |> // n/wgChunk.wgChunk.(idx x int)
            mapWorkGroup(
              // wgChunk.(idx x int)
              split(lChunk) >> // wgChunk/lChunk.lChunk.(idx x int)
                mapLocal(
                  // lChunk.(idx x int)
                  oclReduceByIndexSeq(AddressSpace.Local)(add)(
                    generate(fun(IndexType(k))(_ => l(0))) |>
                      mapSeq(id) // k.int
                  ) >>
                    mapSeq(id) // k.int
                ) // wgChunk/lChunk.k.int
            ) // n/lChunk.k.int
        )
    )))))

    val tempOutput =
      runKernelTwoChunks(individualHistogramsLocal)(LocalSize(lSize), GlobalSize(gSize))(
        n, k, wgChunkSize, lChunkSize, indices)

    val finalOutput = finalReduceGlobal(tempOutput._1)

    checkResult(finalOutput._1, tempOutput._2, finalOutput._2)
  }

  test("Reduce by Index: Multiple threads accumulate into one histogram") {
    val lSize = 128
    val lChunkSize = 8
    val wgChunkSize = lSize * lChunkSize
    val wgSize = n / wgChunkSize
    val gSize = lSize * wgSize

    val sharedHistograms = nFun(n => nFun(k => nFun(chunk => fun(isT(n, k))(is =>
      generate(fun(IndexType(n))(_ => l(1))) |>
        fun(xs => // n.int
          zip(is)(xs) |> // n.(idx x int)
            split(chunk) |> // n/wgChunk.wgChunk.(idx x int)
            mapWorkGroup(
              // wgChunk.(idx x int)
              oclReduceByIndexLocal(AddressSpace.Local)(add)(
                generate(fun(IndexType(k))(_ => l(0))) |>
                  mapLocal(id) // k.int
              ) >>
              mapLocal(id) // k.int
            ) // n/wgChunk.k.int
        )
    ))))

    val tempOutput =
      runKernelChunk(sharedHistograms)(LocalSize(lSize), GlobalSize(gSize))(n, k, wgChunkSize, indices)

    val finalOutput = finalReduceGlobal(tempOutput._1)

    checkResult(finalOutput._1, tempOutput._2, finalOutput._2)
  }

  test("Reduce by Index: All threads accumulate into one histogram") {
    val threads = maxLocalSize

    val singleHistogram = nFun(n => nFun(k => fun(isT(n, k))(is =>
      generate(fun(IndexType(n))(_ => l(1))) |>
        fun(xs => // n.int
          zip(is)(xs) |> // n.(idx x int)
            split(n) |> // 1.n.(idx x int)
            mapWorkGroup(
              // n.(idx x int)
              oclReduceByIndexLocal(AddressSpace.Local)(add)(
                generate(fun(IndexType(k))(_ => l(0))) |>
                  mapLocal(id) // k.int
              ) >>
              mapLocal(id) // k.int
            ) // k.int
        )
    )))

    val output = runKernel(singleHistogram)(LocalSize(threads), GlobalSize(threads))(n, k, indices)

    checkResult(output._1, output._2)
  }

  test("Segmented Reduction: Tree-based second reduction") {
    val lSize = 32
    val lChunkSize = 32
    val wgChunkSize = lChunkSize * lSize
    val wgSize = n / wgChunkSize
    val gSize = lSize * wgSize

    //TODO: The input array has to be sorted before it can used by the segmented reduction algorithm.
    //      Usually this would be part of the algorithm in Rise however, as there isn't a sorting algorithm
    //      in Rise yet, this has to be done by a function call in Scala.
    //      This way of sorting the input array is pretty slow which is why it isn't added to the
    //      elapsed time of the kernel call. Therefore the runtime of this test case is considerably
    //      faster than it normally would be.
    val sortedIndices = indices.sorted

    val segReduceTree = nFun(n => nFun(k => nFun(wgChunk => nFun(lChunk => fun(isT(n, k))(is => fun(xsT(n))(xs =>
          zip(is)(xs) |> // n.(idx x int)
            split(wgChunk) |> // n/wgChunk.wgChunk.(idx x int)
              mapWorkGroup(
                // wgChunk.(idx x int)
                oclSegReduce(lChunk)(AddressSpace.Local)(add)(
                  generate(fun(IndexType(k))(_ => l(0))) |>
                    mapLocal(id) // k.int
                ) >>
                mapLocal(id) // k.int
              ) // n/wgChunk.k.int
        )
    )))))

    val tempOutput =
      runKernelSeg(segReduceTree)(LocalSize(lSize), GlobalSize(gSize))(
        n, k, wgChunkSize, lChunkSize, sortedIndices, values)

    val finalOutput = finalReduceGlobal(tempOutput._1)

    checkResult(finalOutput._1, tempOutput._2, finalOutput._2)
  }

  test("Segmented Reduction: Atomic operation instead of second reduction") {
    val lSize = 128
    val lChunkSize = 8
    val wgChunkSize = lChunkSize * lSize
    val wgSize = n / wgChunkSize
    val gSize = lSize * wgSize

    //TODO: See TODO above
    val sortedIndices = indices.sorted

    val segReduceAtomic = nFun(n => nFun(k => nFun(wgChunk => nFun(lChunk => fun(isT(n, k))(is => fun(xsT(n))(xs =>
      zip(is)(xs) |> // n.(idx x int)
        split(wgChunk) |> // n/wgChunk.wgChunk.(idx x int)
        mapWorkGroup(
          // wgChunk.(idx x int)
          oclSegReduceAtomic(lChunk)(AddressSpace.Local)(add)(
            generate(fun(IndexType(k))(_ => l(0))) |>
              mapLocal(id) // k.int
          ) >>
            mapLocal(id) // k.int
        ) // n/wgChunk.k.int
    )
    )))))

    val tempOutput =
      runKernelSeg(segReduceAtomic)(LocalSize(lSize), GlobalSize(gSize))(
        n, k, wgChunkSize, lChunkSize, sortedIndices, values)

    val finalOutput = finalReduceGlobal(tempOutput._1)

    checkResult(finalOutput._1, tempOutput._2, finalOutput._2)
  }

  //FIXME: This throws a segfault error on CPUs
  test("Generate value array on device") {
    val lSize = 32
    val lChunkSize = 32
    val wgChunkSize = lChunkSize * lSize
    val wgSize = n / wgChunkSize
    val gSize = lSize * wgSize

    //TODO: See TODO above
    val sortedIndices = indices.sorted

    val valuesOnArray = nFun(n => nFun(k => nFun(wgChunk => nFun(lChunk => fun(isT(n, k))(is =>
      generate(fun(IndexType(n))(_ => cast(l(1) :: int))) |>
        fun(xs =>
          zip(is)(xs) |>
          split(wgChunk) |>
          mapWorkGroup(
            oclSegReduceAtomic(lChunk)(AddressSpace.Local)(add)(
              generate(fun(IndexType(k))(_ => l(0))) |>
                mapLocal(id)
            ) >>
              mapLocal(id)
          )
      )
    )))))

    val tempOutput =
      runKernelTwoChunks(valuesOnArray)(LocalSize(lSize), GlobalSize(gSize))(
        n, k, wgChunkSize, lChunkSize, sortedIndices)

    val finalOutput = finalReduceGlobal(tempOutput._1)

    checkResult(finalOutput._1, tempOutput._2, finalOutput._2)
  }

  def nextPowerOf2(n: Int): Int = {
    val highestOneBit = Integer.highestOneBit(n)
    if (n == highestOneBit) return n
    highestOneBit << 1
  }

  def finalReduceGlobal(tempOutput: Array[Int]): (Array[Int], TimeSpan[Time.ms]) = {
    val m = tempOutput.length / k
    val gSize = nextPowerOf2(k)
    val lSize = if (gSize > maxLocalSize) maxLocalSize else gSize

    println("\nReducing all subhistograms (global):")

    runKernel(reduceHistsGlobal)(LocalSize(lSize), GlobalSize(gSize))(m, k, tempOutput)
  }

  def finalReduceWorkGroup(tempOutput: Array[Int], chunkSize: Int): (Array[Int], TimeSpan[Time.ms]) = {
    val m = tempOutput.length / k
    val wgSize = nextPowerOf2(k)
    val lSize = m / chunkSize
    val gSize = lSize * wgSize

    println("\nReducing all subhistograms (work group):")

    runKernelChunk(reduceHistsWorkGroup)(LocalSize(lSize), GlobalSize(gSize))(m, k, chunkSize, tempOutput)
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

  def runKernelChunk(kernel: Expr)(
    localSize: LocalSize,
    globalSize: GlobalSize)(
    n: Int,
    k: Int,
    chunk: Int,
    indices: Array[Int]
  ): (Array[Int], TimeSpan[Time.ms]) = {
    import shine.OpenCL._
    val runKernel = gen
      .OpenCLKernel(kernel)
      .as[ScalaFunction `(` Int `,` Int `,` Int `,` Array[Int] `)=>` Array[Int]]
    runKernel(localSize, globalSize)(n `,` k `,` chunk `,` indices)
  }

  def runKernelTwoChunks(kernel: Expr)(
    localSize: LocalSize,
    globalSize: GlobalSize)(
    n: Int,
    k: Int,
    chunk1: Int,
    chunk2: Int,
    indices: Array[Int]
  ): (Array[Int], TimeSpan[Time.ms]) = {
    import shine.OpenCL._
    val runKernel = gen
      .OpenCLKernel(kernel)
      .as[ScalaFunction `(` Int `,` Int `,` Int `,` Int `,` Array[Int] `)=>` Array[Int]]
    runKernel(localSize, globalSize)(n `,` k `,` chunk1 `,` chunk2 `,` indices)
  }

  def runKernelSeg(kernel: Expr)(
    localSize: LocalSize,
    globalSize: GlobalSize)(
    n: Int,
    k: Int,
    chunk1: Int,
    chunk2: Int,
    indices: Array[Int],
    values: Array[Int]
  ): (Array[Int], TimeSpan[Time.ms]) = {
    import shine.OpenCL._
    val runKernel = gen
      .OpenCLKernel(kernel)
      .as[ScalaFunction `(` Int `,` Int `,` Int `,` Int `,` Array[Int] `,` Array[Int] `)=>` Array[Int]]
    runKernel(localSize, globalSize)(n `,` k `,` chunk1 `,` chunk2 `,` indices `,` values)
  }

}
