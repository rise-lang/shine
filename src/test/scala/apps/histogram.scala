package apps

import rise.core.DSL._
import rise.core.Expr
import rise.core.types._
import rise.core.TypeLevelDSL._
import rise.core._
import rise.openCL.DSL._
import shine.OpenCL.{GlobalSize, LocalSize}
import util.{Time, TimeSpan, gen}

class histogram extends shine.test_util.TestsWithExecutor {

  private def isT(N: NatIdentifier, K: NatIdentifier) = ArrayType(N, IndexType(K))
  private def histsT(N: NatIdentifier, M: NatIdentifier) = ArrayType(N, ArrayType(M, int))

  private val add = fun(x => fun(a => x + a))
  def id: Expr = fun(x => x)

  val n = 8192
  val k = 99

  val indices = new Array[Int](n)

  val result = new Array[Int](k)

  val r = new scala.util.Random
  var index = 0

  for(i <- 0 until n) {
    index = r.nextInt(k)
    indices(i) = index
    result(index) = result(index) + 1
  }

  private val reduceHists = implN(m => implN(k => fun(histsT(m, k))(hists =>
    hists |> // m.k.int
      oclReduceSeq(AddressSpace.Local)(
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
    val reduceByIndexIndividualHistograms = nFun(n => nFun(k => fun(isT(n, k))(is =>
      generate(fun(IndexType(n))(_ => l(1))) |>
        fun(xs =>
          zip(is)(xs) |>
            split(256) |>
              mapLocal(
                oclReduceByIndexSeq(AddressSpace.Local)(add)(
                  generate(fun(IndexType(k))(_ => l(0))) |>
                    mapSeq(id)
                ) >>
                mapSeq(id)
              )
        ) |>
        toLocal |>
        reduceHists
    )))

    val output = runKernel(reduceByIndexIndividualHistograms)(LocalSize(32), GlobalSize(32))(n, k, indices)

    checkResult(output._1, output._2)
  }

  test("Reduce by Index: Multiple threads accumulate into one histogram") {
    val reduceByIndexSharedHistograms = nFun(n => nFun(k => fun(isT(n, k))(is =>
      generate(fun(IndexType(n))(_ => l(1))) |>
        fun(xs =>
          zip(is)(xs) |>
            split(1024) |>
            mapWorkGroup(
              oclReduceByIndexLocal(AddressSpace.Local)(add)(
                generate(fun(IndexType(k))(_ => l(0))) |>
                  mapLocal(id)
              ) >>
              mapLocal(id)
            )
        ) |>
        toGlobal |>
        reduceHists
    )))

    val output = runKernel(reduceByIndexSharedHistograms)(LocalSize(1024), GlobalSize(8192))(n, k, indices)

    checkResult(output._1, output._2)
  }

  test("Reduce by Index: All threads accumulate into one histogram") {
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

    val output = runKernel(reduceByIndexSingleHistogram)(LocalSize(512), GlobalSize(512))(n, k, indices)

    checkResult(output._1, output._2)
  }

  test("Segmented Reduction") {
    //TODO: The input array has to be sorted before it can used by the segmented reduction algorithm.
    //      Usually this would be part of the algorithm in Rise however, as there isn't a sorting algorithm
    //      in Rise yet, this has to be done by a function call in Scala.
    //      This way of sorting the input array is pretty slow which is why it isn't added to the
    //      elapsed time of the kernel call. Therefore the runtime of this test case is considerably
    //      faster than it normally would be.
    val sortedIndices = indices.sorted

    val reduceByIndexSingleHistogram = nFun(n => nFun(k => fun(isT(n, k))(is =>
      generate(fun(IndexType(n))(_ => l(1))) |>
        fun(xs =>
          zip(is)(xs) |>
            split(1024) |>
              mapWorkGroup(
                oclSegmentedReduce(rise.core.types.AddressSpace.Local)(add)(
                  generate(fun(IndexType(k))(_ => l(0))) |>
                    mapLocal(id)
                ) >>
                mapLocal(id)
              )
        ) |>
        toGlobal |>
        reduceHists
    )))

    val output = runKernel(reduceByIndexSingleHistogram)(LocalSize(32), GlobalSize(256))(n, k, sortedIndices)

    checkResult(output._1, output._2)
  }

  def checkResult(output: Array[Int], runtime: TimeSpan[Time.ms]): Unit = {
    println("\nResult: ")
    print(output.deep.mkString(" "))

    println("\nExpected: ")
    for(i <- 0 until k) {
      print(result(i) + " ")

      assert(output(i) == result(i))
    }
    println("")

    println("Time: " + runtime)
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
}
