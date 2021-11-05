package apps

import rise.core.DSL._
import rise.core.Expr
import rise.core.primitives.{let => _, _}
import rise.core.semantics.FloatData
import rise.core.types.DataType._
import rise.core.types.Nat
import rise.openCL.DSL.oclRun
import rise.openCL.primitives._
import shine.OpenCL.KernelExecutor.KernelNoSizes.fromKernelModule
import shine.OpenCL._
import util.gen
import util.gen.c.function

import scala.annotation.tailrec

class scan extends test_util.Tests {
  private val simpleScan = fun(ArrayType(8, f32))(array =>
    array |> scanSeq(add)(lf32(0.0f))
  )
  private val simpleScanOcl = fun(ArrayType(8, f32))(array =>
    array |> oclScanSeq(AddressSpace.Private)(add)(lf32(0.0f))
  )


  test("Simple scan compiles to syntactically correct C") {
    function.asStringFromExpr(simpleScan)
  }

  test("Simple scan compiles to syntactically correct OpenMP") {
    gen.openmp.function.asStringFromExpr(simpleScan)
  }

  // currently fails do to a missing address space at a new
  test("Simple scan compiles to syntactically correct OpenCL") {
    gen.opencl.kernel.asStringFromExpr(simpleScanOcl)
  }


//  private val seqScanOcl = depFun((n:Nat) => depFun((m:Nat) => fun(ArrayType(n*m, f32))(input =>
//    let(input |> split(m) |> mapSeq(
//      oclScanSeqInclusive(AddressSpace.Private)(add)(lf32(0.0f))
//    ) |> oclToMem(AddressSpace.Global) |> map(drop(1))).be(scannedChunks => {
//
//      let(
//        scannedChunks |>
//          map(idx(lidx(m-1, m))) |> take(n-1) |>
//          oclScanSeqInclusive(AddressSpace.Private)(add)(lf32(0.0f)) |>
//          oclToMem(AddressSpace.Global)
//      ).be(scanOfLastElementOfChunks =>
//        zip(scannedChunks)(scanOfLastElementOfChunks) |>
//          map(fun(pair => pair._1 |> map(fun(x => x + pair._2)))) |>
//          join |> padCst(1)(0)(lf32(0.0f)) |> mapSeq(fun(x => x))
//      )
//    })
//  )))

  private val parallelScanOcl = depFun((n:Nat) => depFun((m:Nat) => fun(ArrayType(n*m, f32))(input =>
    let(input |> split(m) |> mapGlobal(0)(
      oclScanSeqInclusive(AddressSpace.Private)(add)(lf32(0.0f))
    ) |> oclToMem(AddressSpace.Global) |> map(drop(1))).be(scannedChunks => {

      let(
        scannedChunks |>
          map(idx(lidx(m-1, m))) |> take(n-1) |>
          oclScanSeqInclusive(AddressSpace.Private)(add)(lf32(0.0f)) |>
          oclToMem(AddressSpace.Global)
      ).be(scanOfLastElementOfChunks =>
        zip(scannedChunks)(scanOfLastElementOfChunks) |>
          map(fun(pair => pair._1 |> map(fun(x => x + pair._2)))) |>
          join |> padCst(1)(0)(lf32(0.0f)) |> mapGlobal(0)(fun(x => x))
      )
    })
  )))

  test("Parallel scan compiles to syntactically correct OpenCL") {
    util.withExecutor {
      val kernel = gen.opencl.kernel.fromExpr(parallelScanOcl)
      println(kernel.code)

      val run = kernel.as[ScalaFunction `(` Int `,` Int `,` Array[Float] `)=>` Array[Float]]

      val inputSize = 8192 * 8
      val m = 256 // Number of elements per thread/chunk
      val n = inputSize/m // Number of threads/chunks
      val data = Array.tabulate(n*m)(x => (x + 1.0f) % 32.0f) // Keep the numbers small

      println(Iterator(
        s"Input size: $inputSize floats",
        s"N (Num Threads): $n",
        s"M (Chunk size): $m"
      ).mkString("\n"))

      val localSize = LocalSize(NDRange(Math.min(n, 128), 1, 1))
      val globalSize = GlobalSize(NDRange(n, 1, 1))
      val (output, runtime) = run(localSize, globalSize)(n `,` m `,` data)

      println(s"Execution time: ${runtime.value} ms")

      val gold = data.scanLeft(0.0f)(_ + _)

      println(s"Output size: ${output.length}")
      //println(gold.iterator.map(_.toString).mkString(", "))
      //println(output.iterator.map(_.toString).mkString(", "))
      assert(gold.zip(output).forall(x => x._1 == x._2))
    }
  }

  val size8Static = fun(ArrayType(8, f32))(input =>
    let(input |> split(2) |> mapSeq(oclReduceSeq(AddressSpace.Private)(add)(lf32(0.0f))) |> oclToMem(AddressSpace.Global))
      .be(up1 => let(up1 |> split(2) |> mapSeq(oclReduceSeq(AddressSpace.Private)(add)(lf32(0.0f))) |> oclToMem(AddressSpace.Global))
        .be(
          up2 =>
            larr(Seq(FloatData(0.0f))) |> fun(prev =>
              zip(prev)(up2 |> split(2)) |>
                mapSeq(fun(pair => oclScanSeqInclusive(AddressSpace.Private)(add)(pair._1)(pair._2))) |>
                join |>
                oclToMem(AddressSpace.Global) |>
                split(3) |> map(take(2)) |> join
            ) |> fun(prev =>
              zip(prev)(up1 |> split(2)) |>
                mapSeq(fun(pair => oclScanSeqInclusive(AddressSpace.Private)(add)(pair._1)(pair._2)))|>
                join |>
                oclToMem(AddressSpace.Global) |>
                split(3) |> map(take(2)) |> join
            ) |> fun(prev =>
              zip(prev)(input |> split(2)) |>
                mapSeq(fun(pair => oclScanSeqInclusive(AddressSpace.Private)(add)(pair._1)(pair._2)))|>
                join |> oclToMem(AddressSpace.Global) |>
                split(3) |> map(take(2)) |> join |>
                mapSeq(fun(x => x))
            )
        )))

  def scanChunkSeq(depth:Int) : ToBeTyped[Expr] = {
    def sum = oclReduceSeq(AddressSpace.Private)(add)(lf32(0.0f))

    @tailrec
    def downsweep(base: ToBeTyped[Expr], stack: List[ToBeTyped[Expr]]): ToBeTyped[Expr] = {
      stack match {
        case prev::rest =>
          val next = zip(base)(prev |> split(2)) |>
            mapSeq(fun(pair => oclScanSeqInclusive(AddressSpace.Private)(add)(pair._1)(pair._2)))|>
            join |>
            oclToMem(AddressSpace.Global) |>
            split(3) |> map(take(2)) |> join
            downsweep(next, rest)
        case Nil => base |> mapSeq(fun(x => x))
      }
    }

    def upsweep(input: ToBeTyped[Expr], depth: Int, stack: List[ToBeTyped[Expr]]): ToBeTyped[Expr] = {
      if (depth == 0) {
        downsweep(larr(Seq(FloatData(0.0f))), stack)
      } else {
        let(input |> split(2) |> mapSeq(sum) |> oclToMem(AddressSpace.Global))
          .be(next => upsweep(next, depth - 1, input::stack))
      }
    }

    val inputSize = Math.pow(2, depth).toInt
    fun(ArrayType(inputSize, f32))(input => upsweep(input, depth, List()))
  }

  def scanChunkPar(inputSize: Int, depth:Int) : ToBeTyped[Expr] = {
    def sum = oclReduceSeqUnroll(AddressSpace.Private)(add)(lf32(0.0f))

    @tailrec
    def downsweep(base: ToBeTyped[Expr], stack: List[ToBeTyped[Expr]]): ToBeTyped[Expr] = {
      stack match {
        case prev::rest =>
          val next = zip(base)(prev |> split(2)) |>
            mapLocal(0)(fun(pair => oclScanSeqUnroll(AddressSpace.Private)(add)(pair._1)(pair._2)))|>
            join
          if (rest == Nil)
            next
          else
          downsweep(next |> oclToMem(AddressSpace.Local), rest)
        case Nil => base |> mapSeq(fun(x => x))
      }
    }

    def upsweep(input: ToBeTyped[Expr], depth: Int, stack: List[ToBeTyped[Expr]]): ToBeTyped[Expr] = {
      if (depth == 0) {
        downsweep(larr(Seq(FloatData(0.0f))) |> mapSeq(fun(x => x)) |> oclToMem(AddressSpace.Local), stack)
      } else {
        let(input |> split(2) |> mapLocal(0)(sum) |> oclToMem(AddressSpace.Local))
          .be(next => upsweep(next, depth - 1, input::stack))
      }
    }

    val chunkSize = Math.pow(2, depth+1).toInt
    if (inputSize % chunkSize != 0) {
      throw new Exception(s"Input size of $inputSize not divisible by chunk size $chunkSize (depth $depth)")
    }
    fun(ArrayType(inputSize, f32))(input =>
      input |> split(chunkSize) |> mapWorkGroup(0)(fun(chunk =>
        upsweep(chunk, depth+1, List()))
      ) |> join
    )
  }


  def scanBlockSums(inputSize: Int, chunkSize: Int): ToBeTyped[Expr] = {
    if (inputSize % chunkSize != 0) {
      throw new Exception(s"Input size of $inputSize not divisible by chunk size $chunkSize")
    }
    fun(ArrayType(inputSize, f32))(input => input |> split(chunkSize) |>
      mapGlobal(0)(oclReduceSeq(AddressSpace.Private)(add)(lf32(0.0f)))
    )
  }

  def scanParChunkSum(inputSize: Int, depth: Int): ToBeTyped[Expr] = {
    val chunkSize = Math.pow(2, depth).toInt
    if (inputSize % chunkSize != 0) {
      throw new Exception(s"Input size of $inputSize not divisible by chunk size $chunkSize (depth $depth)")
    }
    val numChunks = inputSize / chunkSize
    fun(ArrayType(inputSize, f32))(input =>
      fun(ArrayType(numChunks, f32))(partials =>
        zip(input |> split(chunkSize))(partials) |> mapGlobal(0)(fun(pair => pair._1 |> mapSeq(fun(x => x + pair._2))))
      )
    )
  }

  def scanParChunkSum2D(inputSize: Int, chunkSize: Int): ToBeTyped[Expr] = {
    if (inputSize % chunkSize != 0) {
      throw new Exception(s"Input size of $inputSize not divisible by chunk size $chunkSize")
    }
    val numChunks = inputSize / chunkSize
    fun(ArrayType(inputSize, f32))(input =>
      fun(ArrayType(numChunks, f32))(partials =>
        zip(input |> split(chunkSize))(partials) |> mapWorkGroup(0)(fun(pair =>
          pair._1 |> split(1) |> mapLocal(0)(mapSeq(fun(x => x + pair._2))) |> transpose |> join
        ))
      )
    )
  }

  case class Timing(
                     partials: Double,
                     blockSums: Double,
                     aggregate: Double,
                   ) {
    val total: Double = partials + blockSums + aggregate

    def printout(): Unit = {
      println("Timing report:")
      println(s"Partials: $partials ms")
      println(s"Block Sums: $blockSums ms")
      println(s"Aggregate: $aggregate ms")
      println(s"Total: $total ms")
    }


  }
  object Timing {
    def average(timings: Vector[Timing]): Timing = {
      val Timing(p,b,a) = timings.iterator.reduce((a, b) => Timing(
        partials = a.partials + b.partials,
        blockSums = a.blockSums + b.blockSums,
        aggregate = a.aggregate + b.aggregate
      ))

      Timing(
        partials = p/timings.size,
        blockSums = b/timings.size,
        aggregate = a/timings.size
      )
    }
  }

  def runParallelUnrolled(inputSize: Int, blockSize: Int, factor: Int, depth: Int, blockScan: () => ToBeTyped[Expr], doGoldCheck:Boolean  = true): Timing = {
    val numBlocks = inputSize/blockSize

    val input = Array.tabulate(inputSize)(i => (i % 4.0f) + 1.0f)

    val partialsThreads = Math.pow(factor, depth).toInt
    val blockSumThreads = 128
    val aggregateThreads = blockSize/factor

    println(s"Partials parallel threads: ${partialsThreads}")
    println(s"Num blocks: ${numBlocks}")

    val (partials, partialTime) = {
      val expr = blockScan()//scanChunkPar(inputSize, depth)
      val localSize = LocalSize(NDRange(partialsThreads, 1, 1))
      val globalSize = GlobalSize(NDRange(partialsThreads * numBlocks, 1, 1))
      val kernel = gen.opencl.kernel.apply(localSize, globalSize).fromExpr(expr)

      val run = kernel.as[ScalaFunction  `(` Array[Float] `)=>` Array[Float]]

      val (partials, r) = run(localSize, globalSize)(HCons(HNil, input))

      //println(kernel.code)
      //println("Partials")
      //println(partials.map(_.toString).mkString(", "))
      (partials, r)
    }

    val (blockSums, blockSumsTime) = {
      val expr = scanBlockSums(inputSize, blockSize)
      val kernel = gen.opencl.kernel.fromExpr(expr)

      val run = kernel.as[ScalaFunction  `(` Array[Float] `)=>` Array[Float]]
      val localSize = LocalSize(NDRange(blockSumThreads, 1, 1))
      val globalSize = GlobalSize(NDRange(8192, 1, 1))
      val (blockSums, r) = run(localSize, globalSize)(HCons(HNil, input))

      //println(kernel.code)
      (blockSums, r)
    }

    val localScan = blockSums.scanLeft(0.0f)(_ + _)

    val (oclOutput, aggregateTime) = {
      val expr = scanParChunkSum2D(inputSize, blockSize)

      val localSize = LocalSize(NDRange(blockSize, 1, 1))
      val globalSize = GlobalSize(NDRange(blockSize*numBlocks, 1, 1))
      val kernel = gen.opencl.kernel.apply(localSize, globalSize).fromExpr(expr)

      val run = kernel.as[ScalaFunction  `(` Array[Float] `,` Array[Float] `)=>` Array[Float]]
      val (output, r) = run(localSize, globalSize)(partials `,` localScan)

      println(kernel.code)
      //println("Finals")
      //println(output.map(_.toString).mkString(", "))
      (output, r)
    }

    if (doGoldCheck) {
      val gold = input.scanLeft(0.0f)(_ + _)

      val goldCheck = oclOutput.zip(gold).forall { case (x, y) => x == y }
      println(s"Gold check: ${if (goldCheck) "OK" else "ERROR"}")
      if (!goldCheck) {
        val firstError = oclOutput.zip(gold).iterator.zipWithIndex.find((x => x._1._1 != x._1._2)).map(_._2).get
        println(s"First error at index $firstError")
      }
      assert(goldCheck)
    }

    Timing(partialTime.value, blockSumsTime.value, aggregateTime.value)
  }


  test("Unrolled scan parallel") {
    val numSizes = 128
    val runsPerSize = 4
    val baseMult = 256

    val depth = 5
    val blockSize = 64;

    val csv = new StringBuilder

    util.withExecutor {
      (0 until numSizes).foreach(size_i => {
        val inputSize = (size_i + 1) * blockSize * baseMult
        println(s"Input size:\t${inputSize}\tRun:\t${size_i+1}/$numSizes")

        val timings = (0 until runsPerSize).iterator.map(_ => {
          runParallelUnrolled(inputSize, blockSize, factor = 2, depth, () => scanChunkPar(inputSize, depth))
        }).toVector
        val average = Timing.average(timings)
        csv ++= s"dpia,$inputSize,${average.partials},${average.total}\n"
      })
    }
    println(csv)
  }


  test("Host code attempt") {

    // Use the rewrite system to generate the block scan
    val BLOCK_SIZE = 64

    val blockScan = {
      val initExp = {
        fun(ArrayType(BLOCK_SIZE, f32))(input =>
          input |> scan(add)(lf32(0.0f))
        )
      }
      rise.elevate.rules.traversal.body(rise.elevate.rules.workEfficientScan.blockScan())(initExp).get
    }

    val computeBlockSums = depFun((num_blocks:Nat) => fun(ArrayType(BLOCK_SIZE*num_blocks, f32))(input => input |> split(BLOCK_SIZE) |>
      mapGlobal(0)(oclReduceSeq(AddressSpace.Private)(add)(lf32(0.0f)))
    ))

    val aggregateBlockScans = {
      depFun((num_blocks:Nat) =>
      fun(ArrayType(num_blocks*BLOCK_SIZE, f32))(input =>
      fun(ArrayType(num_blocks, f32))(blockScans =>
        zip(input |> split(BLOCK_SIZE))(blockScans) |> mapGlobal(0)(fun(pair => pair._1 |> mapSeq(fun(x => x + pair._2))))
      )
      ))
    }

    val e =  depFun((num_blocks:Nat) => fun(ArrayType(BLOCK_SIZE*num_blocks, f32))(input =>
      // Compute the partial scans (via local block scan)
      oclRun(LocalSize(1), GlobalSize(1))(input |> split(BLOCK_SIZE) |> mapWorkGroup(0)(blockScan) |> join)
        |> store(partials =>
          // Compute the block sums (ouch)
          oclRun(LocalSize(1), GlobalSize(1))(computeBlockSums(num_blocks)(partials)) |>
            store(bSums =>
              // Compute the global block scan (on CPU)
              bSums |> scanSeq(add)(lf32(0.0f)) |> store(bScans =>
                  oclRun(LocalSize(1), GlobalSize(1))(aggregateBlockScans(num_blocks)(partials)(bScans))
              )
            )
        )
    ))

    val m = gen.opencl.hosted.fromExpr(e)

    // How to get the kernel code?
    val code = shine.OpenCL.Module.translateToString(m) + gen.c.function.asString(m.hostCode)
    println(code)
  }


  private def log(base: Int, x: Double) = Math.log10(x) / Math.log10(base.toInt)

  test("scan par rewrite") {
    val blockSize = 64
    val initExp = {
      fun(ArrayType(blockSize, f32))(input =>
        input |> scan(add)(lf32(0.0f))
      )
    }

    val inputSize = 1024
    val maxDepth = log(2, blockSize).toInt
    val skipDepth = 0
    val depth = maxDepth - skipDepth - 1


    val rewritten = rise.elevate.rules.traversal.body(rise.elevate.rules.workEfficientScan.blockScan(skipDepth))(initExp).get
    util.withExecutor {
      runParallelUnrolled(inputSize, blockSize, factor = 2, depth, () => fun(ArrayType(inputSize, f32))(input => {
        input |> split(blockSize) |> mapWorkGroup(0)(rewritten) |> join
      })).printout()
    }

    //val kernel = gen.opencl.kernel.fromExpr(rewritten)
    //println(kernel.code)
  }


  test("scan par rewrite explore") {
    val maxBlockSize = 512
    val numBlocks = 50_000
    val inputSize = maxBlockSize * numBlocks
    val doGoldCheck = inputSize <= 512 * 10_000


    case class Entry(inputSize: Int, blockSize:Int, factor:Int, skipDepth: Int, time: Double) {
      def printout: String = s"${inputSize},${blockSize},${factor},${skipDepth},${time}"
    }

    // Explorations
    val blockSizes = Vector(32, 64, 128, 256, 512)
    val skipDepths = Vector(0, 1, 2, 3, 4)
    val factors = Vector(2)

    val entries = for (
      blockSize <- blockSizes;
      skip <- skipDepths;
      factor <- factors
      if log(factor, blockSize) % 1 == 0;
      maxDepth = log(factor, blockSize).toInt;
      depth = maxDepth - skip - 1
      if depth >= 0
    ) yield {

      val initExp = {
        fun(ArrayType(blockSize, f32))(input =>
          input |> scan(add)(lf32(0.0f))
        )
      }

      val rewritten = rise.elevate.rules.traversal.body(rise.elevate.rules.workEfficientScan.blockScan(
        factor = factor,
        skipDepth = skip))(initExp).get
      util.withExecutor {
        val makeKenrel = () => fun(ArrayType(inputSize, f32))(input => {
          input |> split(blockSize) |> mapWorkGroup(0)(rewritten) |> join
        })
        val timing = runParallelUnrolled(inputSize, blockSize, factor, depth, makeKenrel, doGoldCheck)
        timing.printout()
        Entry(inputSize, blockSize, factor, skip, timing.partials)
      }
    }
    entries.sortBy(_.time).foreach { e => println(e.printout) }
  }

  test("scan par rewrite explore factors") {
    val numSizes = 64
    val baseMult = 256

    val inputSizes = (0 until numSizes).map(numSize =>  (numSize + 1) * 64 * 32 * baseMult)

    val maxBlockSize = 512
    val numBlocks = 10_000
    val inputSize = maxBlockSize * numBlocks
    val doGoldCheck = inputSize <= 512 * 5_000


    case class Entry(inputSize: Int, blockSize:Int, factor:Int, skipDepth: Int, time: Double) {
      def printout: String = s"${inputSize},${blockSize},${factor},${skipDepth},${time}"
    }

    // Explorations
    val blockSize = 512
    val skipDepths = Vector(2)
    val factors = Vector(2, 4)

    factors.foreach(factor => {
      val l = log(factor, blockSize)
      println(s"factor=${factor} -- ${l % 1 == 0} -- $l")
    }
    )

    val entries = for (
      skip <- skipDepths;
      factor <- factors
      if log(factor, blockSize) % 1 == 0;
      maxDepth = log(factor, blockSize).toInt;
      depth = maxDepth - skip - 1
      if depth >= 0
    ) yield {

      val initExp = {
        fun(ArrayType(blockSize, f32))(input =>
          input |> scan(add)(lf32(0.0f))
        )
      }

      val rewritten = rise.elevate.rules.traversal.body(rise.elevate.rules.workEfficientScan.blockScan(
        factor = factor,
        skipDepth = skip))(initExp).get
      util.withExecutor {
        val makeKenrel = () => fun(ArrayType(inputSize, f32))(input => {
          input |> split(blockSize) |> mapWorkGroup(0)(rewritten) |> join
        })
        val timing = runParallelUnrolled(inputSize, blockSize, factor, depth, makeKenrel, doGoldCheck)
        timing.printout()
        Entry(inputSize, blockSize, factor, skip, timing.partials)
      }
    }
    entries.sortBy(_.time).foreach { e => println(e.printout) }
  }

  test("Benchmark best") {
    val numSizes = 64
    val baseMult = 256

    val inputSizes = (0 until numSizes).map(numSize =>  (numSize + 1) * 64 * 32 * baseMult)

    val maxBlockSize = 512
    val numBlocks = 10_000
    val inputSize = maxBlockSize * numBlocks
    val doGoldCheck = inputSize <= 512 * 5_000


    case class Entry(inputSize:Int, blockSize:Int, factor:Int, skipDepth: Int, time: Double, aggregate:Double, total: Double) {
      def printout: String = s"${inputSize},${blockSize},${factor},${skipDepth},$time,$aggregate,$total"
    }

    val blockSize = 512
    val skip = 2
    val factor = 2

    val entries = for (
      inputSize <- inputSizes;
      if log(factor, blockSize) % 1 == 0;
      maxDepth = log(factor, blockSize).toInt;
      depth = maxDepth - skip - 1
      if depth >= 0
    ) yield {

      val initExp = {
        fun(ArrayType(blockSize, f32))(input =>
          input |> scan(add)(lf32(0.0f))
        )
      }

      val rewritten = rise.elevate.rules.traversal.body(rise.elevate.rules.workEfficientScan.blockScan(
        factor = factor,
        skipDepth = skip))(initExp).get
      util.withExecutor {
        val makeKenrel = () => fun(ArrayType(inputSize, f32))(input => {
          input |> split(blockSize) |> mapWorkGroup(0)(rewritten) |> join
        })
        val timing = runParallelUnrolled(inputSize, blockSize, factor, depth, makeKenrel, doGoldCheck)
        timing.printout()
        Entry(inputSize, blockSize, factor, skip, timing.partials, timing.aggregate, timing.partials + timing.aggregate)
      }
    }
    entries.foreach { e => println(e.printout) }
  }

  test("Benchmark middle version") {
    val numSizes = 64
    val baseMult = 256

    val inputSizes = (0 until numSizes).map(numSize =>  (numSize + 1) * 64 * 32 * baseMult)

    case class Entry(inputSize:Int, blockSize:Int, factor:Int, skipDepth: Int, time: Double, aggregate:Double, total: Double) {
      def printout: String = s"${inputSize},${blockSize},${factor},${skipDepth},$time,$aggregate,$total"
    }


    val blockSize = 256
    val skip = 0
    val factor = 2

    val entries = for (
      inputSize <- inputSizes;
      if log(factor, blockSize) % 1 == 0;
      maxDepth = log(factor, blockSize).toInt;
      depth = maxDepth - skip - 1
      if depth >= 0
    ) yield {

      val initExp = {
        fun(ArrayType(blockSize, f32))(input =>
          input |> scan(add)(lf32(0.0f))
        )
      }

      val rewritten = rise.elevate.rules.traversal.body(rise.elevate.rules.workEfficientScan.blockScan(
        factor = factor,
        skipDepth = skip))(initExp).get
      util.withExecutor {
        val makeKenrel = () => fun(ArrayType(inputSize, f32))(input => {
          input |> split(blockSize) |> mapWorkGroup(0)(rewritten) |> join
        })
        val timing = runParallelUnrolled(inputSize, blockSize, factor, depth, makeKenrel, doGoldCheck = false)
        timing.printout()
        Entry(inputSize, blockSize, factor, skip, timing.partials, timing.aggregate, timing.partials + timing.aggregate)
      }
    }
    entries.foreach { e => println(e.printout) }
  }
}

