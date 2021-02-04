package shine.cuda

import arithexpr.arithmetic.ArithExpr
import shine.C.AST.Node
import shine.DPIA.Phrases.Identifier
import shine.DPIA.Types._
import shine.DPIA.{Nat, VarType}
import shine.C
import shine.OpenCL.{GlobalSize, LocalSize, NDRange, get_global_size, get_local_size, get_num_groups}
import yacx.Executor.BenchmarkResult
import yacx.{ByteArg, Devices, DoubleArg, Executor, FloatArg, HalfArg, IntArg, LongArg, Options, Program, ShortArg}

import scala.collection.Seq
import scala.collection.immutable.List

//TODO: this class needs to be refatored
//noinspection ScalaDocParserErrorInspection
case class Kernel(decls: Seq[C.AST.Decl],
                  kernel: C.AST.FunDecl, //TODO
                  outputParam: Identifier[AccType],
                  inputParams: Seq[Identifier[ExpType]],
                  dynamicSharedMemory: Long,
                  intermediateParams: Seq[Identifier[VarType]],
                  printer: Node => String
                 ) extends util.Kernel(decls, kernel, outputParam, inputParams, intermediateParams, printer) {
  //TODO remove this
  def replaceSecondUnnecessaryReduceAllocation(code: String): String = {
    val lines = code.split("\n")

    def serachOclReduceSeq(startIndex: Int): Int = {
      var i = startIndex
      while (i < lines.length){
        if (lines(i).contains("/* oclReduceSeq */")) {
          return i;
        }

        i += 1
      }

      -1;
    }

    def getVName(decl: String): String = {
      if (!decl.contains("wmma::fragment")) return null

      val indexVNameStart = decl.indexOf(">") + 2
      val indexVNameEnd = decl.indexOf("[")
      decl.slice(indexVNameStart, indexVNameEnd)
    }

    val index1 = serachOclReduceSeq(0)
    if (index1 == -1) return code;
    val vname1 = getVName(lines(index1+2))
    if (vname1 == null) return code

    val index2 = serachOclReduceSeq(index1+3)
    if (index2 == -1) return code
    val vname2 = getVName(lines(index2+2))
    if (vname2 == null) return code

    val codeNew = new StringBuffer(code.length)

    var i = 0

    while (i < index2+2){
      codeNew.append(lines(i)).append("\n")
      i += 1
    }

    i += 1
    var brackets = 1
    while (brackets > 0){
      val line = lines(i)

      if (line.contains("{"))
        brackets += 1

      if (line.contains("#pragma unroll")) i += 5
      else {
        codeNew.append(line.replace(vname2, vname1))
        codeNew.append("\n")
      }

      if (line.contains("}"))
        brackets -= 1

      i += 1
    }

    while (i < lines.length){
      codeNew.append(lines(i)).append("\n")
      i += 1
    }

    codeNew.toString
  }

  override def code: String = replaceSecondUnnecessaryReduceAllocation(super.code)

  override protected def findParameterMappings(arguments: List[Argument], localSize: LocalSize, globalSize: GlobalSize): Map[Nat, Nat] = {
    val numGroups: NDRange = (
      globalSize.size.x /^ localSize.size.x,
      globalSize.size.y /^ localSize.size.y,
      globalSize.size.z /^ localSize.size.z)
    val sizeVarMapping = collectSizeVars(arguments, Map(
      get_num_groups(0) -> numGroups.x,
      get_num_groups(1) -> numGroups.y,
      get_num_groups(2) -> numGroups.z,
      get_local_size(0) -> localSize.size.x,
      get_local_size(1) -> localSize.size.y,
      get_local_size(2) -> localSize.size.z,
      get_global_size(0) -> globalSize.size.x,
      get_global_size(1) -> globalSize.size.y,
      get_global_size(2) -> globalSize.size.z
    ))

    sizeVarMapping
  }

  override protected def execute(localSize: LocalSize, globalSize: GlobalSize, sizeVarMapping: Map[Nat, Nat], kernelArgs: List[KernelArg], compilerOptions: Array[String]): Double = {
    val kernel = Program.create(code, this.kernel.name).compile(Options.createOptions(compilerOptions:_*))

    val kernelArgsCUDA = kernelArgs.map(_.asInstanceOf[KernelArgCUDA].kernelArg)

    val blocksX = ArithExpr.substitute(globalSize.size.x /^ localSize.size.x, sizeVarMapping).eval
    val blocksY = ArithExpr.substitute(globalSize.size.y /^ localSize.size.y, sizeVarMapping).eval
    val blocksZ = ArithExpr.substitute(globalSize.size.z /^ localSize.size.z, sizeVarMapping).eval
    val threadsX = ArithExpr.substitute(localSize.size.x, sizeVarMapping).eval
    val threadsY = ArithExpr.substitute(localSize.size.y, sizeVarMapping).eval
    val threadsZ = ArithExpr.substitute(localSize.size.z, sizeVarMapping).eval

    val device = Devices.findDevice()
    println(device)

    println(s"Allocated dynamicSharedMemory $dynamicSharedMemory bytes")
    if (device.getSharedMemPerMultiprocessor < dynamicSharedMemory)
      throw new OutOfMemoryError(s"not enough shared memory! found: $dynamicSharedMemory available: <= ${Devices.findDevice().getSharedMemPerMultiprocessor}")

    println(s"Launch kernel with gridDim=($blocksX, $blocksY, $blocksZ), blockDim=($threadsX, $threadsY, $threadsZ)")
    val maxBlocks = device.getMaxGrid
    val maxThreads = device.getMaxBlock
    if (blocksX > maxBlocks(0) || blocksY > maxBlocks(1) || blocksZ > maxBlocks(2))
      throw new IndexOutOfBoundsException(s"not enough blocks! found: ($blocksX, $blocksY, $blocksZ)" +
        s"available: <= (${maxBlocks(0)}, ${maxBlocks(1)}, ${maxBlocks(2)})")
    if (threadsX > maxThreads(0) || threadsY > maxThreads(1) || threadsZ > maxThreads(2))
      throw new IndexOutOfBoundsException(s"not enough threads per block! found: ($blocksX, $blocksY, $blocksZ)" +
        s"available: <= (${maxThreads(0)}, ${maxThreads(1)}, ${maxThreads(2)})")


    kernel.configure(
      blocksX,
      blocksY,
      blocksZ,
      threadsX,
      threadsY,
      threadsZ,
      dynamicSharedMemory
    )


    val runtime = kernel.launch(kernelArgsCUDA.toArray: _*)

    kernel.dispose()

    runtime.getLaunch.asInstanceOf[Double]
  }

  override protected def dispose(kernelArgs: List[KernelArg]): Unit =  kernelArgs.foreach(_.asInstanceOf[KernelArgCUDA].kernelArg.dispose())

  override def benchmark(creator: util.KernelArgCreator, numberOfIterations: Integer, dataSizesBytes: Array[Long]) : BenchmarkResult = {
    val device = Devices.findDevice()

    if (device.getSharedMemPerMultiprocessor < dynamicSharedMemory)
      throw new OutOfMemoryError(s"not enough shared memory found: $dynamicSharedMemory available: <= ${device.getSharedMemPerMultiprocessor}")

    val creatorYacx: Executor.KernelArgCreator = new Executor.KernelArgCreator {
      private var lastDataLength = -1
      private var gridX, gridY, gridZ, blockX, blockY, blockZ = 0
      private var kernelArgs: List[KernelArg] = _

      override def getDataLength(dataSizeBytes: Long): Int = creator.getDataLength(dataSizeBytes)

      override def createArgs(dataLength: Int): Array[yacx.KernelArg] = {
        setParams(dataLength)
        kernelArgs.map(_.asInstanceOf[KernelArgCUDA].kernelArg).toArray
      }

      override def getGrid0(dataLength: Int): Int = {
        setParams(dataLength)
        gridX
      }

      override def getGrid1(dataLength: Int): Int = {
        setParams(dataLength)
        gridY
      }

      override def getGrid2(dataLength: Int): Int = {
        setParams(dataLength)
        gridZ
      }

      override def getBlock0(dataLength: Int): Int = {
        setParams(dataLength)
        blockX
      }

      override def getBlock1(dataLength: Int): Int = {
        setParams(dataLength)
        blockY
      }

      override def getBlock2(dataLength: Int): Int = {
        setParams(dataLength)
        blockZ
      }

      override def getSharedMemory(dataSizeBytes: Long): Long = dynamicSharedMemory

      def setParams(dataLength: Int): Unit = {
        if (lastDataLength != dataLength) {
          lastDataLength = dataLength

          val gridDim = creator.getGridDim(dataLength)
          val localSize = creator.getBlockDim(dataLength)
          val globalSize = GlobalSize(gridDim.x * localSize.size.x, gridDim.y * localSize.size.y, gridDim.z * localSize.size.z)
          val args = creator.createArgs(dataLength)

          val arguments = constructArguments(inputParams zip args, intermediateParams, Kernel.this.kernel.params.tail)

          val sizeVarMapping = findParameterMappings(arguments, localSize, globalSize)

          val (outputArg, inputArgs) = createKernelArgs(arguments, sizeVarMapping)
          kernelArgs = outputArg :: inputArgs

          gridX = ArithExpr.substitute(globalSize.size.x /^ localSize.size.x, sizeVarMapping).eval
          gridY = ArithExpr.substitute(globalSize.size.y /^ localSize.size.y, sizeVarMapping).eval
          gridZ = ArithExpr.substitute(globalSize.size.z /^ localSize.size.z, sizeVarMapping).eval
          blockX = ArithExpr.substitute(localSize.size.x, sizeVarMapping).eval
          blockY = ArithExpr.substitute(localSize.size.y, sizeVarMapping).eval
          blockZ = ArithExpr.substitute(localSize.size.z, sizeVarMapping).eval
        }
      }
    }

    Executor.benchmark(code, this.kernel.name, Options.createOptions(compilerOptions.toSeq:_*), device, numberOfIterations, creatorYacx, dataSizesBytes.toArray:_*)
  }

  override protected def createLocalArg(sizeInBytes: Long): KernelArgCUDA = ???

  override protected def createOutputArg(numberOfElements: Int, dataType: DataType): KernelArgCUDA = {
    KernelArgCUDA(dataType match {
      case shine.DPIA.Types.i8 =>
        println(s"Allocated global byte-argument with $numberOfElements bytes")
        ByteArg.createOutput(numberOfElements);
      case shine.DPIA.Types.i16 =>
        println(s"Allocated global short-argument with ${numberOfElements * 2L} bytes")
        ShortArg.createOutput(numberOfElements);
      case shine.DPIA.Types.i32 | shine.DPIA.Types.int =>
        println(s"Allocated global int-argument with ${numberOfElements * 4L} bytes")
        IntArg.createOutput(numberOfElements);
      case shine.DPIA.Types.i64 =>
        println(s"Allocated global long-argument with ${numberOfElements * 8L} bytes")
        LongArg.createOutput(numberOfElements);
      case shine.DPIA.Types.f16 =>
        println(s"Allocated global half-argument with ${numberOfElements * 2L} bytes")
        HalfArg.createOutput(numberOfElements);
      case shine.DPIA.Types.f32 =>
        println(s"Allocated global float-argument with ${numberOfElements * 4L} bytes")
        FloatArg.createOutput(numberOfElements);
      case shine.DPIA.Types.f64 =>
        println(s"Allocated global double-argument with ${numberOfElements * 8L} bytes")
        DoubleArg.createOutput(numberOfElements);
      case _ => throw new IllegalArgumentException("Argh Return type of the given lambda expression " +
        "not supported: " + dataType.toString)
    })
  }

  override protected def asArray[R](dt: DataType, output: KernelArg): R = {
    val outputCUDA = output.asInstanceOf[KernelArgCUDA].kernelArg

    (dt match {
      case shine.DPIA.Types.i8 => outputCUDA.asInstanceOf[ByteArg].asByteArray()
      case shine.DPIA.Types.i16 => outputCUDA.asInstanceOf[ShortArg].asShortArray()
      case shine.DPIA.Types.i32 | shine.DPIA.Types.int => outputCUDA.asInstanceOf[IntArg].asIntArray()
      case shine.DPIA.Types.i64 => outputCUDA.asInstanceOf[LongArg].asLongArray()
      case shine.DPIA.Types.f16 => outputCUDA.asInstanceOf[HalfArg].asFloatArray()
      case shine.DPIA.Types.f32 => outputCUDA.asInstanceOf[FloatArg].asFloatArray()
      case shine.DPIA.Types.f64 => outputCUDA.asInstanceOf[DoubleArg].asDoubleArray()
      case _ => throw new IllegalArgumentException("Return type of the given lambda expression " +
        "not supported: " + dt.toString)
    }).asInstanceOf[R]
  }

  override protected def createInputArg(arg: Any, dt: DataType): KernelArgCUDA = {
    KernelArgCUDA(arg match {
      case b: Byte => createValueArg(b)
      case ab: Array[Byte] => createArrayArg(ab)
      case ab: Array[Array[Byte]] => createArrayArg(ab.flatten)
      case ab: Array[Array[Array[Byte]]] => createArrayArg(ab.flatten.flatten)
      case ab: Array[Array[Array[Array[Byte]]]] => createArrayArg(ab.flatten.flatten.flatten)

      case s: Short => createValueArg(s)
      case as: Array[Short] => createArrayArg(as)
      case as: Array[Array[Short]] => createArrayArg(as.flatten)
      case as: Array[Array[Array[Short]]] => createArrayArg(as.flatten.flatten)
      case as: Array[Array[Array[Array[Short]]]] => createArrayArg(as.flatten.flatten.flatten)

      case i: Int => createValueArg(i)
      case ai: Array[Int] => createArrayArg(ai)
      case ai: Array[Array[Int]] => createArrayArg(ai.flatten)
      case ai: Array[Array[Array[Int]]] => createArrayArg(ai.flatten.flatten)
      case ai: Array[Array[Array[Array[Int]]]] => createArrayArg(ai.flatten.flatten.flatten)

      case l: Long => createValueArg(l)
      case al: Array[Long] => createArrayArg(al)
      case al: Array[Array[Long]] => createArrayArg(al.flatten)
      case al: Array[Array[Array[Long]]] => createArrayArg(al.flatten.flatten)
      case al: Array[Array[Array[Array[Long]]]] => createArrayArg(al.flatten.flatten.flatten)

      case f: Float =>
        if (dt == f32)
          createValueArg(f)
        else
          createValueArgHalf(f)

      case af: Array[Float] =>
        if (dt.isInstanceOf[ArrayType] && dt.asInstanceOf[ArrayType].elemType == f32)
          createArrayArg(af)
        else
          createArrayArgHalf(af)

      case af: Array[Array[Float]] =>
        createInputArg(af.flatten, dt.asInstanceOf[ArrayType].elemType).kernelArg

      case af: Array[Array[Array[Float]]] =>
        createInputArg(af.flatten, dt.asInstanceOf[ArrayType].elemType).kernelArg

      case af: Array[Array[Array[Array[Float]]]] =>
        createInputArg(af.flatten, dt.asInstanceOf[ArrayType].elemType).kernelArg

      case d: Double => createValueArg(d)
      case ad: Array[Double] => createArrayArg(ad)
      case ad: Array[Array[Double]] => createArrayArg(ad.flatten)
      case ad: Array[Array[Array[Double]]] => createArrayArg(ad.flatten.flatten)
      case ad: Array[Array[Array[Array[Double]]]] => createArrayArg(ad.flatten.flatten.flatten)

      case p: Array[(_, _)] => p.head match {
        case (_: Int, _: Float) =>
          IntArg.create(flattenToArrayOfInts(p.asInstanceOf[Array[(Int, Float)]]): _*)
        case _ => ???
      }
      case pp: Array[Array[(_, _)]] => pp.head.head match {
        case (_: Int, _: Float) =>
          IntArg.create(pp.flatMap(a => flattenToArrayOfInts(a.asInstanceOf[Array[(Int, Float)]])): _*)
        case _ => ???
      }

      case _ => throw new IllegalArgumentException("Kernel argument is of unsupported type: " +
        arg.getClass.getName)
    })
  }

  private def createArrayArg(array: Array[Byte]): ByteArg = {
    println(s"Allocated global byte-argument with ${array.length * 1L} bytes")
    ByteArg.create(array: _*)
  }

  private def createArrayArg(array: Array[Short]): ShortArg = {
    println(s"Allocated global short-argument with ${array.length * 2L} bytes")
    ShortArg.create(array: _*)
  }

  private def createArrayArg(array: Array[Int]): IntArg = {
    println(s"Allocated global int-argument with ${array.length * 4L} bytes")
    IntArg.create(array: _*)
  }

  private def createArrayArg(array: Array[Long]): LongArg = {
    println(s"Allocated global long-argument with ${array.length * 8L} bytes")
    LongArg.create(array: _*)
  }

  private def createArrayArgHalf(array: Array[Float]): HalfArg = {
    println(s"Allocated global half-argument with ${array.length * 2L} bytes")
    HalfArg.create(array: _*)
  }

  private def createArrayArg(array: Array[Float]): FloatArg = {
    println(s"Allocated global float-argument with ${array.length * 4L} bytes")
    FloatArg.create(array: _*)
  }

  private def createArrayArg(array: Array[Double]): DoubleArg = {
    println(s"Allocated global double-argument with ${array.length * 8L} bytes")
    DoubleArg.create(array: _*)
  }

  private def createValueArg(value: Byte): yacx.KernelArg = {
    println(s"Allocated value byte-argument with 1 bytes")
    ByteArg.createValue(value)
  }

  private def createValueArg(value: Short): yacx.KernelArg = {
    println(s"Allocated value short-argument with 2 bytes")
    ShortArg.createValue(value)
  }

  private def createValueArg(value: Int): yacx.KernelArg = {
    println(s"Allocated value int-argument with 4 bytes")
    IntArg.createValue(value)
  }

  private def createValueArg(value: Long): yacx.KernelArg = {
    println(s"Allocated value long-argument with 8 bytes")
    LongArg.createValue(value)
  }

  private def createValueArgHalf(value: Float): yacx.KernelArg = {
    println(s"Allocated value half-argument with 2 bytes")
    HalfArg.createValue(value)
  }

  private def createValueArg(value: Float): yacx.KernelArg = {
    println(s"Allocated value float-argument with 4 bytes")
    FloatArg.createValue(value)
  }

  private def createValueArg(value: Double): yacx.KernelArg = {
    println(s"Allocated value double-argument with 8 bytes")
    DoubleArg.createValue(value)
  }
}