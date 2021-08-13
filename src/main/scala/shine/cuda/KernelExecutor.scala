package shine.cuda

import arithexpr.arithmetic._
import rise.core.types.{AddressSpaceIdentifier, DataType, NatIdentifier, NatToDataLambda}
import rise.core.types.DataType._
import shine.C.AST.ParamDecl
import shine.C.AST.ParamKind
import shine.DPIA.Types._
import shine.DPIA._
import shine.OpenCL
import shine.OpenCL.{GlobalSize, HList, LocalSize, NDRange, get_global_size, get_local_size, get_num_groups}
import shine.cuda.AST.Kernel
import util.Time.ms
import util.{Time, TimeSpan}
import util.gen
import yacx.Executor.BenchmarkResult
import yacx.{ByteArg, Devices, DoubleArg, Executor, FloatArg, HalfArg, IntArg, KernelArg, LongArg, Options, Program, ShortArg}

import scala.annotation.tailrec
import scala.collection.immutable.List
import scala.collection.Seq
import scala.language.implicitConversions
import scala.util.{Failure, Success, Try}

//TODO: refactor
object KernelExecutor {

  sealed case class KernelWithSizes(ktu: KernelModule,
                                    localSize: LocalSize,
                                    globalSize: GlobalSize,
                                    compilerOptions: List[String] = List.empty) {
    def as[T, R](implicit ev: T <:< HList): T => (R, TimeSpan[Time.ms]) = {
      FromKernelModule(ktu, compilerOptions).as[T, R](localSize, globalSize)
    }

    def benchmark(creator: KernelArgCreator, numberOfIterations: Integer, dataSizesBytes: Array[Long]) : BenchmarkResult =
      FromKernelModule(ktu, compilerOptions).benchmark(creator, numberOfIterations, dataSizesBytes)

    def code: String = util.gen.cuda.kernel.asString(ktu)

    def forgetSizes: KernelNoSizes = KernelNoSizes(ktu)
  }

  sealed case class KernelNoSizes(ktu: KernelModule,
                                  compilerOptions: List[String] = List.empty) {
    def as[T, R](implicit ev: T <:< HList): AS[T, R] = AS()

    case class AS[T, R]()(implicit ev: T <:< HList) {
      def apply(localSize: LocalSize, globalSize: GlobalSize): T => (R, TimeSpan[Time.ms]) = {
        FromKernelModule(ktu, compilerOptions).as[T, R](localSize, globalSize)
      }

      def withSizes(localSize: LocalSize, globalSize: GlobalSize): T => (R, TimeSpan[Time.ms]) = {
        FromKernelModule(ktu, compilerOptions).as[T, R](localSize, globalSize)
      }
    }

    def benchmark(creator: KernelArgCreator, numberOfIterations: Integer, dataSizesBytes: Array[Long]) : BenchmarkResult =
      FromKernelModule(ktu, compilerOptions).benchmark(creator, numberOfIterations, dataSizesBytes)

    def code: String = util.gen.cuda.kernel.asString(ktu)
  }

  object KernelNoSizes {
    implicit def fromKernelModule(ktu: KernelModule): KernelNoSizes =
      KernelNoSizes(ktu)
  }

  abstract class KernelArgCreator {
    def getDataLength(dataSizeBytes: Long): Int

    def createArgs(dataLength: Int): Array[Any]

    def getGridDim(dataLength: Int) : NDRange

    def getBlockDim(dataLength: Int) : LocalSize
  }

  case class FromKernelModule(ktu: KernelModule, compilerOptions: List[String]) {
    assert(ktu.kernels.size == 1)

    val kernel: Kernel = ktu.kernels.head
    val dynamicSharedMemory = kernel.dynamicSharedMemory
    val code: String = gen.cuda.kernel.asString(ktu)
    val outputParam: (ParamDecl, ParamKind) = kernel.outputParams.head

    /** This method will return a Scala function which executed the kernel via CUDA or OpenCL and returns its
      * // result and the time it took to execute the kernel.
      * //
      * // A type annotation `F` has to be provided which specifies the type of the Scala function.
      * // The following syntax is used for this (which implementation can be found in
      * // cuda/Types.scala):
      * //
      * // <kernel>.as[ScalaFunction `(` <Arg1Type> `,` <Arg2Type> `,` <...> `)=>` <ReturnType> ]
      * //
      * // where all text in < > is to be replaced with the appropriate Scala terms.
      * // An example for the dot product which takes two arrays of floats and returns an array of float
      * // would be:
      * //
      * // dotKernel.as[ScalaFunction `(` Array[Float] `,` Array[Float] `)=>` Array[Float]]
      * *
      * NB: If the kernel takes a single argument, then the invocation must use this syntax
      * (Example with a kernel of type Array[Float] => Array[Float]
      * *
      * val kernelF = kernel.as[ScalaFunction`(`Array[Float]`)=>`Array[Float]]
      * val (result, time) = kernelF(xs `;`)
      */
    def as[T, R](localSize: LocalSize, globalSize: GlobalSize)
                (implicit ev: T <:< HList): T => (R, TimeSpan[Time.ms]) = {
      (hArgs: T) => {
        val args: List[Any] = hArgs.toList
        assert(kernel.inputParams.length == args.length)

        //Match up corresponding dpia parameter/intermediate parameter/scala argument (when present) in a convenience
        //structure
        val arguments = constructArguments(
          kernel.inputParams zip args,
          kernel.temporaryParams,
          kernel.params.tail)

        //First, we want to find all the parameter mappings
        val sizeVarMapping = findParameterMappings(arguments, localSize, globalSize)

        val (outputArg, inputArgs) = createKernelArgs(arguments, sizeVarMapping)
        val kernelArgs = outputArg :: inputArgs

        List(localSize match {
          case LocalSize(x) if !x.isEvaluable => Some(s"local size is not evaluable (currently set to $x)")
          case _ => None
        }, globalSize match {
          case GlobalSize(x) if !x.isEvaluable => Some(s"local size is not evaluable (currently set to $x)")
          case _ => None
        }).filter(_.isDefined).map(_.get) match {
          case Nil =>
          case problems =>
            val errorMessage = "Cannot run kernel:\n" ++ problems.reduce(_ ++ _)
            throw new Exception(errorMessage)
        }

        val runtime = execute(localSize, globalSize, sizeVarMapping, kernelArgs, compilerOptions.toArray)

        val dt = outputParam._2.typ
        assert(dt.isInstanceOf[ArrayType] || dt.isInstanceOf[DepArrayType])
        val output = asArray[R](getOutputType(dt), outputArg)

        dispose(kernelArgs)

        (output, TimeSpan.inMilliseconds(runtime))
      }
    }

    private def findParameterMappings(arguments: List[Argument], localSize: LocalSize, globalSize: GlobalSize): Map[Nat, Nat] = {
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

    private def execute(localSize: LocalSize, globalSize: GlobalSize, sizeVarMapping: Map[Nat, Nat], kernelArgs: List[KernelArg], compilerOptions: Array[String]): Double = {
      val kernel = Program.create(code, this.kernel.name).compile(Options.createOptions(compilerOptions:_*))

      val blocksX = ArithExpr.substitute(globalSize.size.x /^ localSize.size.x, sizeVarMapping).eval
      val blocksY = ArithExpr.substitute(globalSize.size.y /^ localSize.size.y, sizeVarMapping).eval
      val blocksZ = ArithExpr.substitute(globalSize.size.z /^ localSize.size.z, sizeVarMapping).eval
      val threadsX = ArithExpr.substitute(globalSize.size.x, sizeVarMapping).eval
      val threadsY = ArithExpr.substitute(globalSize.size.y, sizeVarMapping).eval
      val threadsZ = ArithExpr.substitute(globalSize.size.z, sizeVarMapping).eval

      val device = Devices.findDevice()
      println(device)

      println(s"Allocated dynamicSharedMemory ${dynamicSharedMemory} bytes")
      if (device.getSharedMemPerMultiprocessor < dynamicSharedMemory)
        throw new OutOfMemoryError(s"not enough shared memory! found: ${dynamicSharedMemory} available: <= ${Devices.findDevice().getSharedMemPerMultiprocessor}")

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

      val runtime = kernel.launch(kernelArgs.toArray: _*)

      kernel.dispose()

      runtime.getLaunch.asInstanceOf[Double]
    }

    private def dispose(kernelArgs: List[KernelArg]): Unit = {
      kernelArgs.foreach(_.dispose())
    }

    def benchmark(creator: KernelArgCreator, numberOfIterations: Integer, dataSizesBytes: Array[Long]) : BenchmarkResult = {
      val device = Devices.findDevice()

      if (device.getSharedMemPerMultiprocessor < dynamicSharedMemory)
        throw new OutOfMemoryError(s"not enough shared memory found: ${dynamicSharedMemory} available: <= ${device.getSharedMemPerMultiprocessor}")

      val creatorYacx: Executor.KernelArgCreator = new Executor.KernelArgCreator {
        private var lastDataLength = -1
        private var gridX, gridY, gridZ, blockX, blockY, blockZ = 0
        private var kernelArgs: List[KernelArg] = _

        override def getDataLength(dataSizeBytes: Long): Int = {
          val dataLength = creator.getDataLength(dataSizeBytes)
          setParams(dataLength)
          dataLength
        }

        override def createArgs(dataLength: Int): Array[yacx.KernelArg] = {
          kernelArgs.toArray
        }

        override def getGrid0(dataLength: Int): Int = {
          gridX
        }

        override def getGrid1(dataLength: Int): Int = {
          gridY
        }

        override def getGrid2(dataLength: Int): Int = {
          gridZ
        }

        override def getBlock0(dataLength: Int): Int = {
          blockX
        }

        override def getBlock1(dataLength: Int): Int = {
          blockY
        }

        override def getBlock2(dataLength: Int): Int = {
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

            val arguments = constructArguments(kernel.inputParams zip args, kernel.temporaryParams, kernel.params.tail)

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

    private def createOutputArg(numberOfElements: Int, dataType: DataType): KernelArg = {
      dataType match {
        case DataType.i8 =>
          println(s"Allocated global byte-argument with $numberOfElements bytes")
          ByteArg.createOutput(numberOfElements);
        case DataType.i16 =>
          println(s"Allocated global short-argument with ${numberOfElements * 2L} bytes")
          ShortArg.createOutput(numberOfElements);
        case DataType.i32 | DataType.int =>
          println(s"Allocated global int-argument with ${numberOfElements * 4L} bytes")
          IntArg.createOutput(numberOfElements);
        case DataType.i64 =>
          println(s"Allocated global long-argument with ${numberOfElements * 8L} bytes")
          LongArg.createOutput(numberOfElements);
        case DataType.f16 =>
          println(s"Allocated global half-argument with ${numberOfElements * 2L} bytes")
          HalfArg.createOutput(numberOfElements);
        case DataType.f32 =>
          println(s"Allocated global float-argument with ${numberOfElements * 4L} bytes")
          FloatArg.createOutput(numberOfElements);
        case DataType.f64 =>
          println(s"Allocated global double-argument with ${numberOfElements * 8L} bytes")
          DoubleArg.createOutput(numberOfElements);
        case _ => throw new IllegalArgumentException("Argh Return type of the given lambda expression " +
          "not supported: " + dataType.toString)
      }
    }

    private def asArray[R](dt: DataType, output: KernelArg): R = {
      (dt match {
        case DataType.i8 => output.asInstanceOf[ByteArg].asByteArray()
        case DataType.i16 => output.asInstanceOf[ShortArg].asShortArray()
        case DataType.i32 | DataType.int => output.asInstanceOf[IntArg].asIntArray()
        case DataType.i64 => output.asInstanceOf[LongArg].asLongArray()
        case DataType.f16 => output.asInstanceOf[HalfArg].asFloatArray()
        case DataType.f32 => output.asInstanceOf[FloatArg].asFloatArray()
        case DataType.f64 => output.asInstanceOf[DoubleArg].asDoubleArray()
        case _ => throw new IllegalArgumentException("Return type of the given lambda expression " +
          "not supported: " + dt.toString)
      }).asInstanceOf[R]
    }

    @tailrec
    private def createInputArg(arg: Any, dt: DataType): KernelArg = {
      arg match {
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
          if (dt == DataType.f32)
            createValueArg(f)
          else
            createValueArgHalf(f)

        case af: Array[Float] =>
          dt match {
            case arrayType: ArrayType if arrayType.elemType == DataType.f32 => createArrayArg(af)
            case _ => createArrayArgHalf(af)
          }

        case af: Array[Array[Float]] =>
          createInputArg(af.flatten, dt.asInstanceOf[ArrayType].elemType)

        case af: Array[Array[Array[Float]]] =>
          createInputArg(af.flatten, dt.asInstanceOf[ArrayType].elemType)

        case af: Array[Array[Array[Array[Float]]]] =>
          createInputArg(af.flatten, dt.asInstanceOf[ArrayType].elemType)

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
      }
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

    /**
      * A helper class to group together the various bits of related information that make up a parameter
      * @param parameter The OpenCL parameter as appearing in the generated kernel
      * @param argValue For non-intermediate input parameters, this carries the scala value to pass in the executor
      */
    private case class Argument(parameter: (ParamDecl, ParamKind), argValue: Option[Any])

    private def constructArguments(inputs: Seq[((ParamDecl, ParamKind), Any)],
                                   intermediateParams: Seq[(ParamDecl, ParamKind)],
                                   oclParams: Seq[ParamDecl]): List[Argument] = {
      // For each input ...
      inputs.headOption match {
        // ... if we have no more, we look at the intermediates ...
        case None => intermediateParams.headOption match {
          // ... if we have no more, we should also be out of ParamDecls
          case None =>
            assert(oclParams.isEmpty)
            Nil
          case Some(param) =>
            // We must have an OpenCl parameter
            assert(oclParams.nonEmpty, "Not enough opencl parameters")
            Argument((oclParams.head, param._2), None) ::
              constructArguments(inputs, intermediateParams.tail, oclParams.tail)
        }
        case Some((param, arg)) =>
          // We must have an OpenCl parameter
          assert(oclParams.nonEmpty, "Not enough opencl parameters")
          Argument((oclParams.head, param._2), Some(arg)) ::
            constructArguments(inputs.tail, intermediateParams, oclParams.tail)
      }
    }

    /**
      * Iterates through all the input arguments, collecting the values of all int-typed input parameters, which may
      * appear in the sizes of the other arguments.
      */
    private def collectSizeVars(arguments: List[Argument], sizeVariables: Map[Nat, Nat]): Map[Nat, Nat] = {
      def recordSizeVariable(sizeVariables:Map[Nat, Nat], arg:Argument): Map[Nat, Nat] = {
        arg.parameter._2.typ match {
          case DataType.int =>
            arg.argValue match {
              case Some(i:Int) => sizeVariables + ((NatIdentifier(arg.parameter._1.name), Cst(i)))
              case Some(num) =>
                throw new Exception(s"Int value for kernel argument ${arg.parameter._1.name} expected " +
                  s"but $num (of type ${num.getClass.getName} found")
              case None =>
                throw new Exception("Int kernel parameter needs a value")
            }
          case _ => sizeVariables
        }
      }

      arguments.foldLeft(sizeVariables)(recordSizeVariable)
    }

    /**
      * From the dpia paramters and the scala arguments, returns a Map of identifiers to sizes for "size vars",
      * the output kernel argument, and all the input kernel arguments
      */
    protected def createKernelArgs(arguments: Seq[Argument],
                                   sizeVariables: Map[Nat, Nat]): (KernelArg, List[KernelArg]) = {
      println(sizeVariables)

      //Now create the output kernel arg
      println(s"Create output argument")
      val kernelOutput = createOutputKernelArg(sizeVariables)

      //Finally, generate the input kernel args
      println(s"Create input arguments")
      val kernelArguments = createInputKernelArgs(arguments, sizeVariables)

      (kernelOutput, kernelArguments)
    }

    private def createInputKernelArgs(arguments: Seq[Argument], sizeVariables: Map[Nat, Nat]):List[KernelArg] = {

      //Helper for the creation of intermdiate arguments
      def createIntermediateArg(arg: Argument, sizeVariables: Map[Nat, Nat]): KernelArg = {
        //Try to substitue away all the free variables
        val rawSize = sizeInElements(arg.parameter._2.typ).value
        val cleanSize = ArithExpr.substitute(rawSize, sizeVariables)
        Try(cleanSize.evalInt) match {
          case Success(actualSize) =>
            arg.parameter._1.t match {
              case OpenCL.AST.PointerType(a, _, _) => a match {
                case shine.cuda.AddressSpace.Private => throw new Exception("'Private memory' is an invalid memory for parameter")
                case shine.cuda.AddressSpace.Local => ???
                case shine.cuda.AddressSpace.Global => createOutputArg(actualSize, getOutputType(arg.parameter._2.typ))
                case shine.cuda.AddressSpace.Constant => ???
                case AddressSpaceIdentifier(_) => throw new Exception("This shouldn't happen")
              }
              case _ => throw new Exception("This shouldn't happen")
            }
          case Failure(_) => throw new Exception(s"Could not evaluate $cleanSize")
        }
      }

      arguments match {
        case Nil => Nil
        case arg :: remainingArgs =>
          val kernelArg = arg.argValue match {
            //We have a scala value - this is an input argument
            case Some(scalaValue) => createInputArg(scalaValue, arg.parameter._2.typ)
            //No scala value - this is an intermediate argument
            case None => createIntermediateArg(arg, sizeVariables)
          }
          kernelArg::createInputKernelArgs(remainingArgs, sizeVariables)
      }
    }

    private def createOutputKernelArg(sizeVariables: Map[Nat, Nat]): KernelArg = {
      val rawSize = sizeInElements(this.outputParam._2.typ).value
      val cleanSize = ArithExpr.substitute(rawSize, sizeVariables)
      Try(cleanSize.evalInt) match {
        case Success(actualSize) => createOutputArg(actualSize, getOutputType(this.outputParam._2.typ))
        case Failure(_) => throw new Exception(s"Could not evaluate $cleanSize")
      }
    }

    protected def flattenToArrayOfInts(a: Array[(Int, Float)]): Array[Int] = {
      a.flatMap{ case (x,y) => Iterable(x, java.lang.Float.floatToIntBits(y)) }
    }

    private def getOutputType(dt: DataType): DataType = dt match {
      case _: ScalarType => dt
      case _: IndexType => DataType.int
      case _: DataTypeIdentifier => dt
      case VectorType(_, elem) => elem
      case PairType(fst, snd) =>
        val fstO = getOutputType(fst)
        val sndO = getOutputType(snd)
        if (fstO != sndO) {
          throw new IllegalArgumentException("no supported output type " +
            s"for heterogeneous pair: ${dt}")
        }
        fstO
      case ArrayType(_, elemType) => getOutputType(elemType)
      case DepArrayType(_, NatToDataLambda(_, elemType)) =>
        getOutputType(elemType)
      case _ =>
        throw new Exception("This should not happen")
    }

    private def sizeInElements(dt: DataType): SizeInElements = dt match {
      case v: VectorType =>  sizeInElements(v.elemType) * v.size
      case r: PairType => sizeInElements(r.dt1) + sizeInElements(r.dt2)
      case a: ArrayType => sizeInElements(a.elemType) * a.size
      case a: DepArrayType => ???
      case _ => SizeInElements(1)
    }

    case class SizeInElements(value: Nat) {
      def *(rhs: Nat): SizeInElements = SizeInElements(value * rhs)
      def +(rhs: SizeInElements): SizeInElements = SizeInElements(value + rhs.value)

      override def toString = s"$value elements"
    }
  }
}
