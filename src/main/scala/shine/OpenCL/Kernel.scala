package shine.OpenCL

import arithexpr.arithmetic._
import opencl.executor._
import shine.C.AST.ParamDecl
import shine.C.SizeInByte
import shine.DPIA.Phrases.Identifier
import shine.DPIA.Types._
import shine.DPIA._
import shine.OpenCL.Kernel.PREAMBLE
import shine.{C, OpenCL}
import util.{Time, TimeSpan}

import java.io.ByteArrayOutputStream
import java.nio.ByteBuffer
import scala.collection.immutable.List
import scala.collection.Seq
import scala.language.implicitConversions
import scala.util.{Failure, Success, Try}

//noinspection ScalaDocParserErrorInspection
case class Kernel(decls: Seq[C.AST.Decl],
                  kernel: OpenCL.AST.KernelDecl,
                  outputParam: Identifier[AccType],
                  inputParams: Seq[Identifier[ExpType]],
                  intermediateParams: Seq[Identifier[VarType]],
                  fallbackOutputSize:Option[SizeInByte],
                  manualIntermediateBufferSize: scala.collection.immutable.Map[Int, SizeInByte]) {

  def code: String = {
    val sb = new StringBuilder
    sb ++= Kernel.PREAMBLE
    sb ++= "\n\n"
    sb ++= decls.map(OpenCL.AST.Printer(_)).mkString("\n")
    sb ++= "\n\n"
    sb ++= OpenCL.AST.Printer(kernel)
    sb.toString()
  }

  /** This method will return a Scala function which executed the kernel via OpenCL and returns its
  // result and the time it took to execute the kernel.
  //
  // A type annotation `F` has to be provided which specifies the type of the Scala function.
  // The following syntax is used for this (which implementation can be found in
  // OpenCL/Types.scala):
  //
  // <kernel>.as[ScalaFunction `(` <Arg1Type> `,` <Arg2Type> `,` <...> `)=>` <ReturnType> ]
  //
  // where all text in < > is to be replaced with the appropriate Scala terms.
  // An example for the dot product which takes two arrays of floats and returns an array of float
  // would be:
  //
  // dotKernel.as[ScalaFunction `(` Array[Float] `,` Array[Float] `)=>` Array[Float]]

    NB: If the kernel takes a single argument, then the invocation must use this syntax
    (Example with a kernel of type Array[Float] => Array[Float]

    val kernelF = kernel.as[ScalaFunction`(`Array[Float]`)=>`Array[Float]]
    val (result, time) = kernelF(xs `;`)
    */
  def as[F <: FunctionHelper](localSize: LocalSize, globalSize: GlobalSize)
                             (implicit ev: F#T <:< HList): F#T => (F#R, TimeSpan[Time.ms]) = {
    hArgs: F#T => {
      val args: List[Any] = hArgs.toList
      assert(inputParams.length == args.length)

      //Match up corresponding dpia parameter/intermediate parameter/scala argument (when present) in a covenience
      //structure
      val arguments = constructArguments(inputParams zip args, intermediateParams, this.kernel.params.tail)

      //First, we want to find all the parameter mappings
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

      val (outputArg, inputArgs) = createKernelArgs(arguments, sizeVarMapping)
      val kernelArgs = outputArg :: inputArgs

      val codeToSend = this.code
      val kernelJNI = opencl.executor.Kernel.create(codeToSend, kernel.name, "")

      List(localSize match {
        case LocalSize(x) if !x.isEvaluable => Some(s"OpenCL local size is not evaluable (currently set to $x)")
        case _ => None
      }, globalSize match {
        case GlobalSize(x) if !x.isEvaluable => Some(s"OpenCL local size is not evaluable (currently set to $x)")
        case _ => None
      }).filter(_.isDefined).map(_.get) match {
        case Nil =>
        case problems =>
          val errorMessage = "Cannot run kernel:\n" ++ problems.reduce(_ ++ _)
          throw new Exception(errorMessage)
      }

      val runtime = Executor.execute(kernelJNI,
        ArithExpr.substitute(localSize.size.x, sizeVarMapping).eval,
        ArithExpr.substitute(localSize.size.y, sizeVarMapping).eval,
        ArithExpr.substitute(localSize.size.z, sizeVarMapping).eval,
        ArithExpr.substitute(globalSize.size.x, sizeVarMapping).eval,
        ArithExpr.substitute(globalSize.size.y, sizeVarMapping).eval,
        ArithExpr.substitute(globalSize.size.z, sizeVarMapping).eval,
        kernelArgs.toArray
        )

      val output = castToOutputType[F#R](outputParam.`type`.dataType, outputArg)

      kernelArgs.foreach(_.dispose)
      kernelJNI.dispose()

      (output, TimeSpan.inMilliseconds(runtime))
    }
  }

  /**
    * A helper class to group together the various bits of related information that make up a parameter
    * @param identifier The identifier representing the parameter in the dpia source
    * @param parameter The OpenCL parameter as appearing in the generated kernel
    * @param argValue For non-intermediate input parameters, this carries the scala value to pass in the executor
    */
  private case class Argument(identifier: Identifier[ExpType], parameter: ParamDecl, argValue: Option[Any],
                              sizeOverride: Option[SizeInByte])

  private def constructArguments(inputs: Seq[(Identifier[ExpType], Any)],
                                 intermediateParams: Seq[Identifier[VarType]],
                                 oclParams: Seq[ParamDecl],
                                 intermediateParameterIdx: Int = 0 // This tracks the index of the intermediate parameter we
                                //will see next. It's 0 at first, and it stays so until all input parameters are parsed.
                                // It is used to recover the manual size override.
                                ): List[Argument] = {
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
            val overrideSize = this.manualIntermediateBufferSize.get(intermediateParameterIdx)

            Argument(Identifier(param.name, param.t.t1), oclParams.head, None, overrideSize) ::
              constructArguments(inputs, intermediateParams.tail, oclParams.tail, intermediateParameterIdx + 1)
        }
        case Some((param, arg)) =>
          // We must have an OpenCl parameter
          assert(oclParams.nonEmpty, "Not enough opencl parameters")
          Argument(param, oclParams.head, Some(arg), sizeOverride = None) :: constructArguments(inputs.tail, intermediateParams, oclParams.tail)
      }
  }

  /**
    * From the dpia paramters and the scala arguments, returns a Map of identifiers to sizes for "size vars",
    * the output kernel argument, and all the input kernel arguments
   */
  private def createKernelArgs(arguments: Seq[Argument],
                               sizeVariables: Map[Nat, Nat]): (GlobalArg, List[KernelArg]) = {
    //Now generate the input kernel args
    println(s"Create input arguments")
    val kernelArguments = createInputKernelArgs(arguments, sizeVariables)
    //Finally, create the output
    println(s"Create output argument")
    val kernelOutput = createOutputKernelArg(sizeVariables)

    (kernelOutput, kernelArguments)
  }

  /**
   * Iterates through all the input arguments, collecting the values of all int-typed input parameters, which may appear
    * in the sizes of the other arguments.
   */
  private def collectSizeVars(arguments: List[Argument], sizeVariables: Map[Nat, Nat]): Map[Nat, Nat] = {
    def recordSizeVariable(sizeVariables:Map[Nat, Nat], arg:Argument) = {
      arg.identifier.t match {
        case ExpType(shine.DPIA.Types.int, read) =>
          arg.argValue match {
            case Some(i:Int) => sizeVariables + ((NatIdentifier(arg.identifier.name), Cst(i)))
            case Some(num) =>
              throw new Exception(s"Int value for kernel argument ${arg.identifier.name} expected but $num (of type ${num.getClass.getName} found")
            case None =>
              throw new Exception("Int kernel parameter needs a value")
          }
        case _ => sizeVariables
      }
    }

    arguments.foldLeft(sizeVariables)(recordSizeVariable)
  }

  private def createLocalArg(sizeInByte: Long): LocalArg = {
    println(s"Allocated local argument with $sizeInByte bytes")
    LocalArg.create(sizeInByte)
  }

  private def createGlobalArg(sizeInByte: Long): GlobalArg = {
    println(s"Allocated global argument with $sizeInByte bytes")
    GlobalArg.createOutput(sizeInByte)
  }

  private def createGlobalArg(array: Array[Float]): GlobalArg = {
    println(s"Allocated global argument with ${array.length * 4} bytes")
    GlobalArg.createInput(array)
  }

  private def createGlobalArg(array: Array[Int]): GlobalArg = {
    println(s"Allocated global argument with ${array.length * 4} bytes")
    GlobalArg.createInput(array)
  }

  private def createGlobalArg(array: Array[Double]): GlobalArg = {
    println(s"Allocated global argument with ${array.length * 8} bytes")
    GlobalArg.createInput(array)
  }

  private def createValueArg(value: Float): ValueArg = {
    println(s"Allocated value argument with 4 bytes")
    ValueArg.create(value)
  }

  private def createValueArg(value: Int): ValueArg = {
    println(s"Allocated value argument with 4 bytes")
    ValueArg.create(value)
  }

  private def createValueArg(value: Double): ValueArg = {
    println(s"Allocated value argument with 8 bytes")
    ValueArg.create(value)
  }

  /**
    * Generates the input kernel args
    * @param arguments
    * @param sizeVariables
    * @return
    */
  private def createInputKernelArgs(arguments: Seq[Argument], sizeVariables: Map[Nat, Nat]):List[KernelArg] = {

    //Helper for the creation of intermdiate arguments
    def createIntermediateArg(arg: Argument, sizeVariables: Map[Nat, Nat]): KernelArg = {
      val actualSize = arg.sizeOverride match {
        case Some(bytes) => bytes.value.eval
        case None =>
          //Get the size of bytes, potentially with free variables
          val rawSize = SizeInByte(arg.identifier.t.dataType)
          //Try to substitue away all the free variables
          val cleanSize = ArithExpr.substitute(rawSize.value, sizeVariables)
          Try(cleanSize.evalLong) match {
            case Success(actualSize) => actualSize
            case Failure(_) => throw new Exception(s"Could not evaluate $cleanSize")
          }
      }
      arg.parameter.t match {
        case OpenCL.AST.PointerType(a, _, _) => a match {
          case AddressSpace.Private => throw new Exception ("'Private memory' is an invalid memory for opencl parameter")
          case AddressSpace.Local => createLocalArg(actualSize)
          case AddressSpace.Global =>  createGlobalArg(actualSize)
          case AddressSpace.Constant => ???
          case AddressSpaceIdentifier(_) => throw new Exception ("This shouldn't happen")
        }
        case _ => throw new Exception ("This shouldn't happen")
      }
    }

    arguments match {
      case Nil => Nil
      case arg :: remainingArgs =>
        val kernelArg = arg.argValue match {
            //We have a scala value - this is an input argument
          case Some(scalaValue) => createInputArgFromScalaValue(scalaValue)
            //No scala value - this is an intermediate argument
          case None => createIntermediateArg(arg, sizeVariables)
        }
        kernelArg::createInputKernelArgs(remainingArgs, sizeVariables)
    }
  }

  def withFallbackOutputSize(sizeInByte: SizeInByte): Kernel =
    this.copy(
      fallbackOutputSize = Some(sizeInByte)
    )
  def setIntermediateBufferSize(idx: Int, sizeInByte: SizeInByte): Kernel =
    this.copy(
      manualIntermediateBufferSize = manualIntermediateBufferSize + (idx -> sizeInByte)
    )

  private def createOutputKernelArg(sizeVariables:Map[Nat,Nat]):GlobalArg = {
    val rawSize = SizeInByte(this.outputParam.t.dataType).value

    val cleanSize = ArithExpr.substitute(rawSize, sizeVariables)
    if (cleanSize.isEvaluable) {
      createGlobalArg(cleanSize.evalLong)
    } else {
      this.fallbackOutputSize match {
        case Some(value) => createGlobalArg(value.value.evalLong)
        case None => throw new Exception(s"Could not evaluate $cleanSize")
      }
    }
  }

  private def createInputArgFromScalaValue(arg: Any): KernelArg = {
    arg match {
      case  f: Float => createValueArg(f)
      case af: Array[Float] => createGlobalArg(af)
      case af: Array[Array[Float]] => createGlobalArg(af.flatten)
      case af: Array[Array[Array[Float]]] => createGlobalArg(af.flatten.flatten)
      case af: Array[Array[Array[Array[Float]]]] => createGlobalArg(af.flatten.flatten.flatten)

      case  i: Int => createValueArg(i)
      case ai: Array[Int] => createGlobalArg(ai)
      case ai: Array[Array[Int]] => createGlobalArg(ai.flatten)
      case ai: Array[Array[Array[Int]]] => createGlobalArg(ai.flatten.flatten)
      case ai: Array[Array[Array[Array[Int]]]] => createGlobalArg(ai.flatten.flatten.flatten)

      case  d: Double => createValueArg(d)
      case ad: Array[Double] => createGlobalArg(ad)
      case ad: Array[Array[Double]] => createGlobalArg(ad.flatten)
      case ad: Array[Array[Array[Double]]] => createGlobalArg(ad.flatten.flatten)
      case ad: Array[Array[Array[Array[Double]]]] => createGlobalArg(ad.flatten.flatten.flatten)

      case p: Array[(_, _)] => p.head match {
          case (_: Int, _: Float) =>
            GlobalArg.createInput(flattenToArrayOfInts(p.asInstanceOf[Array[(Int, Float)]]))
          case (_: Float, _: Float) =>
            GlobalArg.createInput(p.asInstanceOf[Array[(Float, Float)]].iterator.flatten(x => Iterator(x._1, x._2)).toArray)
          case (_:Int, _: Int) =>
            GlobalArg.createInput(p.asInstanceOf[Array[(Int, Int)]].iterator.flatten(x => Iterator(x._1, x._2)).toArray)
          case _ => ???
        }
      case pp: Array[Array[(_, _)]] => pp.find(_.nonEmpty).get.head match {
        case (_: Int, _: Float) =>
          val flattened = pp.flatMap(a => flattenToArrayOfInts(a.asInstanceOf[Array[(Int, Float)]]))
          createGlobalArg(flattened)
        case _ => ???
      }

      case dp: KernelScalaInterop.Puttable[_] =>
        val size = dp.sizeInByte
        val byteBuffer = ByteBuffer.allocate(size)
        dp.put(byteBuffer)
        byteBuffer.flip()
        val intBuffer = byteBuffer.asIntBuffer()
        val array = new Array[Int](intBuffer.remaining())
        intBuffer.get(array)
        createGlobalArg(array)

      case _ => throw new IllegalArgumentException("Kernel argument is of unsupported type: " +
        arg.getClass.getName)
    }
  }

  private def flattenToArrayOfInts(a: Array[(Int, Float)]): Array[Int] = {
    val result = a.flatMap{ case (x,y) => Iterable(x, java.lang.Float.floatToIntBits(y)) }

    result
  }

  private def castToOutputType[R](dt: DataType, output: GlobalArg): R = {
    dt match {
      case _:ArrayType | _:DepArrayType =>
        (getOutputType(dt) match {
          case shine.DPIA.Types.int => output.asIntArray()
          case shine.DPIA.Types.f32 => output.asFloatArray()
          case shine.DPIA.Types.f64 => output.asDoubleArray()
          case shine.DPIA.Types.PairType(e1, e2)
            if e1 == shine.DPIA.Types.f32 && e2 == e1 =>
            val flat = output.asFloatArray()
            assert(flat.length % 2 == 0)
            Array.tabulate(flat.length/2)(i => (flat(i), flat(i + 1)))
          case shine.DPIA.Types.DepPairType(n, et) => n match {
            case n:NatIdentifier => output.asIntArray()
            case _ => ???
          }
          case _ => throw new IllegalArgumentException("Return type of the given lambda expression " +
            "not supported: " + dt.toString)
        }).asInstanceOf[R]
      case pair: DepPairType[_] =>
        // Bit-frobbing in scala.Yay!
        val rawData = output.asIntArray()
        val bytes = ByteBuffer.allocate(rawData.length * 4)
        rawData.foreach(x => bytes.putInt(x))
        bytes.position(0)
        // We gotta read the data out. First, we are going to have an int or many ints - depending on the type
        // of the dep pair fst

        val (fst, readSoFar):(Any, Int) = pair.x match {
          case _:Nat => (bytes.getInt(), 1)
          case _:NatCollection =>
            val len = bytes.getInt()
            val data = Array.tabulate(len)(_ => {
              bytes.getInt()
            })
            (data, len + 1)
        }
        // The rest of the data is the second element...here we dont have much choice of types, only simple
        // cases for now
        val snd: Any = getOutputType(pair.elemT) match {
          case shine.DPIA.Types.int =>
            val len = rawData.length - readSoFar
            val data = Array.tabulate(len)(_ => {
              bytes.getInt()
            })
            data
          case shine.DPIA.Types.f32 =>
            val len = rawData.length - readSoFar
            val data = Array.tabulate(len)(_ => {
              bytes.getFloat()
            })
            data
          case _ => ???
        }

        (fst, snd).asInstanceOf[R]
      case _ => ???
    }
  }

  private def getOutputType(dt: DataType): DataType = dt match {
    case _: ScalarType => dt
    case _: IndexType => int
    case _: DataTypeIdentifier => dt
    case VectorType(_, elem) => elem
    case PairType(fst, snd) =>
      val fstO = getOutputType(fst)
      val sndO = getOutputType(snd)
      if (fstO != sndO) {
        val szFst = SizeInByte(fst)
        val szSnd = SizeInByte(snd)
        if (szFst.value == szSnd.value) {
          szFst.value.eval match {
            case 4 => i32
            case _ => throw new IllegalArgumentException(s"No supported heterogeneous pair type ${dt}: elements have size ${szFst}")
          }
        } else throw new IllegalArgumentException("no supported output type " +
          s"for heterogeneous pair: ${dt}")
      }
      fstO
    case ArrayType(_, elemType) => getOutputType(elemType)
    case DepArrayType(_, NatToDataLambda(_, elemType)) =>
      getOutputType(elemType)
    case DepArrayType(_, _) | _: NatToDataApply =>
      throw new Exception("This should not happen")
    case x:DepPairType[_] => x
  }
}

sealed case class KernelWithSizes(kernel: Kernel,
                                  localSize: LocalSize,
                                  globalSize: GlobalSize) {
  def as[F <: FunctionHelper](implicit ev: F#T <:< HList): F#T => (F#R, TimeSpan[Time.ms]) =
    kernel.as[F](localSize, globalSize)

  def code: String= kernel.code
}

sealed case class KernelNoSizes(kernel: Kernel) {
  //noinspection TypeAnnotation
  def as[F <: FunctionHelper](implicit ev: F#T <:< HList) = new {
    def apply(localSize: LocalSize, globalSize: GlobalSize): F#T => (F#R, TimeSpan[Time.ms]) =
      kernel.as[F](localSize, globalSize)

    def withSizes(localSize: LocalSize, globalSize: GlobalSize): F#T => (F#R, TimeSpan[Time.ms]) =
      kernel.as[F](localSize, globalSize)
  }

  def code: String= kernel.code
}

object Kernel {
  implicit def forgetSizes(k: KernelWithSizes): KernelNoSizes =
    KernelNoSizes(k.kernel)

  val PREAMBLE:String =
    """
      |#define uint8_t uchar
      |#define uint32_t uint
      |""".stripMargin
}

object KernelScalaInterop {
  final case class DependentPair[T](fst: Array[Int], data:T)
  final case class Puttable[T:Put](x:T) {
    def put(bb: ByteBuffer): Unit = {
      implicitly[Put[T]].write(this.x, bb)
    }
    def sizeInByte: Int = implicitly[Put[T]].sizeInByte(this.x)
  }

  trait Put[T] {
    def write(x:T, b: ByteBuffer): Unit
    def sizeInByte(x: T): Int
  }

  implicit val writeInt = new Put[Int] {
    override def write(x: Int, b: ByteBuffer): Unit = b.putInt(x)

    override def sizeInByte(x: Int): Int = 4
  }

  implicit val writeFloat = new Put[Float] {
    override def write(x: Float, b: ByteBuffer): Unit = b.putFloat(x)

    override def sizeInByte(x: Float): Int = 4
  }

  implicit def putPair[A: Put, B: Put] = new Put[(A,B)] {
    override def write(x: (A, B), b: ByteBuffer): Unit = {
      implicitly[Put[A]].write(x._1, b)
      implicitly[Put[B]].write(x._2, b)
    }

    override def sizeInByte(x: (A, B)): Int =
      implicitly[Put[A]].sizeInByte(x._1) + implicitly[Put[B]].sizeInByte(x._2)
  }

  implicit def putArray[A: Put] = new Put[Array[A]] {
    override def write(x: Array[A], b: ByteBuffer): Unit =
      x.foreach(implicitly[Put[A]].write(_, b))

    override def sizeInByte(x: Array[A]): Int = {
      var total = 0
      for {
        item <- x
      } {
        total += implicitly[Put[A]].sizeInByte(item)
      }
      total
    }
  }

  implicit def putDepPair[T: Put] = new Put[DependentPair[T]]{
    def alignTo(x: Int, align: Int): Int = {
      val diff =  if(x % align == 0) 0 else align - (x % align)
      val total = x + diff
      assert(total % align == 0)
      total
    }

    override def write(x: DependentPair[T], b: ByteBuffer): Unit = {
      val headArray = Array.fill(64)(0)
      headArray(0) = alignTo(x.fst.length, 64)
      implicitly[Put[Array[Int]]].write(headArray, b)
      implicitly[Put[Array[Int]]].write(x.fst, b)
      val pad = headArray(0) - x.fst.length
      implicitly[Put[Array[Int]]].write(Array.fill(pad)(0), b)
      println(b.position())
      implicitly[Put[T]].write(x.data, b)
    }

    override def sizeInByte(x: DependentPair[T]): Int = {
      val dataSize = implicitly[Put[T]].sizeInByte(x.data)
      64*4 + alignTo(x.fst.length, 64)*4 + dataSize
    }
  }
}
