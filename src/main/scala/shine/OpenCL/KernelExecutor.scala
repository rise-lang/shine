package shine.OpenCL

import arithexpr.arithmetic._
import opencl.executor.{Kernel => _, _}
import rise.core.types.DataType._
import rise.core.types.{Nat => _, _}
import shine.C.AST.{ParamDecl, ParamKind}
import shine.DPIA._
import shine.OpenCL
import shine.OpenCL.AST.Kernel
import util.Time.ms
import util.{Time, TimeSpan, gen}

import scala.collection.Seq
import scala.collection.immutable.List
import scala.language.implicitConversions
import scala.util.{Failure, Success, Try}

object KernelExecutor {

  sealed case class KernelWithSizes(ktu: KernelModule,
                                    localSize: LocalSize,
                                    globalSize: GlobalSize) {
    def as[T, R](implicit ev: T <:< HList): T => (R, TimeSpan[Time.ms]) = {
      FromKernelModule(ktu).as[T, R](localSize, globalSize)
    }

    def code: String = util.gen.opencl.kernel.asString(ktu)

    def forgetSizes: KernelNoSizes = KernelNoSizes(ktu)
  }

  sealed case class KernelNoSizes(ktu: KernelModule) {
    def as[T, R](implicit ev: T <:< HList): AS[T, R] = AS()

    case class AS[T, R]()(implicit ev: T <:< HList) {
      def apply(localSize: LocalSize, globalSize: GlobalSize): T => (R, TimeSpan[Time.ms]) = {
        FromKernelModule(ktu).as[T, R](localSize, globalSize)
      }

      def withSizes(localSize: LocalSize, globalSize: GlobalSize): T => (R, TimeSpan[Time.ms]) = {
        FromKernelModule(ktu).as[T, R](localSize, globalSize)
      }
    }

    def code: String = util.gen.opencl.kernel.asString(ktu)
  }

  object KernelNoSizes {
    implicit def fromKernelModule(ktu: KernelModule): KernelNoSizes =
      KernelNoSizes(ktu)
  }

  case class FromKernelModule(ktu: KernelModule) {

    assert(ktu.kernels.size == 1)

    val kernel: Kernel = ktu.kernels.head
    val code: String = gen.opencl.kernel.asString(ktu)
    val outputParam: (ParamDecl, ParamKind) = kernel.outputParams.head

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

        val (outputArg, inputArgs) = createKernelArgs(arguments, outputParam, sizeVarMapping)
        val kernelArgs = outputArg :: inputArgs

        val kernelJNI = opencl.executor.Kernel.create(code, kernel.name, "")

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

        val output = castToOutputType[R](outputParam._2.typ, outputArg)

        kernelArgs.foreach(_.dispose)
        kernelJNI.dispose()

        (output, TimeSpan.inMilliseconds(runtime))
      }
    }
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
    * From the dpia paramters and the scala arguments, returns a Map of identifiers to sizes for "size vars",
    * the output kernel argument, and all the input kernel arguments
    */
  private def createKernelArgs(arguments: Seq[Argument],
                               outputParam: (ParamDecl, ParamKind),
                               sizeVariables: Map[Nat, Nat]): (GlobalArg, List[KernelArg]) = {
    //Now generate the input kernel args
    println(s"Create input arguments")
    val kernelArguments = createInputKernelArgs(arguments, sizeVariables)
    //Finally, create the output
    println(s"Create output argument")
    val kernelOutput = createOutputKernelArg(outputParam, sizeVariables)

    (kernelOutput, kernelArguments)
  }

  /**
    * Iterates through all the input arguments, collecting the values of all int-typed input parameters, which may
    * appear in the sizes of the other arguments.
    */
  private def collectSizeVars(arguments: List[Argument], sizeVariables: Map[Nat, Nat]): Map[Nat, Nat] = {
    def recordSizeVariable(sizeVariables:Map[Nat, Nat], arg:Argument): Map[Nat, Nat] = {
      arg.parameter._2.typ match {
        case rise.core.types.DataType.int =>
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

  private def createInputKernelArgs(arguments: Seq[Argument], sizeVariables: Map[Nat, Nat]):List[KernelArg] = {

    //Helper for the creation of intermdiate arguments
    def createIntermediateArg(arg: Argument, sizeVariables: Map[Nat, Nat]): KernelArg = {
      //Get the size of bytes, potentially with free variables
      val rawSize = sizeInByte(arg.parameter._2.typ)
      //Try to substitue away all the free variables
      val cleanSize = ArithExpr.substitute(rawSize.value, sizeVariables)
      Try(cleanSize.evalLong) match {
        case Success(actualSize) =>
          arg.parameter._1.t match {
            case OpenCL.AST.PointerType(a, _, _) => a match {
              case AddressSpace.Private =>
                throw new Exception ("'Private memory' is an invalid memory for opencl parameter")
              case AddressSpace.Local => createLocalArg(actualSize)
              case AddressSpace.Global =>  createGlobalArg(actualSize)
              case AddressSpace.Constant => ???
              case AddressSpaceIdentifier(_) => throw new Exception ("This shouldn't happen")
            }
            case _ => throw new Exception ("This shouldn't happen")
          }
        case Failure(_) => throw new Exception(s"Could not evaluate $cleanSize")
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

  private def createOutputKernelArg(outputParam: (ParamDecl, ParamKind), sizeVariables:Map[Nat,Nat]):GlobalArg = {
    val rawSize = sizeInByte(outputParam._2.typ).value
    val cleanSize = ArithExpr.substitute(rawSize, sizeVariables)
    Try(cleanSize.evalLong) match {
      case Success(actualSize) => createGlobalArg(actualSize)
      case Failure(_) => throw new Exception(s"Could not evaluate $cleanSize")
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
        case _ => ???
      }
      case pp: Array[Array[(_, _)]] => pp.head.head match {
        case (_: Int, _: Float) =>
          GlobalArg.createInput(pp.flatMap(a => flattenToArrayOfInts(a.asInstanceOf[Array[(Int, Float)]])))
        case _ => ???
      }

      case _ => throw new IllegalArgumentException("Kernel argument is of unsupported type: " +
        arg.getClass.getName)
    }
  }

  private def flattenToArrayOfInts(a: Array[(Int, Float)]): Array[Int] = {
    a.flatMap{ case (x,y) => Iterable(x, java.lang.Float.floatToIntBits(y)) }
  }

  private def castToOutputType[R](dt: DataType, output: GlobalArg): R = {
    assert(dt.isInstanceOf[ArrayType] || dt.isInstanceOf[DepArrayType])
    (getOutputType(dt) match {
      case rise.core.types.DataType.int => output.asIntArray()
      case rise.core.types.DataType.f32 => output.asFloatArray()
      case rise.core.types.DataType.f64 => output.asDoubleArray()
      case _ => throw new IllegalArgumentException("Return type of the given lambda expression " +
        "not supported: " + dt.toString)
    }).asInstanceOf[R]
  }

  private def getOutputType(dt: DataType): DataType = dt match {
    case _: ScalarType => dt
    case _: IndexType => rise.core.types.DataType.int
    case NatType => rise.core.types.DataType.int
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
    case DepArrayType(_, _) | _: NatToDataApply => throw new Exception("This should not happen")
    case _: DepPairType[_, _] | _: ManagedBufferType | _: OpaqueType | _: FragmentType =>
      throw new Exception(s"${dt} not supported as output type")
  }

  private def sizeInByte(dt: DataType): SizeInByte = dt match {
    case s: ScalarType => s match {
      case rise.core.types.DataType.bool => SizeInByte(1)
      case rise.core.types.DataType.int => SizeInByte(4)
      case rise.core.types.DataType.u8 | rise.core.types.DataType.i8 =>
        SizeInByte(1)
      case rise.core.types.DataType.u16 | rise.core.types.DataType.i16 | rise.core.types.DataType.f16 =>
        SizeInByte(2)
      case rise.core.types.DataType.u32 | rise.core.types.DataType.i32 | rise.core.types.DataType.f32 =>
        SizeInByte(4)
      case rise.core.types.DataType.u64 | rise.core.types.DataType.i64 | rise.core.types.DataType.f64 =>
        SizeInByte(8)
    }
    case rise.core.types.DataType.NatType => SizeInByte(4)
    case _: IndexType => SizeInByte(4) // == sizeof(int)
    case v: VectorType => sizeInByte(v.elemType) * v.size
    case r: PairType => sizeInByte(r.dt1) + sizeInByte(r.dt2)
    case a: ArrayType => sizeInByte(a.elemType) * a.size
    case a: DepArrayType =>
      a.fdt match {
        case NatToDataLambda(x, body) =>
          SizeInByte(BigSum(Cst(0), a.size - 1, `for`=x, in=sizeInByte(body).value))
        case _: NatToDataIdentifier =>
          throw new Exception("This should not happen")
      }
    case _: DepPairType[_, _] | _: NatToDataApply | _: DataTypeIdentifier |
         _: ManagedBufferType | _: OpaqueType | _: FragmentType =>
      throw new Exception(s"the byte size of ${dt} should not be requested")
  }

  case class SizeInByte(value: Nat) {
    def *(rhs: Nat) = SizeInByte(value * rhs)
    def +(rhs: SizeInByte) = SizeInByte(value + rhs.value)

    override def toString = s"$value bytes"
  }
}
