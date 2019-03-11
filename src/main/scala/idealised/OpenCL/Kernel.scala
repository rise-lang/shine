package idealised.OpenCL

import idealised.OpenCL.AST.ParamDecl
import idealised.DPIA.Phrases.Identifier
import idealised.DPIA.Types._
import idealised.DPIA._
import idealised.{C, OpenCL}
import idealised.utils._
import lift.arithmetic._
import opencl.executor._

import scala.collection.immutable.List
import scala.collection.{Seq, immutable}
import scala.language.implicitConversions

//noinspection ScalaDocParserErrorInspection
case class Kernel(decls: Seq[C.AST.Decl],
                  kernel: OpenCL.AST.KernelDecl,
                  outputParam: Identifier[AccType],
                  inputParams: Seq[Identifier[ExpType]],
                  intermediateParams: Seq[Identifier[VarType]]) {

  def code: String = decls.map(OpenCL.AST.Printer(_)).mkString("\n") +
    "\n\n" +
    OpenCL.AST.Printer(kernel)

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
  def as[F <: FunctionHelper](localSize: NDRange, globalSize: NDRange)
                             (implicit ev: F#T <:< HList): F#T => (F#R, TimeSpan[Time.ms]) = {
    hArgs: F#T => {
      val args: List[Any] = hArgs.toList

      val (sizeVarMap, outputArg, inputArgs) = createKernelArgs(inputParams, args)
      val kernelArgs = outputArg::inputArgs

      val c = code
      val kernelJNI = opencl.executor.Kernel.create(c, kernel.name, "")

      assert(localSize.isEvaluable && globalSize.isEvaluable,
        "Local and Global Size must be evaluable and set before executing the kernel.")

      val lengthMapping = sizeVarMap.asInstanceOf[Map[Nat,Nat]]
      val runtime = Executor.execute(kernelJNI,
        ArithExpr.substitute(localSize.x, lengthMapping).eval,
        ArithExpr.substitute(localSize.y, lengthMapping).eval,
        ArithExpr.substitute(localSize.z, lengthMapping).eval,
        ArithExpr.substitute(globalSize.x, lengthMapping).eval,
        ArithExpr.substitute(globalSize.y, lengthMapping).eval,
        ArithExpr.substitute(globalSize.z, lengthMapping).eval,
        kernelArgs.toArray
        )

      val output = castToOutputType[F#R](outputParam.`type`.dataType, outputArg)

      kernelArgs.foreach(_.dispose)
      kernelJNI.dispose()

      (output, TimeSpan.inMilliseconds(runtime))
    }
  }

  private case class Argument(dpiaParameter:Identifier[ExpType], openCLParameter:ParamDecl, scalaValue:Option[Any])

  private def constructArguments(params:Seq[Identifier[ExpType]], scalaArgs:List[Any], oclParams:Seq[ParamDecl]):List[Argument] = {
      params.headOption match {
        case None =>
          assert(scalaArgs.isEmpty && oclParams.isEmpty)
          Nil
        case Some(param) =>
          assert(oclParams.nonEmpty, "Not enough opencl parameters")
          Argument(param, oclParams.head, scalaArgs.headOption) :: constructArguments(params.tail, scalaArgs.tail, oclParams.tail)
      }
  }

  private def createKernelArgs(params:Seq[Identifier[ExpType]], args:List[Any]):(Map[NatIdentifier, Nat], GlobalArg, List[KernelArg]) = {
    //Match up corresponding dpia parameter/intermediate parameter/scala argument (when present) in a covenience
    //structure
    val arguments = constructArguments(params, args, this.kernel.params.tail)
    //First, we want to find all the parameter mappings
    val sizeVarMapping = collectSizeVars(arguments, Map())
    //Now generate the input kernel args
    val kernelArguments = createInputKernelArgs(arguments, sizeVarMapping)
    //Finally, create the output
    val kernelOutput = createOutputKernelArg(sizeVarMapping)

    (sizeVarMapping, kernelOutput, kernelArguments)
  }

  private def collectSizeVars(arguments:List[Argument], sizeVariables:Map[NatIdentifier, Nat]):Map[NatIdentifier, Nat] = {
    def recordSizeVariable( sizeVariables:Map[NatIdentifier, Nat], arg:Argument) = {
      arg.dpiaParameter.t match {
        case ExpType(idealised.DPIA.Types.int) =>
          arg.scalaValue match {
            case Some(i:Int) => sizeVariables + ((NamedVar(arg.dpiaParameter.name), Cst(i)))
            case Some(num) =>
              throw new Exception(s"Int value for kernel argument ${arg.dpiaParameter.name} expected but $num (of type ${num.getClass.getName} found")
            case None =>
              throw new Exception("Int kernel parameter needs a value")
          }
        case _ => sizeVariables
      }
    }

    arguments.foldLeft(Map[NatIdentifier, Nat]())(recordSizeVariable)
  }

  private def createInputKernelArgs(arguments:List[Argument], sizeVariables:Map[NatIdentifier, Nat]):List[KernelArg] = {

    def createIntermediateArg(arg:Argument, sizeVariables:Map[NatIdentifier, Nat]):KernelArg = {
      val rawSize = sizeInByte(arg.dpiaParameter.t.dataType)
      val cleanSize = ArithExpr.substitute(rawSize.value, sizeVariables.asInstanceOf[Map[Nat, Nat]])
      val actualSize = cleanSize.evalLong
      arg.openCLParameter.addressSpace match {
        case PrivateMemory => throw new Exception("'Private memory' is an invalid memory for opencl parameter")
        case LocalMemory => LocalArg.create(actualSize)
        case GlobalMemory => GlobalArg.createOutput(actualSize)
      }
    }

    arguments match {
      case Nil => Nil
      case arg::remainingArgs =>
        val kernelArg = arg.scalaValue match {
            //We have the scala value - this is an input argument
          case Some(scalaValue) => createInputArgFromScalaValue(scalaValue)
          case None => createIntermediateArg(arg, sizeVariables)
        }
        kernelArg::createInputKernelArgs(remainingArgs, sizeVariables)
    }
  }

  private def createOutputKernelArg(sizeVariables:Map[NatIdentifier,Nat]):GlobalArg = {
    val rawSize = sizeInByte(this.outputParam.t.dataType).value
    val cleanSize = ArithExpr.substitute(rawSize, sizeVariables.asInstanceOf[Map[Nat,Nat]])
    val actualSize = cleanSize.evalLong
    GlobalArg.createOutput(actualSize)
  }

  private def createInputArgFromScalaValue(arg: Any): KernelArg = {
    arg match {
      case  f: Float => ValueArg.create(f)
      case af: Array[Float] => GlobalArg.createInput(af)
      case af: Array[Array[Float]] => GlobalArg.createInput(af.flatten)
      case af: Array[Array[Array[Float]]] => GlobalArg.createInput(af.flatten.flatten)
      case af: Array[Array[Array[Array[Float]]]] => GlobalArg.createInput(af.flatten.flatten.flatten)

      case  i: Int => ValueArg.create(i)
      case ai: Array[Int] => GlobalArg.createInput(ai)
      case ai: Array[Array[Int]] => GlobalArg.createInput(ai.flatten)
      case ai: Array[Array[Array[Int]]] => GlobalArg.createInput(ai.flatten.flatten)
      case ai: Array[Array[Array[Array[Int]]]] => GlobalArg.createInput(ai.flatten.flatten.flatten)

      case  d: Double => ValueArg.create(d)
      case ad: Array[Double] => GlobalArg.createInput(ad)
      case ad: Array[Array[Double]] => GlobalArg.createInput(ad.flatten)
      case ad: Array[Array[Array[Double]]] => GlobalArg.createInput(ad.flatten.flatten)
      case ad: Array[Array[Array[Array[Double]]]] => GlobalArg.createInput(ad.flatten.flatten.flatten)

      case _ => throw new IllegalArgumentException("Kernel argument is of unsupported type: " +
        arg.getClass.toString)
    }
  }

  private def castToOutputType[R](dt: DataType, output: GlobalArg): R = {
    assert(dt.isInstanceOf[ArrayType] || dt.isInstanceOf[DepArrayType])
    (DataType.getBaseDataType(dt) match {
      case idealised.DPIA.Types.float  => output.asFloatArray()
      case idealised.DPIA.Types.int    => output.asIntArray()
      case idealised.DPIA.Types.double => output.asDoubleArray()
      case _ => throw new IllegalArgumentException("Return type of the given lambda expression " +
        "not supported: " + dt.toString)
    }).asInstanceOf[R]
  }

  private implicit def getDataType(i: Identifier[_]): DataType = {
    i.t match {
      case ExpType(dataType) => dataType
      case AccType(dataType) => dataType
      case PairType(ExpType(dt1), AccType(dt2)) if dt1 == dt2 => dt1
      case _ => throw new Exception("This should not happen")
    }
  }

  private def sizeInByte(dt: DataType): SizeInByte = dt match {
    case s: ScalarType => s match {
      case idealised.DPIA.Types.bool    => SizeInByte(1)
      case idealised.DPIA.Types.int     => SizeInByte(4)
      case idealised.DPIA.Types.float   => SizeInByte(4)
      case idealised.DPIA.Types.double  => SizeInByte(8)
    }
    case _: IndexType   => SizeInByte(4) // == sizeof(int)
    case v: VectorType  => sizeInByte(v.elemType) * v.size
    case r: RecordType  => sizeInByte(r.fst) + sizeInByte(r.snd)
    case a: ArrayType   => sizeInByte(a.elemType) * a.size
    case a: DepArrayType => SizeInByte(BigSum(Cst(0), a.size - 1, `for`=a.i, in=sizeInByte(a.elemType).value))
    case _: DataTypeIdentifier => throw new Exception("This should not happen")
  }

  private implicit class SubstitutionsHelper(size: SizeInByte) {
    def `with`(valueMap: immutable.Map[Nat, Nat]): SizeInByte = {
      SizeInByte(ArithExpr.substitute(size.value, valueMap))
    }
  }
}

sealed case class KernelWithSizes(kernel: Kernel,
                                  localSize: NDRange,
                                  globalSize: NDRange) {
  def as[F <: FunctionHelper](implicit ev: F#T <:< HList): F#T => (F#R, TimeSpan[Time.ms]) =
    kernel.as[F](localSize, globalSize)

  def code: String= kernel.code
}

sealed case class KernelNoSizes(kernel: Kernel) {
  //noinspection TypeAnnotation
  def as[F <: FunctionHelper](implicit ev: F#T <:< HList) = new {
    def apply(localSize: NDRange, globalSize: NDRange): F#T => (F#R, TimeSpan[Time.ms]) =
      kernel.as[F](localSize, globalSize)

    def withSizes(localSize: NDRange, globalSize: NDRange): F#T => (F#R, TimeSpan[Time.ms]) =
      kernel.as[F](localSize, globalSize)
  }

  def code: String= kernel.code
}
