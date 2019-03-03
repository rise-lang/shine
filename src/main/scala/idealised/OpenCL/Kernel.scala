package idealised.OpenCL

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

case class Kernel(decls: Seq[C.AST.Decl],
                  kernel: OpenCL.AST.KernelDecl,
                  outputParam: Identifier[AccType],
                  inputParams: Seq[Identifier[ExpType]],
                  intermediateParams: Seq[Identifier[VarType]],
                  localSize: Nat, globalSize: Nat) {

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
  def as[F <: FunctionHelper](implicit ev: F#T <:< HList): F#T => (F#R, TimeSpan[Time.ms]) = {
    hArgs: F#T => {
      val args: List[Any] = hArgs.toList

      val lengthMapping = createLengthMap(inputParams, args)

      val (outputArg, inputArgs) = createKernelArgs(args, lengthMapping)
      val kernelArgs = (outputArg +: inputArgs).toArray

      val c = code
      val kernelJNI = opencl.executor.Kernel.create(c, kernel.name, "")

      List(localSize match {
        case ? => Some("OpenCL local size not specified\n")
        case x if !x.isEvaluable => Some(s"OpenCL local size is not evaluable (currently set to $x)")
        case x => None
      }, globalSize match {
        case ? => Some("OpenCL global size ot specified\n")
        case x if !x.isEvaluable => Some(s"OpenCL local size is not evaluable (currently set to $x)")
        case _ => None
      }).filter(_.isDefined).map(_.get) match {
        case Nil =>
        case problems =>
          val errorMessage = "Cannot run kernel:\n" ++ problems.reduce(_ ++ _)
          throw new Exception(errorMessage)
      }

      val runtime = Executor.execute(kernelJNI,
        ArithExpr.substitute(localSize, lengthMapping).eval, 1, 1,
        ArithExpr.substitute(globalSize, lengthMapping).eval, 1, 1,
        kernelArgs)

      val output = castToOutputType[F#R](outputParam.`type`.dataType, outputArg)

      kernelArgs.foreach(_.dispose)
      kernelJNI.dispose()

      (output, TimeSpan.inMilliseconds(runtime))
    }
  }

  private def createLengthMap(params: Seq[Identifier[ExpType]],
                              args: List[Any]): immutable.Map[Nat, Nat] = {
    val seq = (params, args).zipped.flatMap(createLengthMapping)
    seq.map(x => (x._1, Cst(x._2))).toMap
  }

  private def createLengthMapping(p: Identifier[ExpType], a: Any): Seq[(Nat, Int)] = {
    createLengthMapping(p.t.dataType, a).filter(_._1.isInstanceOf[Var])
  }

  private def createLengthMapping(t: DataType, a: Any): Seq[(Nat, Int)] = {
    (t, a) match {
      case (bt: BasicType, _) => (bt, a) match {
        case (`bool`, _: Boolean) => Seq()
        case (`int`, _: Int) => Seq()
        case (`float`, _: Float) => Seq()
        case (VectorType(_, ebt), _) => createLengthMapping(ebt, a)
        case _ => throw new Exception(s"Expected $bt, but got $a")
      }
      case (at: ArrayType, array: Array[_]) =>
        Seq((at.size, array.length)) ++ createLengthMapping(at.elemType, array.head)
      case (at: DepArrayType, array:Array[_]) =>
        Seq((at.size, array.length)) ++ createLengthMapping(at.elemFType.body, array.head)
      case (rt: RecordType, tuple: (_, _)) =>
        createLengthMapping(rt.fst, tuple._1) ++
          createLengthMapping(rt.snd, tuple._2)
      case _ => throw new Exception(s"Expected $t but got $a")
    }
  }

  private def createKernelArgs(args: List[Any], lengthMapping: immutable.Map[Nat, Nat]): (GlobalArg, List[KernelArg]) = {
    val numberOfKernelArgs = 1 + args.length + intermediateParams.size + lengthMapping.size
    assert(kernel.params.length == numberOfKernelArgs)

    val outputArg = createOutputArg(sizeInByte(outputParam) `with` lengthMapping)
    val inputArgs = args.map(createInputArg)
    val intermediateArgs = createIntermediateArgs(args.length, lengthMapping)
    val lengthArgs = createLengthArgs(lengthMapping)

    (outputArg, inputArgs ++ intermediateArgs ++ lengthArgs)
  }

  private def createOutputArg(size: SizeInByte): GlobalArg = {
    GlobalArg.createOutput(size.value.eval)
  }

  private def createInputArg(arg: Any): KernelArg = {
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

  private def createIntermediateArgs(argsLength: Int, lengthMapping: immutable.Map[Nat, Nat]): Seq[KernelArg] = {
    val intermediateParamDecls = getIntermediateParamDecls(argsLength)
    (intermediateParamDecls, intermediateParams).zipped.map { case (pDecl, param) =>
      val size = (sizeInByte(param) `with` lengthMapping).value.max.eval
      pDecl.addressSpace match {
        case OpenCL.LocalMemory =>
          println(s"intermediate (local): $size bytes")
          LocalArg.create(size)
        case OpenCL.GlobalMemory =>
          println(s"intermediate (global): $size bytes")
          GlobalArg.createOutput(size)
        case OpenCL.PrivateMemory =>
          throw new Exception("Intermediate argument can not be in private memory")
      }
    }
  }

  private def getIntermediateParamDecls(argsLength: Int): collection.Seq[OpenCL.AST.ParamDecl] = {
    val startIndex = 1 + argsLength
    kernel.params.slice(startIndex, startIndex + intermediateParams.size)
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

  private def createLengthArgs(lengthMapping: immutable.Map[Nat, Nat]): immutable.Seq[ValueArg] = {
    // create length args sorted by names (cmp. KernelGenerator.makeLengthParams)
    lengthMapping.toList.map({
      case (v: Var, n) => (v.name,  ValueArg.create(n.eval))
      case _ => throw new Exception("length mapping should only contain variables")
    }).sortBy(_._1).map(_._2)
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
    case a: DepArrayType => SizeInByte(BigSum(Cst(0), a.size - 1, `for`=a.elemFType.x, in=sizeInByte(a.elemFType.body).value))
    case _: DataTypeIdentifier => throw new Exception("This should not happen")
  }

  private implicit class SubstitutionsHelper(size: SizeInByte) {
    def `with`(valueMap: immutable.Map[Nat, Nat]): SizeInByte = {
      SizeInByte(ArithExpr.substitute(size.value, valueMap))
    }
  }
}
