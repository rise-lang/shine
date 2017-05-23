package idealised.OpenCL

import idealised.Core._
import ir.Type
import lift.arithmetic.{ArithExpr, Cst, Var}
import opencl.executor._
import opencl.generator.OpenCLAST.ParamDecl
import opencl.generator.OpenCLPrinter
import opencl.generator._
import opencl.ir.{Double, Float, Int}

import scala.collection.{Seq, immutable}
import scala.collection.immutable.List
import scala.language.implicitConversions

case class Kernel(function: OpenCLAST.Function,
                  outputParam: Identifier[AccType],
                  inputParams: List[Identifier[ExpType]],
                  intermediateParams: List[Identifier[VarType]],
                  localSize: Nat, globalSize: Nat) {

  def code: String = (new OpenCLPrinter)(this)

  def asFunction[F <: FunctionHelper](implicit ev: F#T <:< HList): (F#T) => (F#R, TimeSpan[Time.ms]) = {
    (args: F#T) => {
      val lengthMapping = createLengthMap(inputParams, args)

      val (outputArg, inputArgs) = createKernelArgs(args, lengthMapping)
      val kernelArgs = (outputArg +: inputArgs).toArray

      val kernelJNI = opencl.executor.Kernel.create(code, function.name, "")

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
                              args: HList): immutable.Map[Nat, Nat] = {
    val seq = (params, args.toList).zipped.flatMap(createLengthMapping)
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
      case (rt: RecordType, tuple: (_, _)) =>
        createLengthMapping(rt.fst, tuple._1) ++
          createLengthMapping(rt.snd, tuple._2)
      case _ => throw new Exception(s"Expected $t but got $a")
    }
  }

  private def createKernelArgs(args: HList,
                               lengthMapping: immutable.Map[Nat, Nat]) = {
    val numberOfKernelArgs = 1 + args.length + intermediateParams.size + lengthMapping.size
    assert(function.params.length == numberOfKernelArgs)

    println("Allocations on the host: ")
    val outputArg = createOutputArg(DataType.sizeInByte(outputParam) `with` lengthMapping)
    val inputArgs = createInputArgs(args.toList)
    val intermediateArgs = createIntermediateArgs(args.length, lengthMapping)
    val lengthArgs = createLengthArgs(lengthMapping)

    (outputArg, inputArgs ++ intermediateArgs ++ lengthArgs)
  }

  private def createOutputArg(size: SizeInByte) = {
    println(s"output (global): $size")
    GlobalArg.createOutput(size.value.eval)
  }

  private def createInputArgs(args: List[Any]) = {
    args.map(createInputArg)
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

  private def createIntermediateArgs(argsLength: Int,
                                     lengthMapping: immutable.Map[Nat, Nat]) = {
    val intermediateParamDecls = getIntermediateParamDecls(argsLength)
    (intermediateParamDecls, intermediateParams).zipped.map { case (pDecl, param) =>
      val size = (DataType.sizeInByte(param) `with` lengthMapping).value.max.eval
      pDecl.addressSpace match {
        case opencl.ir.LocalMemory =>
          println(s"intermediate (local): $size bytes")
          LocalArg.create(size)
        case opencl.ir.GlobalMemory =>
          println(s"intermediate (global): $size bytes")
          GlobalArg.createOutput(size)
      }
    }
  }

  private def getIntermediateParamDecls(argsLength: Int): List[ParamDecl] = {
    val startIndex = 1 + argsLength
    function.params.slice(startIndex, startIndex + intermediateParams.size)
  }

  private def castToOutputType[R](dt: DataType, output: GlobalArg): R = {
    assert(dt.isInstanceOf[ArrayType])
    (Type.getBaseType(DataType.toType(dt)) match {
      case Float  => output.asFloatArray()
      case Int    => output.asIntArray()
      case Double => output.asDoubleArray()
      case _ => throw new IllegalArgumentException("Return type of the given lambda expression " +
        "not supported: " + dt.toString)
    }).asInstanceOf[R]
  }

  private def createLengthArgs(lengthMapping: immutable.Map[Nat, Nat]) = {
    lengthMapping.map( p => {
      println("length (private): 4 bytes")
      ValueArg.create(p._2.eval)
    } ).toList
  }

  private implicit class SubstitutionsHelper(size: SizeInByte) {
    def `with`(valueMap: immutable.Map[Nat, Nat]): SizeInByte = {
      SizeInByte(ArithExpr.substitute(size.value, valueMap))
    }
  }

  private implicit def getDataType(i: Identifier[_]): DataType = {
    i.t match {
      case ExpType(dataType) => dataType
      case AccType(dataType) => dataType
      case PairType(ExpType(dt1), AccType(dt2)) if dt1 == dt2 => dt1
      case _ => throw new Exception("This should not happen")
    }
  }
}
