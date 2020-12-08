package shine.C

import shine.DPIA.Nat
import shine.DPIA.Phrases.Identifier
import shine.DPIA.Types.{AccType, DataType, ExpType}
import shine.OpenCL.{GlobalSize, LocalSize, NDRange, get_global_size, get_local_size, get_num_groups}
import shine._
import util.KernelArgCreator
import yacx.{ByteArg, DoubleArg, Executor, FloatArg, HalfArg, IntArg, LongArg, ShortArg}

import scala.collection.immutable.List

//TODO: this class needs to be refatored
case class Program(decls: Seq[C.AST.Decl],
                   function: C.AST.FunDecl,
                   outputParam: Identifier[AccType],
                   inputParams: Seq[Identifier[ExpType]])
  extends util.Kernel(decls, function, outputParam, inputParams, Nil, null) {

  override def code: String =
    "#include <stdint.h>\n" +
    decls.map(C.AST.Printer(_)).mkString("\n") +
    "\n\n" +
    C.AST.Printer(function)

  override protected def execute(localSize: LocalSize, globalSize: GlobalSize, sizeVarMapping: Map[Nat, Nat], kernelArgs: List[KernelArg], compilerOptions: Array[String]): Double = {
    val kernelArgsCUDA = kernelArgs.map(_.asInstanceOf[KernelArgCUDA].kernelArg)

    val runtime = Executor.executeC(code, this.function.name, kernelArgsCUDA.toArray: _*)

    runtime.asInstanceOf[Double]
  }

  override protected def dispose(kernelArgs: List[KernelArg]): Unit =  kernelArgs.map(_.asInstanceOf[KernelArgCUDA].kernelArg.dispose())

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
  override protected def createLocalArg(sizeInBytes: Long): KernelArgCUDA = ???

  override protected def createOutputArg(numberOfElements: Int, dataType: DataType): KernelArgCUDA = {
    KernelArgCUDA(dataType match {
      case shine.DPIA.Types.i8 =>
        println(s"Allocated global byte-argument with $numberOfElements bytes")
        ByteArg.createOutput(numberOfElements);
      case shine.DPIA.Types.i16 =>
        println(s"Allocated global short-argument with ${numberOfElements * 2} bytes")
        ShortArg.createOutput(numberOfElements);
      case shine.DPIA.Types.i32 | shine.DPIA.Types.int =>
        println(s"Allocated global int-argument with ${numberOfElements * 4} bytes")
        IntArg.createOutput(numberOfElements);
      case shine.DPIA.Types.i64 =>
        println(s"Allocated global long-argument with ${numberOfElements * 8} bytes")
        LongArg.createOutput(numberOfElements);
      case shine.DPIA.Types.f16 =>
        println(s"Allocated global half-argument with ${numberOfElements * 2} bytes")
        HalfArg.createOutput(numberOfElements);
      case shine.DPIA.Types.f32 =>
        println(s"Allocated global float-argument with ${numberOfElements * 4} bytes")
        FloatArg.createOutput(numberOfElements);
      case shine.DPIA.Types.f64 =>
        println(s"Allocated global double-argument with ${numberOfElements * 8} bytes")
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
      case  b: Byte => createValueArg(b)
      case ab: Array[Byte] => createArrayArg(ab)
      case ab: Array[Array[Byte]] => createArrayArg(ab.flatten)
      case ab: Array[Array[Array[Byte]]] => createArrayArg(ab.flatten.flatten)
      case ab: Array[Array[Array[Array[Byte]]]] => createArrayArg(ab.flatten.flatten.flatten)

      case  s: Short => createValueArg(s)
      case as: Array[Short] => createArrayArg(as)
      case as: Array[Array[Short]] => createArrayArg(as.flatten)
      case as: Array[Array[Array[Short]]] => createArrayArg(as.flatten.flatten)
      case as: Array[Array[Array[Array[Short]]]] => createArrayArg(as.flatten.flatten.flatten)

      case  i: Int => createValueArg(i)
      case ai: Array[Int] => createArrayArg(ai)
      case ai: Array[Array[Int]] => createArrayArg(ai.flatten)
      case ai: Array[Array[Array[Int]]] => createArrayArg(ai.flatten.flatten)
      case ai: Array[Array[Array[Array[Int]]]] => createArrayArg(ai.flatten.flatten.flatten)

      case  l: Long => createValueArg(l)
      case al: Array[Long] => createArrayArg(al)
      case al: Array[Array[Long]] => createArrayArg(al.flatten)
      case al: Array[Array[Array[Long]]] => createArrayArg(al.flatten.flatten)
      case al: Array[Array[Array[Array[Long]]]] => createArrayArg(al.flatten.flatten.flatten)

      case  f: Float => createValueArg(f)
      case af: Array[Float] => createArrayArg(af)
      case af: Array[Array[Float]] => createArrayArg(af.flatten)
      case af: Array[Array[Array[Float]]] => createArrayArg(af.flatten.flatten)
      case af: Array[Array[Array[Array[Float]]]] => createArrayArg(af.flatten.flatten.flatten)

      case  d: Double => createValueArg(d)
      case ad: Array[Double] => createArrayArg(ad)
      case ad: Array[Array[Double]] => createArrayArg(ad.flatten)
      case ad: Array[Array[Array[Double]]] => createArrayArg(ad.flatten.flatten)
      case ad: Array[Array[Array[Array[Double]]]] => createArrayArg(ad.flatten.flatten.flatten)

      case p: Array[(_, _)] => p.head match {
        case (_: Int, _: Float) =>
          IntArg.create(flattenToArrayOfInts(p.asInstanceOf[Array[(Int, Float)]]):_*)
        case _ => ???
      }
      case pp: Array[Array[(_, _)]] => pp.head.head match {
        case (_: Int, _: Float) =>
          IntArg.create(pp.flatMap(a => flattenToArrayOfInts(a.asInstanceOf[Array[(Int, Float)]])):_*)
        case _ => ???
      }

      case _ => throw new IllegalArgumentException("Kernel argument is of unsupported type: " +
        arg.getClass.getName)
    })
  }

  private def createArrayArg(array: Array[Byte]): ByteArg = {
    println(s"Allocated global byte-argument with ${array.length * 1} bytes")
    ByteArg.create(array:_*)
  }

  private def createArrayArg(array: Array[Short]): ShortArg = {
    println(s"Allocated global short-argument with ${array.length * 2} bytes")
    ShortArg.create(array:_*)
  }

  private def createArrayArg(array: Array[Int]): IntArg = {
    println(s"Allocated global int-argument with ${array.length * 4} bytes")
    IntArg.create(array:_*)
  }

  private def createArrayArg(array: Array[Long]): LongArg = {
    println(s"Allocated global long-argument with ${array.length * 8} bytes")
    LongArg.create(array:_*)
  }

  private def createArrayArg(array: Array[Float]): FloatArg = {
    println(s"Allocated global float-argument with ${array.length * 4} bytes")
    FloatArg.create(array:_*)
  }

  private def createArrayArg(array: Array[Double]): DoubleArg = {
    println(s"Allocated global double-argument with ${array.length * 8} bytes")
    DoubleArg.create(array:_*)
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

  private def createValueArg(value: Float): yacx.KernelArg = {
    println(s"Allocated value float-argument with 4 bytes")
    FloatArg.createValue(value)
  }

  private def createValueArg(value: Double): yacx.KernelArg = {
    println(s"Allocated value double-argument with 8 bytes")
    DoubleArg.createValue(value)
  }
}

