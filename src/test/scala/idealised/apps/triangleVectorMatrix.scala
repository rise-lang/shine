//package idealised.apps
//
//import idealised.OpenCL.SurfaceLanguage.DSL._
//import idealised.OpenCL._
//import idealised.OpenMP.SurfaceLanguage.DSL.depMapPar
//import idealised.SurfaceLanguage.DSL._
//import idealised.SurfaceLanguage.Types._
//import idealised.SurfaceLanguage._
//import idealised.util.SyntaxChecker
//import idealised.utils.Display
//import lift.arithmetic.{ArithExpr, Cst}
//import opencl.executor.Executor
//
//import scala.language.{implicitConversions, postfixOps}
/*
class triangleVectorMultNoExecutor extends idealised.util.Tests {
  val mult = fun(x => x._1 * x._2)

  val add = fun(x => fun(y => x + y))

  val multSumAcc = fun(x => fun(y => (x._1 * x._2) + y))


  val triangleVectorMultSeq: Expr =
    fun(DepArrayType(8, i => ArrayType(i + 1, int)))(triangle =>
      fun(ArrayType(8, int))(vector =>
        depMapSeq(fun(row => zip(row, take(Macros.GetLength(row), vector))
          :>> mapSeq(mult) :>> reduceSeq(add, 0)
        ), triangle)
      )
    )

  val triangleVectorMultPar: Expr =
    fun(DepArrayType(8, i => ArrayType(i + 1, int)))(triangle =>
      fun(ArrayType(8, int))(vector =>
        depMapPar(fun(row => zip(row, take(Macros.GetLength(row), vector))
          :>> mapSeq(mult) :>> reduceSeq(add, 0)
        ), triangle)
      )
    )

  val triangleVectorMultSeqOpenCL: Expr =
    fun(DepArrayType(8, i => ArrayType(i + 1, int)))(triangle =>
      fun(ArrayType(8, int))(vector =>
        depMapSeq(fun(row => zip(row, take(Macros.GetLength(row), vector))
          :>> toGlobal(mapSeq(mult)) :>> oclReduceSeq(add, 0, PrivateMemory)
        ), triangle)
      )
    )

  val triangleVectorMultGlobal: Expr =
    fun(DepArrayType(8, i => ArrayType(i + 1, int)))(triangle =>
      fun(ArrayType(8, int))(vector =>
        depMapGlobal(fun(row => zip(row, take(Macros.GetLength(row), vector))
          :>> toGlobal(mapSeq(mult)) :>> oclReduceSeq(add, 0, PrivateMemory)
        ), triangle)
      )
    )

  def generateInputs(n:Int):(Array[Array[Float]], Array[Float]) = {
    val inputVector = Array.tabulate(n)(id => id + 1.0f)
    val inputMatrix = Array.tabulate(n)(rowIndex => Array.tabulate(rowIndex + 1)(colIndex => if (rowIndex == colIndex) 1.0f else 0.0f))

    (inputMatrix, inputVector)
  }

  def triangleVectorMultGlobalFused(N:ArithExpr): Expr =
    fun(DepArrayType(N, i => ArrayType(i + 1, float)))(triangle =>
      fun(ArrayType(N, float))(vector =>
        depMapGlobal(fun(row => zip(row, take(Macros.GetLength(row), vector)) :>> oclReduceSeq(multSumAcc, 0.0f, PrivateMemory)
        ), triangle)
      )
    )

  def scalaMultSumAcc(acc:Float, item:(Float,Float)):Float = acc + (item._1 * item._2)

  def scalaMatrixVector(matrix:Array[Array[Float]], vector:Array[Float]): Array[Float] = {
    matrix.map(row => row.zip(vector.take(row.length)).foldLeft(0.0f)(scalaMultSumAcc))
  }

  test("Basic triangle vector multiplication type inference works") {
    noException should be thrownBy {
      // TODO: fix comparison of DepArrayType objects in the SurfaceLanguage.
      //assertResult(FunctionType(DepArrayType(8, i => ArrayType(i + 1, int)), FunctionType(ArrayType(8, int), DepArrayType(8, _ => int)))) {
      TypeInference(triangleVectorMultSeq, Map()).t.get
    }
  }

  test("Basic sequential triangle vector multiplication compiles to syntactically correct C") {
    val p = idealised.C.ProgramGenerator.makeCode(idealised.DPIA.FromSurfaceLanguage(TypeInference(triangleVectorMultSeq, Map())))
    println(p.code)
    SyntaxChecker(p.code)
  }

  test("Basic sequential triangle vector multiplication compiles to syntactically correct OpenMP") {
    val p = idealised.OpenMP.ProgramGenerator.makeCode(idealised.DPIA.FromSurfaceLanguage(TypeInference(triangleVectorMultSeq, Map())))
    println(p.code)
    SyntaxChecker(p.code)
  }

  test("Basic parallel triangle vector multiplication compiles to syntactically correct OpenMP") {
    val p = idealised.OpenMP.ProgramGenerator.makeCode(idealised.DPIA.FromSurfaceLanguage(TypeInference(triangleVectorMultPar, Map())))
    println(p.code)
    SyntaxChecker(p.code)
  }

  test("Basic sequential triangle vector multiplication compiles to syntactically correct OpenCL") {
    val p = idealised.OpenCL.KernelGenerator.makeCode(idealised.DPIA.FromSurfaceLanguage(TypeInference(triangleVectorMultSeqOpenCL, Map())))
    println(p.code)
    SyntaxChecker.checkOpenCL(p.code)
  }

  case class TriangleMatrixConfResult(inputSize:Int,
                                      splitSize:Int,
                                      localSize:Int,
                                      globalSize:Int,
                                      runtime:Double,
                                      correct:Boolean,
                                      code:String) extends Display {
    def display:String = {
      s"input = $inputSize; splitSize = $splitSize; localSize = $localSize; globalSize = $globalSize; runtime:$runtime correct:$correct"
    }
  }


  private def triangleMatrixBasic(inputSize:Int, localSize:Int, globalSize:Int):TriangleMatrixConfResult = {
    import idealised.OpenCL._
    val actualN = inputSize
    val f = triangleVectorMultGlobalFused(actualN)

    val kernel = idealised.OpenCL.KernelGenerator.makeCode(localSize, globalSize)(idealised.DPIA.FromSurfaceLanguage(TypeInference(f, Map())))
    //println(kernel.code)

    val(inputMatrix, inputVector) = generateInputs(actualN)

    val kernelFun = kernel.as[ScalaFunction `(` Array[Array[Float]] `,` Array[Float] `)=>` Array[Float]]

    val (output, time) = kernelFun(inputMatrix `,` inputVector)

    val scalaOutput = scalaMatrixVector(inputMatrix, inputVector)

    val correct = output.zip(scalaOutput).forall{case (x,y) => Math.abs(x - y) < 0.01}

    TriangleMatrixConfResult(inputSize, 0, localSize, globalSize, time.value, correct, kernel.code)
  }

  ignore ("Basic parallel triangle vector multiplication compiles to syntactically correct OpenCL") {
    val inputSize = 4096
    val results = for(
      localSize <- Seq(4, 8, 16, 32, 64, 128, 256, 512)
    ) yield {
      triangleMatrixBasic(inputSize, localSize, inputSize)
    }
    results.filter(_.correct).sortBy(_.runtime).foreach(x => println(x.display))
  }


  private def triangleMatrixPadSplit(inputSize:Int, splitSize:Int, localSize:Int, globalSize:Int):TriangleMatrixConfResult = {
    import idealised.OpenCL._
    val actualN = inputSize
    val splitN = splitSize
    val f: Expr = {

      val SPLIT_SIZE = Cst(splitN)
      nFun(N =>
      fun(DepArrayType(N, i => ArrayType(i + 1, float)))(triangle =>
        fun(ArrayType(N, float))(vector =>
          depMapWorkgroup.withIndex(nFun(rowIndex => fun(row =>
            zip(pad(0, N - rowIndex - 1, 0.0f, row), vector) :>> split(SPLIT_SIZE) :>> mapLocal(oclReduceSeq(multSumAcc, 0.0f, PrivateMemory))
          )), triangle)
        )
      ))
    }

    val kernel = idealised.OpenCL.KernelGenerator.makeCode(localSize, globalSize)(idealised.DPIA.FromSurfaceLanguage(TypeInference(f, Map())))

    val(inputMatrix, inputVector) = generateInputs(actualN)
    val scalaOutput = scalaMatrixVector(inputMatrix, inputVector)

    val kernelFun = kernel.as[ScalaFunction `(` Int `,` Array[Array[Float]] `,` Array[Float] `)=>` Array[Float]]

    val (partialOutput, time) = kernelFun(actualN `,` inputMatrix `,` inputVector)

    val finalOutput = partialOutput.grouped(actualN/splitN).map(_.sum).toArray

    val correct = finalOutput.zip(scalaOutput).forall{case (x,y) => Math.abs(x - y) < 0.01}

    TriangleMatrixConfResult(
      inputSize, splitSize, localSize, globalSize, time.value, correct, kernel.code
    )
  }

  test ("Parallel OpenCL triangle vector partial multiplication (padding the row up to vector) (PLDI '19 submission listing 5)") {
    Executor.loadAndInit()

    val inputSize = 4096
    println(Executor.getPlatformName)
    println(Executor.getDeviceName)

    val result = try {
      triangleMatrixPadSplit(inputSize, 8, 8, inputSize)
    } finally {
      Executor.shutdown()
    }

    assert(result.correct)
  }
}
*/
