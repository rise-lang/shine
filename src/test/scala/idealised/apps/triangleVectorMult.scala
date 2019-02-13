package idealised.apps

import idealised.OpenCL.SurfaceLanguage.DSL.{depMapGlobal, depMapWorkgroup, mapLocal}
import idealised.OpenMP.SurfaceLanguage.DSL.depMapPar
import idealised.SurfaceLanguage.DSL._
import idealised.SurfaceLanguage.Types._
import idealised.SurfaceLanguage._
import idealised.util.SyntaxChecker
import lift.arithmetic.{?, ArithExpr, Cst, SizeVar}
import opencl.executor.Executor

import scala.language.{implicitConversions, postfixOps}
import scala.util.Random

class triangleVectorMult extends idealised.util.TestsWithExecutor {
  val mult = fun(x => x._1 * x._2)

  val add = fun(x => fun(y => x + y))

  val multSumAcc = fun(x => fun(y => (x._1 * x._2) + y))


  val triangleVectorMultSeq: Expr[DataType -> (DataType -> DataType)] =
    fun(DepArrayType(8, i => ArrayType(i + 1, int)))(triangle =>
      fun(ArrayType(8, int))(vector =>
        depMapSeq(fun(row => zip(row, take(Macros.GetLength(row), vector))
          :>> mapSeq(mult) :>> reduceSeq(add, 0)
        ), triangle)
      )
    )

  val triangleVectorMultPar: Expr[DataType -> (DataType -> DataType)] =
    fun(DepArrayType(8, i => ArrayType(i + 1, int)))(triangle =>
      fun(ArrayType(8, int))(vector =>
        depMapPar(fun(row => zip(row, take(Macros.GetLength(row), vector))
          :>> mapSeq(mult) :>> reduceSeq(add, 0)
        ), triangle)
      )
    )

  val triangleVectorMultGlobal: Expr[DataType -> (DataType -> DataType)] =
    fun(DepArrayType(8, i => ArrayType(i + 1, int)))(triangle =>
      fun(ArrayType(8, int))(vector =>
        depMapGlobal(fun(row => zip(row, take(Macros.GetLength(row), vector))
          :>> mapSeq(mult) :>> reduceSeq(add, 0)
        ), triangle)
      )
    )

  def generateInputs(n:Int):(Array[Array[Float]], Array[Float]) = {
    val rng = new Random()
    val inputVector = Array.tabulate(n)(id => id + 1.0f)
    val inputMatrix = Array.tabulate(n)(rowIndex => Array.tabulate(rowIndex + 1)(colIndex => if (rowIndex == colIndex) 1.0f else 0.0f))

    (inputMatrix, inputVector)
  }

  def triangleVectorMultGlobalFused(N:ArithExpr): Expr[DataType -> (DataType -> DataType)] =
    fun(DepArrayType(N, i => ArrayType(i + 1, float)))(triangle =>
      fun(ArrayType(N, float))(vector =>
        depMapGlobal(fun(row => zip(row, take(Macros.GetLength(row), vector)) :>> reduceSeq(multSumAcc, 0.0f)
        ), triangle)
      )
    )

  def scalaMultSumAcc(acc:Float, item:(Float,Float)):Float = acc + (item._1 * item._2)

  def scalaMatrixVector(matrix:Array[Array[Float]], vector:Array[Float]) = {
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
    val p = idealised.C.ProgramGenerator.makeCode(TypeInference(triangleVectorMultSeq, Map()).toPhrase)
    println(p.code)
    SyntaxChecker(p.code)
  }

  test("Basic sequential triangle vector multiplication compiles to syntactically correct OpenMP") {
    val p = idealised.OpenMP.ProgramGenerator.makeCode(TypeInference(triangleVectorMultSeq, Map()).toPhrase)
    println(p.code)
    SyntaxChecker(p.code)
  }

  test("Basic parallel triangle vector multiplication compiles to syntactically correct OpenMP") {
    val p = idealised.OpenMP.ProgramGenerator.makeCode(TypeInference(triangleVectorMultPar, Map()).toPhrase)
    println(p.code)
    SyntaxChecker(p.code)
  }

  test("Basic sequential triangle vector multiplication compiles to syntactically correct OpenCL") {
    val p = idealised.OpenCL.KernelGenerator.makeCode(TypeInference(triangleVectorMultSeq, Map()).toPhrase, ?, ?)
    println(p.code)
    SyntaxChecker.checkOpenCL(p.code)
  }

  case class TriangleMatrixConfResult(inputSize:Int, splitSize:Int, localSize:Int, globalSize:Int, runtime:Double, correct:Boolean, code:String) {
    def printout(): Unit = {
      println(s"input = $inputSize; splitSize = $splitSize; localSize = $localSize; globalSize = $globalSize; runtime:$runtime correct:$correct")
    }
  }


  private def triangleMatrixBasic(inputSize:Int, localSize:Int, globalSize:Int):TriangleMatrixConfResult = {
    import idealised.OpenCL._
    val actualN = inputSize
    val f = triangleVectorMultGlobalFused(actualN)

    val kernel = idealised.OpenCL.KernelGenerator.makeCode(TypeInference(f, Map()).toPhrase, localSize, globalSize)
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
    results.filter(_.correct).sortBy(_.runtime).foreach(_.printout())
  }


  private def triangleMatrixPadSplit(inputSize:Int, splitSize:Int, localSize:Int, globalSize:Int):TriangleMatrixConfResult = {
    import idealised.OpenCL._
    val actualN = inputSize
    val splitN = splitSize
    val f: Expr[DataType -> (DataType -> DataType)] = {

      val N:ArithExpr = SizeVar("N")
      val SPLIT_SIZE = Cst(splitN)
      fun(DepArrayType(N, i => ArrayType(i + 1, float)))(triangle =>
        fun(ArrayType(N, float))(vector =>
          depMapWorkgroup.withIndex(dFun(rowIndex => fun(row =>
            zip(pad(0, N - rowIndex - 1, 0.0f, row), vector) :>> split(SPLIT_SIZE) :>> mapLocal(reduceSeq(multSumAcc, 0.0f))
          )), triangle)
        )
      )
    }

    val kernel = idealised.OpenCL.KernelGenerator.makeCode(TypeInference(f, Map()).toPhrase, localSize, globalSize)

    val(inputMatrix, inputVector) = generateInputs(actualN)
    val scalaOutput = scalaMatrixVector(inputMatrix, inputVector)

    val kernelFun = kernel.as[ScalaFunction `(` Array[Array[Float]] `,` Array[Float] `)=>` Array[Float]]

    val (partialOutput, time) = kernelFun(inputMatrix `,` inputVector)

    val finalOutput = partialOutput.grouped(actualN/splitN).map(_.sum).toArray

    val correct = finalOutput.zip(scalaOutput).forall{case (x,y) => Math.abs(x - y) < 0.01}

    TriangleMatrixConfResult(
      inputSize, splitSize, localSize, globalSize, time.value, correct, kernel.code
    )
  }

  ignore ("Parallel OpenCL triangle vector partial multiplication (padding the row up to vector) (PLDI '19 submission listing 5)") {
    val inputSize = 4096
    println(Executor.getPlatformName)
    println(Executor.getDeviceName)

    val results = for (localSize <- Seq(4, 8, 16, 32, 64, 128, 256, 512);
                       splitSize <- Seq(4, 8, 16, 32, 64, 128, 256, 512)
    ) yield {
      triangleMatrixPadSplit(inputSize, splitSize, localSize, inputSize)
    }

    results.sortBy(_.runtime).foreach(_.printout())
  }

  test("Parallel triangle vector multiplication with global threads compiles to syntactically correct OpenCL") {
    val kernel = idealised.OpenCL.KernelGenerator.makeCode(TypeInference(triangleVectorMultGlobal, Map()).toPhrase, ?, ?)
    SyntaxChecker.checkOpenCL(kernel.code)
  }
}
