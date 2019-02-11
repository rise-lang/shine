package idealised.apps

import idealised.OpenCL.SurfaceLanguage.DSL.{depMapGlobal, depMapWorkgroup, mapLocal}
import idealised.OpenMP.SurfaceLanguage.DSL.depMapPar
import idealised.SurfaceLanguage.DSL._
import idealised.SurfaceLanguage.Types._
import idealised.SurfaceLanguage._
import idealised.util.SyntaxChecker
import lift.arithmetic.{?, ArithExpr, Cst, SizeVar}

import scala.language.{implicitConversions, postfixOps}

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

  val triangleVectorMultGlobalFused: Expr[DataType -> (DataType -> DataType)] =
    fun(DepArrayType(8, i => ArrayType(i + 1, int)))(triangle =>
      fun(ArrayType(8, int))(vector =>
        depMapGlobal(fun(row => zip(row, take(Macros.GetLength(row), vector)) :>> reduceSeq(multSumAcc, 0)
        ), triangle)
      )
    )

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

  test("Basic parallel triangle vector multiplication compiles to syntactically correct OpenCL") {
    import idealised.OpenCL._
    val kernel = idealised.OpenCL.KernelGenerator.makeCode(TypeInference(triangleVectorMultGlobalFused, Map()).toPhrase, 1, 1)
    println(kernel.code)

    val actualN = 64
    val inputVector = Array.tabulate(actualN)(id => id + 1.0f)
    val inputMatrix = Array.tabulate(actualN)(rowIndex => Array.tabulate(rowIndex + 1)(colIndex => if(colIndex == rowIndex) 1.0f else 0.0f))

    val kernelFun = kernel.as[ScalaFunction `(` Array[Array[Float]] `,` Array[Float] `)=>` Array[Float]]

    val (output, time) = kernelFun(inputMatrix `,` inputVector)

    output.foreach(x => print(x))
  }

  test("Parallel OpenCL triangle vector partial multiplication (padding the row up to vector) (PLDI '19 submission listing 5)") {
    import idealised.OpenCL._
    val actualN = 256
    val f: Expr[DataType -> (DataType -> DataType)] = {

      val N:ArithExpr = actualN
      val SPLIT_SIZE = Cst(32)
      fun(DepArrayType(N, i => ArrayType(i + 1, float)))(triangle =>
        fun(ArrayType(N, float))(vector =>
          depMapWorkgroup.withIndex(dFun(rowIndex => fun(row =>
            zip(pad(0, N - rowIndex - 1, 0.0f, row), vector) :>> split(SPLIT_SIZE) :>> mapLocal(reduceSeq(multSumAcc, 0.0f))
          )), triangle)
        )
      )
    }
    val kernel = idealised.OpenCL.KernelGenerator.makeCode(TypeInference(f, Map()).toPhrase, 1, 1)
    println(kernel.code)

    val inputVector = Array.tabulate(actualN)(id => id + 1.0f)
    val inputMatrix = Array.tabulate(actualN)(rowIndex => Array.tabulate(rowIndex + 1)(colIndex => if(colIndex == rowIndex) 1.0f else 0.0f))

    val kernelFun = kernel.as[ScalaFunction `(` Array[Array[Float]] `,` Array[Float] `)=>` Array[Float]]

    val (output, time) = kernelFun(inputMatrix `,` inputVector)

    output.grouped(actualN).foreach(row => {
      row.foreach(println)
      println()
    })

    println(time)
  }
}
