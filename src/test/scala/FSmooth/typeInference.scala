package FSmooth

import FSmooth.DSL._
import FSmooth.MSmooth._

class typeInference extends test_util.Tests {

  test("Type inference MSmooth vector constructs") {
    println("vectorRange = " + TypeInference.infer(vectorRange))

    println("vectorFill = " + TypeInference.infer(vectorFill))

    println("vectorHot = " + TypeInference.infer(vectorHot))

    println("vectorMap = " + TypeInference.infer(vectorMap))

    println("vectorMap2 = " + TypeInference.infer(vectorMap2))

    println("vectorZip = " + TypeInference.infer(vectorZip))

    println("vectorAdd = " + TypeInference.infer(vectorAdd))

    println("vectorEMul = " + TypeInference.infer(vectorEMul))

    println("vectorSMul = " + TypeInference.infer(vectorSMul))

    println("vectorSum = " + TypeInference.infer(vectorSum))

    println("vectorDot = " + TypeInference.infer(vectorDot))

    println("vectorNorm = " + TypeInference.infer(vectorNorm))

//    println("vectorSlice = " + TypeInference.infer(vectorSlice))

    println("vectorToMatrix = " + TypeInference.infer(vectorToMatrix))

    println("vectorOutProd = " + TypeInference.infer(vectorOutProd))
  }

  test("Type inference MSmooth matrix constructs") {
    println("matrixRows = " + TypeInference.infer(matrixRows))

    println("matrixCols = " + TypeInference.infer(matrixCols))

    println("matrixZeros = " + TypeInference.infer(matrixZeros))

    println("matrixOnes = " + TypeInference.infer(matrixOnes))

    println("matrixEye = " + TypeInference.infer(matrixEye))

    println("matrixHot = " + TypeInference.infer(matrixHot))

    println("matrixMap = " + TypeInference.infer(matrixMap))

    println("matrixMap2 = " + TypeInference.infer(matrixMap2))

    println("matrixAdd = " + TypeInference.infer(matrixAdd))

    println("matrixTranspose = " + TypeInference.infer(matrixTranspose))

    println("matrixMult = " + TypeInference.infer(matrixMult))

    println("matrixTrace = " + TypeInference.infer(matrixTrace))
  }

  test("Example1: uMv^T") {
    val f = fun( (u, M, v) =>
      let( vectorToMatrix(u) ).beIn(um =>
      let( matrixTranspose(vectorToMatrix(v)) ).beIn(vt =>
      let( matrixMult(um, matrixMult(M, vt)) ).beIn(m =>
      m.get(idx(0)).get(idx(0))
      )))
    )
    println(f)
    println("f = " + TypeInference.infer(f))
  }

  test("matrixEye typechecks") {
    val expr = matrixEye
    println(expr)
    TypeInference.infer(expr)
  }

  test("Free variables") {
    val y = Variable(freshName("y"))
    val z = Variable(freshName("z"))
    assert( Differentiation.fvs( fun(x => x) ) == Seq() )
    assert( Differentiation.fvs( fun(x => x + y) ) == Seq(y) )
    assert( Differentiation.fvs( fun(x => z + x + y) ) == Seq(z, y) )
  }
}
