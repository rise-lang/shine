package FSmooth

import FSmooth.DSL._
import FSmooth.MSmooth._
import elevate.core.strategies.basic.{id, repeat}
import elevate.core.strategies.traversal.oncetd
import elevate.core.{FSmooth, Strategy}
import elevate.fsmooth.rules._
import elevate.fsmooth.traversal._
import elevate.lift.strategies

class ExamplesFromICFP2019Paper extends idealised.util.Tests {

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

  test("Example5: Matrix Transpose") {
    val e = fun(M => matrixTranspose(matrixTranspose(M)))
    def normalize: Strategy[FSmooth] => Strategy[FSmooth] =
      s => repeat(oncetd(s))

    println(normalize(buildGet <+ lengthBuild <+ letPartialEvaluation <+ conditionalPartialEvalution <+ conditionApplication <+ letApplication <+ funToLet <+ letFission <+ letInitDuplication).apply(e))
    println("--")
    println(e)
  }

  test("Free variables") {
    val y = Variable(freshName("y"))
    val z = Variable(freshName("z"))
    assert( Differentiation.fvs( fun(x => x) ) == Seq() )
    assert( Differentiation.fvs( fun(x => x + y) ) == Seq(y) )
    assert( Differentiation.fvs( fun(x => z + x + y) ) == Seq(z, y) )
  }
}
