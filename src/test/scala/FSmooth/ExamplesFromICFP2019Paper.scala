package FSmooth

import FSmooth.DSL._
import FSmooth.MSmooth._

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
    println("matrixCols = " + TypeInference.infer(matrixCols))

    println("matrixRows = " + TypeInference.infer(matrixRows))

    println("matrixTranspose = " + TypeInference.infer(matrixTranspose))

    println("matrixMult = " + TypeInference.infer(matrixMult))
  }

  test("Example1: uMv^T") {
    val f = let(fun( (u, M, v) =>
        let( vectorToMatrix(u) ).beIn(um =>
        let( matrixTranspose(vectorToMatrix(v)) ).beIn(vt =>
        let( matrixMult(um, matrixMult(M, vt)) ).beIn(m =>
        m.get(idx(0)).get(idx(0))
        )))
    )).beIn(f =>
      f
    )
    println(f)
    println("f = " + TypeInference.infer(f))
  }
}
