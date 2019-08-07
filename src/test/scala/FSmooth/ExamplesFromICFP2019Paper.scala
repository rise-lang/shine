package FSmooth

import FSmooth.DSL._

class ExamplesFromICFP2019Paper extends idealised.util.Tests {

  test("Example1: uMv^T") {

    def vectorToMatrix =
      fun(v => build(card(1), fun(i => v)))
    println("vectorToMatrix = " + TypeInference.infer(vectorToMatrix))

    def vectorMap2: Expr = fun( (v1, v2, f) =>
      build(len(v1), fun(i => f(v1.get(i), v2.get(i)))) )
    println("vectorMap2 = " + TypeInference.infer(vectorMap2))

    def vectorEMul: Expr = fun( (v1, v2) =>
      vectorMap2(v1, v2, fun( (x1, x2) => x1 * x2))
    )
    println("vectorEMul = " + TypeInference.infer(vectorEMul))

    def vectorSum: Expr = fun(v =>
      ifold(fun( (s, i) => s + v.get(i) ), scalar(0.0), len(v)) )
    println("vectorSum = " + TypeInference.infer(vectorSum))

    def vectorDot: Expr = fun( (v1, v2) =>
      vectorSum(vectorEMul(v1, v2)))
    println("vectorDot = " + TypeInference.infer(vectorDot))

    def matrixCols: Expr = fun(m =>
      len(m.get(idx(0))))
    println("matrixCols = " + TypeInference.infer(matrixCols))

    def matrixRows: Expr = fun(m =>
      len(m))
    println("matrixRows = " + TypeInference.infer(matrixRows))

    def matrixTranspose = fun(m =>
      build(matrixCols(m), fun(i =>
        build(matrixRows(m), fun(j =>
          m.get(j).get(i)
        ))
      ))
    )
    println("matrixTranspose = " + TypeInference.infer(matrixTranspose))

    def matrixMult: Expr = fun( (m1, m2) =>
      let(matrixTranspose(m2)).beIn(m2T =>
        build(matrixRows(m1), fun(i =>
          build(matrixCols(m2), fun(j =>
            vectorDot(m1.get(i), m2T.get(j))
          ))
        ))
      )
    )
    println("matrixMult = " + TypeInference.infer(matrixMult))

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
