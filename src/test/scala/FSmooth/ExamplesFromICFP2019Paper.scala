package FSmooth

import FSmooth.DSL._
import FSmooth.ScalarFunctionConstants.`*`
import FSmooth.VectorFunctionConstants.{length => len, _}

class ExamplesFromICFP2019Paper extends idealised.util.Tests {

  test("Example1: uMv^T") {

    def vectorToMatrix =
      fun(v => build(card(1))(fun(i => v)))

    def vectorMap2: Expr = fun( (v1, v2, f) =>
      build(len(v1))(fun(i => f(v1.get(i), v2.get(i)))))

    def vectorEMul: Expr = fun( (v1, v2) =>
      vectorMap2(v1)(v2)(`*`))

    def vectorSum: Expr = fun(v =>
      ifold(fun( (s, i) => s + v.get(i) ))(scalar(0.0))(len(v)))

    def vectorDot: Expr = fun( (v1, v2) =>
      vectorSum(vectorEMul(v1, v2)))

    def matrixCols: Expr = fun(m =>
      len(m.get(idx(0))))

    def matrixRows: Expr = fun(m =>
      len(m))

    def matrixTranspose = fun(m =>
      build(matrixCols(m))(fun(i =>
        build(matrixRows(m))(fun(j =>
          m.get(j).get(i)
        ))
      ))
    )

    def matrixMult: Expr = fun( (m1, m2) =>
      let(matrixTranspose(m2)).beIn(m2T =>
        build(matrixRows(m1))(fun(i =>
          build(matrixCols(m2))(fun(j =>
            vectorDot(m1.get(i))(m2T.get(j))
          ))
        ))
      )
    )

    let(fun( (u, M, v) =>
        let( vectorToMatrix(u) ).beIn(um =>
        let( matrixTranspose(vectorToMatrix(v)) ).beIn(vt =>
        let( matrixMult(um)(matrixMult(M)(vt)) ).beIn(m =>
        m.get(idx(0)).get(idx(0))
        )))
    )).beIn(f =>
      f
    )

  }
}
