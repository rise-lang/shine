package FSmooth

import FSmooth.DSL._

//noinspection DuplicatedCode
object MSmooth {
  // vector constructs
  def vectorRange = fun(n =>
    build(n, fun(i => i)) )

  def vectorFill = fun( (n, e) =>
    build(n, fun(i => e)) )

  def vectorHot = fun( (n, i) =>
      Application(build, Seq(n, fun(j => Conditional(i `=:=` j, scalar(1.0), scalar(0.0)) ))))
//    build(n, fun(j =>
//      `if` (i `=:=` j) `then` scalar(1.0) `else` scalar(0.0))) )

  def vectorMap = fun( (v, f) =>
    build(len(v), fun(i => f(v.get(i)))) )

  def vectorMap2 = fun( (v1, v2, f) =>
    build(len(v1), fun(i => f(v1.get(i), v2.get(i)))) )

  def vectorZip = fun( (v1, v2) =>
    vectorMap2(v1, v2, pair) )

  def vectorAdd = fun( (v1, v2) =>
    vectorMap2(v1, v2, fun(_ + _)) )

  def vectorEMul = fun( (v1, v2) =>
    vectorMap2(v1, v2, fun(_ * _)) )

  def vectorSMul = fun( (v1, s) =>
    vectorMap(v1, fun(_ * s)) )

  def vectorSum = fun(v =>
    ifold(fun( (s, i) => s + v.get(i) ), scalar(0.0), len(v)) )

  def vectorDot = fun( (v1, v2) =>
    vectorSum(vectorEMul(v1, v2)) )

  def vectorNorm = fun( v =>
    sqrt(vectorDot(v, v)) )

  def vectorSlice = fun( (v, s, e) =>
    build(e - s + card(1), fun(i => v.get(i + s))) )

  def vectorToMatrix = fun(v =>
    build(card(1), fun(i => v)) )

  def vectorOutProd = fun( (v1, v2) =>
    let(vectorToMatrix(v1)).beIn(m1 =>
    let(vectorToMatrix(v2)).beIn(m2 =>
    let(matrixTranspose(m2)).beIn(m2T =>
      matrixMult(m1, m2T) ))) )

  // matrix constructs
  def matrixRows = fun(m =>
    len(m) )

  def matrixCols = fun(m =>
    len(m.get(idx(0))) )

  def matrixZeros = fun( (r, c) =>
    build(r, fun(i => vectorFill(c, scalar(0.0)))) )

  def matrixOnes = fun( (r, c) =>
    build(r, fun(i => vectorFill(c, scalar(1.0)))) )

  def matrixEye = fun(n =>
    //build(n, fun(i => vectorHot(n, i)))
    Application(build, Seq(n, fun(i => Application(vectorHot, Seq(n, i)))))
  )

  def matrixHot = fun( (n, m, r, c) =>
    build(n, fun(i =>
      build(m, fun(j =>
        `if` ((i `=:=` r) `&&` (j `=:=` c))
          `then` scalar(1.0)
          `else` scalar(0.0) )) )) )

  def matrixMap = fun( (m, f) =>
    build(len(m), fun(i => f(m.get(i)))) )

  def matrixMap2 = fun( (m1, m2, f) =>
    build(len(m1), fun(i => f(m1.get(i), m2.get(i)))) )

  def matrixAdd = fun( (m1, m2) =>
    matrixMap2(m1, m2, vectorAdd) )

  def matrixTranspose = fun(m =>
    build(matrixCols(m), fun(i =>
      build(matrixRows(m), fun(j =>
        m.get(j).get(i) )) )) )

  def matrixMult: Expr = fun( (m1, m2) =>
    let(matrixTranspose(m2)).beIn(m2T =>
      build(matrixRows(m1), fun(i =>
        build(matrixCols(m2), fun(j =>
          vectorDot(m1.get(i), m2T.get(j)) )) )) ) )

  def matrixTrace = fun(m =>
    ifold(fun( (s, i) => s + m.get(i).get(i) ), scalar(0.0), len(m)) )

  def matrixZip: Expr = ???
}
