package rise.core

import rise.core.TypeLevelDSL._
import rise.core.TypedDSL._
import rise.core.primitives._
import rise.core.semantics.NatData
import rise.core.types._


class sparseDense extends test_util.Tests {
  test("Dense to sparse") {
    val e = depFun((n: Nat) => depFun((m: Nat)=> fun(n `.` m `.` f32)(array => {
      def pred = fun(x => x =/= l(0.0f))

      def offs = toMem(array |>
        map(fun(row => map(pred)(row) |> count |> indexAsNat))
        |> scanSeq(fun(x => fun(y => x + y)))(Literal(NatData(0)))
      )

      liftNats(offs)(depFun((offs:NatCollection) =>
        dpairNats(offs)(toDepArray(array) |>
          depMapSeq(depFun((rowIdx:Nat) => fun(row =>
            which(map(pred)(row))((offs `@` (rowIdx + 1)) - (offs `@` rowIdx))
              |> mapSeq(fun(nnzIdx => pair(nnzIdx)(row `@` nnzIdx)))
          ))))
      ))
    })))

    val inferred: Expr = TDSL.infer(e)
    println(inferred)
    print(inferred.t)
    util.gen.CProgram(inferred, "sparse_to_dense")
  }

  test("Dense matrix vector product") {
    val e = depFun((n:Nat) => depFun((m:Nat) => fun(n `.` m `.` f32)(matrix => fun(m `.` f32)(vector => {
      matrix |> mapSeq(fun(row => zip(row)(vector) |> reduceSeq(fun(acc => fun(pair => acc + (pair._1 * pair._2))))(l(0.0f))))
    }))))

    val inferred: Expr = TDSL.infer(e)
    println(inferred)
    print(inferred.t)
    util.gen.CProgram(inferred, "dense_mv")
  }

  def csrMatrix(rows: Nat, cols:Nat, ns: NatCollection):DataType = rows `*.` (i => ((ns `@` (i+1)) - (ns `@` i)) `.` (f32 x IndexType(cols)))
  def wrappedCsrMatrix(rows: Nat, cols: Nat): Type = NatCollection ** (ns => csrMatrix(rows, cols, ns))

  test("Sparse matrix vector product") {
    val e = depFun((n:Nat) => depFun((m:Nat) => fun(wrappedCsrMatrix(n, m))(matrixWrap => fun(m `.`f32)(vector => {
      dmatchNats(matrixWrap)(depFun((ns:NatCollection) => fun(csrMatrix(n, m, ns))(matrix =>
        (matrix |> depMapSeq(depFun((i:Nat) => fun(row =>
          (row::(((ns `@` (i+1)) - (ns `@` i)) `.` (f32 x IndexType(m)))) |>
              reduceSeq(fun(acc => fun(y => acc + (y._1 * (vector `@` y._2)))))(l(0.0f))
        )))) |> unDepArray)))
    }))))

    val inferred: Expr = TDSL.infer(e)
    println(inferred)
    print(inferred.t)
    util.gen.CProgram(inferred, "sparse_mv")
  }

  test("Dense matrix matrix product") {
    val e = depFun((n:Nat) => depFun((m:Nat) => fun(n `.` m `.` f32)(matrix1 => fun(n `.` m `.` f32)(matrix2 => {
      matrix1 |> mapSeq(fun(row1 => matrix2 |> mapSeq(fun(row2 =>
        zip(row1)(row2) |> reduceSeq(fun(acc => fun(pair => acc + (pair._1 * pair._2))))(l(0.0f))))
      ))
    }))))

    val inferred: Expr = TDSL.infer(e)
    println(inferred)
    print(inferred.t)
    util.gen.CProgram(inferred, "dense_mm")
 }

  test("Sparse matrix dense matrix product") {
    val e = depFun((n:Nat) => depFun((m:Nat) => fun(wrappedCsrMatrix(n, m))(matrixWrap => fun(n `.` m `.`f32)(dense => {

      dense |> mapSeq(fun(vector =>
        dmatchNats(matrixWrap)(depFun((ns:NatCollection) => fun(csrMatrix(n, m, ns))(matrix =>
          (matrix |> depMapSeq(depFun((i:Nat) => fun(row =>
            (row::(((ns `@` (i+1)) - (ns `@` i)) `.` (f32 x IndexType(m)))) |>
              reduceSeq(fun(acc => fun(y => acc + (y._1 * (vector `@` y._2)))))(l(0.0f))
          )))) |> unDepArray)))
      ))
    }))))

    val inferred: Expr = TDSL.infer(e)
    println(inferred)
    print(inferred.t)
    util.gen.CProgram(inferred, "sparse_mm")
  }
}