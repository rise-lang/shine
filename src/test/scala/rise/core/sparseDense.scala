package rise.core

import rise.core.TypeLevelDSL._
import rise.core.TypedDSL._
import rise.core.primitives._
import rise.core.semantics.NatData
import rise.core.types._
import rise.openCL.primitives._
import util.Execute


class sparseDense extends test_util.Tests {

  def csrMatrix(rows: Nat, cols:Nat, ns: NatCollection, et:ScalarType):DataType =
    rows `*.` (i => ((ns `@` (i+1)) - (ns `@` i)) `.` (et x IndexType(cols)))

  def wrappedCsrMatrix(rows: Nat, cols: Nat, et:ScalarType): Type =
    NatCollection ** (ns => csrMatrix(rows, cols, ns, et))

  def dense_to_sparse(n:Nat, m:Nat):ToBeTyped[Expr] = fun(n `.` m `.` f32)(array => {
    def pred = fun(x => x =/= l(0.0f))

    (toMem(array |>
      map(fun(row => map(pred)(row) |> count |> indexAsNat))
      |> scanSeqInclusive(fun(x => fun(y => x + y)))(Literal(NatData(0)))
    ) |> fun(offs => liftNats(offs)(depFun((offs:NatCollection) =>
      dpairNats(offs)(toDepArray(array) |>
        depMapSeq(depFun((rowIdx:Nat) => fun(row =>
          which(map(pred)(row))((offs `@` (rowIdx + 1)) - (offs `@` rowIdx))
            |> mapSeq(fun(nnzIdx => pair(row `@` nnzIdx)(nnzIdx)))
        )))))
    )))::(wrappedCsrMatrix(n, m, f32))
  })

  def sparse_to_dense(n: Nat, m: Nat) = fun(wrappedCsrMatrix(n, m, f32))(f = matrixWrap => {
    dmatchNats(matrixWrap)(depFun((ns: NatCollection) => fun(csrMatrix(n, m, ns, f32))(matrix =>
      matrix |> depMapSeq(depFun((i: Nat) => fun(row =>
        (row :: (((ns `@` (i + 1)) - (ns `@` i)) `.` (f32 x IndexType(m)))) |>
          reduceSeq(fun(acc => fun(y => updateAtIdx(acc)(y._2)(y._1))))(generate(fun(IndexType(m))(_ => l(0.0f))) |> mapSeq(fun(x => x)))
          |> mapSeq(fun(x => x))
      ))) |> unDepArray))) ::(n `.` m `.` f32)
  })


  def dense_to_sparse_ocl_1(n:Nat, m:Nat):ToBeTyped[Expr] = fun(n `.` m `.` f32)(array => {
    def pred = fun(x => x =/= l(0.0f))

    (array |>
      map(fun(row => map(pred)(row) |> count |> indexAsNat))
      |> scanSeqInclusive(fun(x => fun(y => x + y)))(Literal(NatData(0)))
    )::((n+1) `.` NatType)
  })

  def dense_to_sparse_ocl_2(n:Nat, m:Nat):ToBeTyped[Expr] = fun((n + 1) `.` NatType)(offs => fun(n `.` m `.` f32)(array => {
    def pred = fun(x => x =/= l(0.0f))

    liftNats(offs)(depFun((offs: NatCollection) =>
      dpairNats(offs)(toDepArray(array) |>
        depMapSeq(depFun((rowIdx:Nat) => fun(row =>
        oclWhich(row |> map(pred))((offs `@` (rowIdx + 1)) - (offs `@` rowIdx))
          |> oclToMem(AddressSpace.Global) |> mapSeq(fun(nnzIdx => pair(row `@` nnzIdx)(nnzIdx)))
        )
        )))))
  }))

  val dense_mv = depFun((n:Nat) => depFun((m:Nat) => fun(n `.` m `.` f32)(matrix => fun(m `.` f32)(vector => {
    matrix |>
      mapSeq(fun(row => zip(row)(vector) |>
        reduceSeq(fun(acc => fun(pair => acc + (pair._1 * pair._2))))(l(0.0f))
      ))
  }))))

  val sparse_mv = depFun((n:Nat) => depFun((m:Nat) => fun(wrappedCsrMatrix(n, m, f32))(matrixWrap => fun(m `.`f32)(vector => {
    dmatchNats(matrixWrap)(depFun((ns:NatCollection) => fun(csrMatrix(n, m, ns, f32))(matrix =>
      matrix |> depMapSeq(depFun((i:Nat) => fun(row =>
        (row::(((ns `@` (i+1)) - (ns `@` i)) `.` (f32 x IndexType(m)))) |>
          reduceSeq(fun(acc => fun(y => acc + (y._1 * (vector `@` y._2)))))(l(0.0f))
      )))|> unDepArray)))
  }))))


  val souped_up =  depFun((n:Nat) => depFun((m:Nat) => fun(wrappedCsrMatrix(n, m, f32))(matrixWrap => fun(m `.` f32)(vector => {
    dmatchNats(matrixWrap)(depFun((ns:NatCollection) => fun(csrMatrix(n, m, ns, f32))(matrix =>
      matrix |> depMapSeq(depFun((i:Nat) => fun(row =>
        (row::(((ns `@` (i+1)) - (ns `@` i)) `.` (f32 x IndexType(m)))) |>
          reduceSeq(fun(acc => fun(y => updateAtIdx(acc)(y._2)(y._1))))(generate(fun(IndexType(m))(_ => l(0.0f))) |> mapSeq(fun(x => x)))
          |> mapSeq(fun(x => x))
      ))) |> unDepArray |> mapSeq(fun(row => zip(row)(vector) |>
        reduceSeq(fun(acc => fun(pair => acc + (pair._1 * pair._2))))(l(0.0f))
      ))
    )))
  }))))

  val souped_up2=  depFun((n:Nat) => depFun((m:Nat) => fun(wrappedCsrMatrix(n, m, f32))(matrixWrap => fun(m `.` f32)(vector => {
    dmatchNats(matrixWrap)(depFun((ns:NatCollection) => fun(csrMatrix(n, m, ns, f32))(matrix =>
      matrix |> depMapSeq(depFun((i:Nat) => fun(row =>
        (row::(((ns `@` (i+1)) - (ns `@` i)) `.` (f32 x IndexType(m)))) |>
          reduceSeq(fun(acc => fun(y => updateAtIdx(acc)(y._2)(y._1))))(generate(fun(IndexType(m))(_ => l(0.0f))) |> mapSeq(fun(x => x)))
          |> mapSeq(fun(x => x))
      ))) |> depMapSeq(depFun((_:Nat) => fun(row => zip(row)(vector) |>
        reduceSeq(fun(acc => fun(pair => acc + (pair._1 * pair._2))))(l(0.0f))
      ))) |> unDepArray
    )))
  }))))

  val souped_up3 =  depFun((n:Nat) => depFun((m:Nat) => fun(wrappedCsrMatrix(n, m, f32))(matrixWrap => fun(m `.` f32)(vector => {
    dmatchNats(matrixWrap)(depFun((ns:NatCollection) => fun(csrMatrix(n, m, ns, f32))(matrix =>
      matrix |> depMapSeq(depFun((i:Nat) => fun(row =>
        (row::(((ns `@` (i+1)) - (ns `@` i)) `.` (f32 x IndexType(m)))) |>
          reduceSeq(fun(acc => fun(y => updateAtIdx(acc)(y._2)(y._1))))(generate(fun(IndexType(m))(_ => l(0.0f))) |> mapSeq(fun(x => x)))
          |> mapSeq(fun(x => x))
      )) >> depFun((_:Nat) => fun(row => zip(row)(vector) |>
        reduceSeq(fun(acc => fun(pair => acc + (pair._1 * pair._2))))(l(0.0f))
      ))) |> unDepArray
    )))
  }))))

  val souped_up4=  depFun((n:Nat) => depFun((m:Nat) => fun(wrappedCsrMatrix(n, m, f32))(matrixWrap => fun(m `.` f32)(vector => {
    dmatchNats(matrixWrap)(depFun((ns:NatCollection) => fun(csrMatrix(n, m, ns, f32))(matrix =>
      matrix |> depMapSeq(depFun((i:Nat) => fun(row =>
        (row::(((ns `@` (i+1)) - (ns `@` i)) `.` (f32 x IndexType(m)))) |>
          reduceSeq(fun(acc => fun(y => updateAtIdx(acc)(y._2)(y._1))))(generate(fun(IndexType(m))(_ => l(0.0f))) |> mapSeq(fun(x => x)))
          |> mapSeq(fun(x => x))
          |> fun(x => zip(x)(vector))
          |> reduceSeq(fun(acc => fun(pair => acc + (pair._1 * pair._2))))(l(0.0f))
      ))) |> unDepArray
    )))
  }))))

  val souped_up5 =  depFun((n:Nat) => depFun((m:Nat) => fun(wrappedCsrMatrix(n, m, f32))(matrixWrap => fun(m `.` f32)(vector => {
    dmatchNats(matrixWrap)(depFun((ns:NatCollection) => fun(csrMatrix(n, m, ns, f32))(matrix =>
      matrix |> depMapSeq(depFun((i:Nat) => fun(row =>
        (row::(((ns `@` (i+1)) - (ns `@` i)) `.` (f32 x IndexType(m)))) |>
          reduceSeq(fun(acc => fun(y => updateAtIdx(acc)(y._2)(y._1))))(generate(fun(IndexType(m))(_ => l(0.0f))) |> mapSeq(fun(x => x)))
          |> fun(row => zip(row |> mapSeq(fun(x => x)))(vector))
          |> reduceSeq(fun(acc => fun(pair => acc + (pair._1 * pair._2))))(l(0.0f))
      ))) |> unDepArray
    )))
  }))))

  val souped_up6 =  depFun((n:Nat) => depFun((m:Nat) => fun(wrappedCsrMatrix(n, m, f32))(matrixWrap => fun(m `.` f32)(vector => {
    dmatchNats(matrixWrap)(depFun((ns:NatCollection) => fun(csrMatrix(n, m, ns, f32))(matrix =>
      matrix |> depMapSeq(depFun((i:Nat) => fun(row =>
        (row::(((ns `@` (i+1)) - (ns `@` i)) `.` (f32 x IndexType(m))))
          |> fun(row => zip(row |> reduceSeq(fun(acc => fun(y => updateAtIdx(acc)(y._2)(y._1))))(generate(fun(IndexType(m))(_ => l(0.0f))) |> mapSeq(fun(x => x)))
                |> mapSeq(fun(x => x)))(vector)
          )
          |> reduceSeq(fun(acc => fun(pair => acc + (pair._1 * pair._2))))(l(0.0f))
      ))) |> unDepArray
    )))
  }))))

  val souped_up7 =  depFun((n:Nat) => depFun((m:Nat) => fun(wrappedCsrMatrix(n, m, f32))(matrixWrap => fun(m `.` f32)(vector => {
    dmatchNats(matrixWrap)(depFun((ns:NatCollection) => fun(csrMatrix(n, m, ns, f32))(matrix =>
      matrix |> depMapSeq(depFun((i:Nat) => fun(row =>
        (row::(((ns `@` (i+1)) - (ns `@` i)) `.` (f32 x IndexType(m))))
          |> zip(
            row |> reduceSeq(fun(acc => fun(y => updateAtIdx(acc)(y._2)(y._1))))(generate(fun(IndexType(m))(_ => l(0.0f))) |> mapSeq(fun(x => x)))
                |> mapSeq(fun(x => x))
        )(vector)
          |> reduceSeq(fun(acc => fun(pair => acc + (pair._1 * pair._2))))(l(0.0f))
      ))) |> unDepArray
    )))
  }))))


  // sparse_to_dense >> dense_mv >> dense_to_sparse

  test("Dense to sparse") {
    val inferred: Expr = TDSL.infer(depFun((n:Nat) => depFun((m:Nat) => dense_to_sparse(n, m))))
    println(inferred)
    print(inferred.t)
    val cFun = util.gen.CProgram(inferred, "dense_to_sparse")

    val testCode =
      s"""
        |
        |#include<stdlib.h>
        |#include<stdio.h>
        |#include<stdint.h>
        |#include<string.h>
        |${cFun.code}
        |
        |struct Pair {
        |  float fst;
        |  int snd;
        |};
        |
        |int main() {
        |  int n = 8;
        |  int m = 8;
        |
        |  float matrix[n * m];
        |  for (int i = 0; i < n; i++) {
        |    for (int j = 0; j < m; j++) {
        |      int idx = i + j * n;
        |      float value = (idx % 2 == 0 || j == 3) ? 0.0:((float)j);
        |      matrix[idx] = value;
        |    }
        |  }
        |
        |  uint8_t output[sizeof(float) * n * m + sizeof(uint32_t) * 2];
        |  dense_to_sparse(output, n, m, matrix);
        |
        |  uint32_t* idx_output = (uint32_t*)output;
        |  struct Pair* data_output = (struct Pair*)(idx_output + 2 + n);
        |  for (int i = 0; i < idx_output[0] - 1; i++) {
        |    int len = idx_output[i + 2] - idx_output[i + 1];
        |    for (int j = 0; j < len; j++) {
        |      int offset = idx_output[i + 1] + j;
        |      struct Pair data = data_output[offset];
        |      if (data.snd != 2*j + 1 || data.fst != (float)i) {
        |        return 1;
        |      }
        |    }
        |  }
        |
        |
        |  return 0;
        |}""".stripMargin

        Execute(testCode)
  }

  test("Dense to sparse ocl") {
    val e1 = depFun((n: Nat) => depFun((m:Nat) => dense_to_sparse_ocl_1(n, m)))
    val e2 = depFun((n: Nat) => depFun((m:Nat) => dense_to_sparse_ocl_2(n, m)))

   //util.gen.CProgram(e1, "dense_to_sparse_1")
    util.gen.OpenCLKernel(e2, "dense_to_sparse_2")
  }

  test("Sparse to dense") {
    val inferred: Expr = TDSL.infer(depFun((n:Nat) => depFun((m:Nat) => sparse_to_dense(n, m))))
    println(inferred)
    print(inferred.t)
    util.gen.CProgram(inferred, "sparse_to_dense")
  }

  test("there and back again") {
    val e =  depFun((n: Nat) => depFun((m: Nat)=> fun(n `.` m `.` f32)(array =>
      (array |> dense_to_sparse(n,m))::wrappedCsrMatrix(n, m, f32) |> toMem |> sparse_to_dense(n, m)
    )))
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

  test("Sparse matrix vector product") {
    val e = depFun((n:Nat) => depFun((m:Nat) => fun(wrappedCsrMatrix(n, m, f32))(matrixWrap => fun(m `.`f32)(vector => {
      dmatchNats(matrixWrap)(depFun((ns:NatCollection) => fun(csrMatrix(n, m, ns, f32))(matrix =>
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
    val e = depFun((n:Nat) => depFun((m:Nat) => depFun((k:Nat)=> fun(n `.` m `.` f32)(matrix1 => fun(m `.` k `.` f32)(matrix2 => {
      matrix2 |> transpose |> mapSeq(fun(row2 => matrix1 |> mapSeq(fun(row1 =>
        zip(row1)(row2) |> reduceSeq(fun(acc => fun(pair => acc + (pair._1 * pair._2))))(l(0.0f))))
      ))
    })))))

    val inferred: Expr = TDSL.infer(e)
    println(inferred)
    print(inferred.t)
    util.gen.CProgram(inferred, "dense_dense_mm")
 }

  test("Sparse matrix dense matrix product") {
    val e = depFun((n:Nat) => depFun((m:Nat) => depFun((k:Nat) => fun(wrappedCsrMatrix(n, m, f32))(matrixWrap => fun(m `.` k `.`f32)(dense => {
      dense |> transpose |> mapSeq(fun(vector =>
        dmatchNats(matrixWrap)(depFun((ns:NatCollection) => fun(csrMatrix(n, m, ns, f32))(matrix =>
          (matrix |> depMapSeq(depFun((i:Nat) => fun(row =>
            (row::(((ns `@` (i+1)) - (ns `@` i)) `.` (f32 x IndexType(m)))) |>
              reduceSeq(fun(acc => fun(y => acc + (y._1 * (vector `@` y._2)))))(l(0.0f))
          )))) |> unDepArray)))
      ))
    })))))

    val inferred: Expr = TDSL.infer(e)
    println(inferred)
    print(inferred.t)
    util.gen.CProgram(inferred, "sparse_dense_mm")
  }
}