package shine.OpenCL

import rise.core.DSL.Type._
import rise.core.DSL._
import rise.core.primitives._
import rise.core.types.AddressSpace._
import rise.core.types.DataType._
import rise.core.types._
import rise.openCL.DSL._
import rise.openCL.primitives.oclReduceSeq
import util.gen

class OclToMem extends test_util.Tests {
  val id = fun(x => x)
  val add1 = fun(x => x + lf32(1.0f))

  test("To creates OpenCLNew with appropriate data type: private mem with two mapLocal nesting two mapSeq") {
    val e = depFun((m: Nat, n: Nat, o: Nat, p: Nat) =>
              fun(m`.`n`.`o`.`p`.`f32)(xs =>
                xs
                |> toPrivateFun(mapLocal(1) (mapLocal(0) (mapSeq (mapSeq (fun(x => x))))))
                |> mapLocal(1) (mapLocal(0) (mapSeq (mapSeq (fun(x => x))))) ))

    val localSize = LocalSize((4, 2, 1))
    val globalSize = GlobalSize((4, 2, 1))
    gen.opencl.kernel(localSize, globalSize).fromExpr(e(4)(4)(2)(2))

    //Expected: Outer-most 2 dimensions have size of the ceiling of dividing by get_local_size(dim).
    // Inner dimensions stay the same.
  }

  test("To creates OpenCLNew with appropriate data type: private mem with two mapGlobal") {
    val e = depFun((m: Nat, n: Nat) =>
      fun(m`.`n`.`f32)(xs =>
        xs
          |> toPrivateFun(mapGlobal(1) (mapGlobal(0) (fun(x => x))))
          |> mapGlobal(1) (mapGlobal(0) (fun(x => x)))))

    val localSize = LocalSize((1, 1, 1))
    val globalSize = GlobalSize((4, 2, 1))
    gen.opencl.kernel(localSize, globalSize).fromExpr(e(4)(4))

    //Expected: Dimensions have size of the ceiling of dividing by get_global_size(dim)
  }


  test("To creates OpenCLNew with appropriate data type: private mem with mapLocal over pair of mapLocal") {
    val e = depFun((m: Nat, n: Nat) =>
      fun(m`.`n`.`f32)(xs =>
        xs
          |> toPrivateFun(mapLocal(1) (fun(x => makePair(x |> mapLocal(0) (fun(x => x)))(x |> mapLocal(0) (fun(x => x))))))
          |> mapLocal(1) (fun(t => makePair(t.`1` |> mapLocal(0) (fun(x => x)))(t.`2` |> mapLocal(0) (fun(x => x)))))))

    val localSize = LocalSize((4, 4, 1))
    val globalSize = GlobalSize((4, 8, 1))
    gen.opencl.kernel(localSize, globalSize).fromExpr(e(4)(8))

    //Expected: Outer dimension has size of the ceiling of dividing by get_local_size(dim).
    // In RecordType dimensions are adapted as well.
  }

  test("To creates OpenCLNew with appropriate data type: local mem with mapLocal over pair of mapLocal") {
    val e = depFun((m: Nat, n: Nat) =>
      fun(m`.`n`.`f32)(xs =>
        xs
          |> toLocalFun(mapLocal(1) (fun(x => makePair(x |> mapLocal(0) (fun(x => x)))(x |> mapLocal(0) (fun(x => x))))))
          |> mapLocal(1) (fun(t => makePair(t.`1` |> mapLocal(0) (fun(x => x)))(t.`2` |> mapLocal(0) (fun(x => x)))))))

    gen.opencl.kernel.fromExpr(e(4)(8))

    //Expected: No changes.
  }

  test("oclReduceSeq allocates memory with appropriate data type:" +
    "private memory accumulator with two mapLocal nesting two mapSeq") {

    val zeros = depFun((n1: Nat, n2: Nat, n3: Nat, n4: Nat) =>
      generate(fun(IndexType(n1))(_ =>
        generate(fun(IndexType(n2))(_ =>
          generate(fun(IndexType(n3))(_ =>
            generate(fun(IndexType(n4))(_ => lf32(0.0f))))))))))

    val e = depFun((k: Nat, m: Nat, n: Nat, o: Nat, p: Nat) =>
      fun(k `.` m `.` n `.` o `.` p `.` f32)(xs =>
        xs
          |> oclReduceSeq(Private)(fun((x, y) =>
          zip(x)(y)
            |> mapLocal(1)(fun(zippedDim4Row => zip(zippedDim4Row.`1`)(zippedDim4Row.`2`)
            |> mapLocal(0)(fun(zippedDim3Row => zip(zippedDim3Row.`1`)(zippedDim3Row.`2`)
            |> mapSeq(fun(zippedDim2Row => zip(zippedDim2Row.`1`)(zippedDim2Row.`2`)
            |> mapSeq(fun(zippedDim1Row => zippedDim1Row.`1` + zippedDim1Row.`2`))))))))))
        (zeros(m)(n)(o)(p) |> mapLocal(1)(mapLocal(0)(mapSeq(mapSeq(fun(x => x))))))
          |> mapLocal(1)(mapLocal(0)(mapSeq(mapSeq(fun(x => x)))))))

    val localSize = LocalSize((2, 2, 1))
    val globalSize = GlobalSize((8, 8, 1))
    gen.opencl.kernel(localSize, globalSize).fromExpr(e(8)(4)(4)(2)(2))

    //Expected: Outer-most 2 dimensions have size of the ceiling of dividing by get_local_size(dim).
    // Inner dimensions stay the same.
  }

  test("generate OpenCL code with array in private memory") {
    val e = depFun((n: Nat) => fun(ArrayType(n, ArrayType(3, f32)))(a =>
      a |> mapGlobal(toPrivateFun(mapSeq(add1)) >> mapSeq(id))
    ))

    val code = gen.opencl.kernel.asStringFromExpr(e)
    "for \\(".r.findAllIn(code).length shouldBe 1
  }

  test("toGlobal inside mapSeq") {
    val e = depFun((n: Nat) => fun(ArrayType(n, ArrayType(3, f32)))(a =>
      a |> mapSeq(toGlobalFun(mapSeq(add1)) >> mapSeq(id))
    ))

    //val code =
      gen.opencl.kernel.asStringFromExpr(e)
    // `3.f32` should be allocated, indexing should not depend on the outer loop
    // this assumes that there is only one thread running because there is no parallel map
  }

  test("toGlobal inside mapGlobal") {
    val e = depFun((n: Nat) => fun(ArrayType(n, ArrayType(3, f32)))(a =>
      a |> mapGlobal(toGlobalFun(mapSeq(add1)) >> mapSeq(id))
    ))

    //val code =
      gen.opencl.kernel.asStringFromExpr(e)
    // `max(nthreads, n).3.f32` should be allocated, indexing should depend on get_global_id(0)
  }

  test("toGlobal inside mapSeq inside toGlobal inside mapGlobal") {
    val e = depFun((n: Nat) => fun(ArrayType(n, ArrayType(3, f32)))(a =>
      a |> mapGlobal(
        toGlobalFun(mapSeq(toGlobalFun(add1) >> add1)) >>
        mapSeq(id)
      )
    ))

    //val code =
      gen.opencl.kernel.asStringFromExpr(e)
    // first inner loop, first memory write:
    // `max(nthreads, n).f32` should be allocated, indexing should depend on get_global_id(0)
    // first inner loop, second memory write:
    // `max(nthreads, n).3.f32` should be allocated, indexing should depend on get_global_id(0)
  }

  test("arithmetic expressions should be simplified when unrolling private arrays") {
    val e = fun(ArrayType(1, f32))(a =>
      a |> padCst(1)(1)(lf32(1.0f)) |> toPrivateFun(mapSeq(id)) |> mapSeq(id)
    )

    val code = gen.opencl.kernel.asStringFromExpr(e)
    "for \\(".r.findAllIn(code).length shouldBe 0
    " \\? ".r.findAllIn(code).length shouldBe 0
  }
}
