package idealised.OpenCL

import idealised.util.gen
import lift.core.primitives._
import lift.core.DSL._
import lift.core.types._
import lift.core.types.AddressSpace._
import lift.OpenCL.primitives._

class To extends idealised.util.TestsWithExecutor {

  test("To creates OpenCLNew with appropriate data type: private mem with two mapLocal nesting two mapSeq") {
    val e = nFun((m, n, o, p) =>
              fun(m`.`n`.`o`.`p`.`float)(xs =>
                xs
                |> toPrivateFun(mapLocal(1) (mapLocal(0) (mapSeq (mapSeq (fun(x => x))))))
                |> mapLocal(1) (mapLocal(0) (mapSeq (mapSeq (fun(x => x))))) ))

    gen.OpenCLKernel(e(4)(4)(2)(2))

    //Expected: Outer-most 2 dimensions have size of the ceiling of dividing by get_local_size(dim).
    // Inner dimensions stay the same.
  }

  test("To creates OpenCLNew with appropriate data type: private mem with two mapGlobal") {
    val e = nFun((m, n) =>
      fun(m`.`n`.`float)(xs =>
        xs
          |> toPrivateFun(mapGlobal(1) (mapGlobal(0) (fun(x => x))))
          |> mapGlobal(1) (mapGlobal(0) (fun(x => x)))))

    gen.OpenCLKernel(e(4)(4))

    //Expected: Dimensions have size of the ceiling of dividing by get_global_size(dim)
  }


  test("To creates OpenCLNew with appropriate data type: private mem with mapLocal over pair of mapLocal") {
    val e = nFun((m, n) =>
      fun(m`.`n`.`float)(xs =>
        xs
          |> toPrivateFun(mapLocal(1) (fun(x => pair(x |> mapLocal(0) (fun(x => x)), x |> mapLocal(0) (fun(x => x))))))
          |> mapLocal(1) (fun(t => pair(t._1 |> mapLocal(0) (fun(x => x)), t._2 |> mapLocal(0) (fun(x => x)))))))

    gen.OpenCLKernel(e(4)(8))

    //Expected: Outer dimension has size of the ceiling of dividing by get_local_size(dim).
    // In RecordType dimensions are adapted as well.
  }

  test("To creates OpenCLNew with appropriate data type: local mem with mapLocal over pair of mapLocal") {
    val e = nFun((m, n) =>
      fun(m`.`n`.`float)(xs =>
        xs
          |> toLocalFun(mapLocal(1) (fun(x => pair(x |> mapLocal(0) (fun(x => x)), x |> mapLocal(0) (fun(x => x))))))
          |> mapLocal(1) (fun(t => pair(t._1 |> mapLocal(0) (fun(x => x)), t._2 |> mapLocal(0) (fun(x => x)))))))

    gen.OpenCLKernel(e(4)(8))

    //Expected: No changes.
  }

  test("oclReduceSeq allocates memory with appropriate data type:" +
    "private memory accumulator with two mapLocal nesting two mapSeq") {

    val zeros = nFun(n1 => nFun(n2 => nFun(n3 => nFun(n4 =>
      generate(fun(IndexType(n4))(_ =>
        generate(fun(IndexType(n3))(_ =>
          generate(fun(IndexType(n2))(_ =>
            generate(fun(IndexType(n1))(_ => l(0.0f)))))))))))))

    val e = nFun((k, m, n, o, p) =>
      fun(k`.`m`.`n`.`o`.`p`.`float)(xs =>
        xs
          |> oclReduceSeq (Private) (fun((x, y) =>
                zip (x) (y)
                  |> mapLocal(1) (fun(zippedDim4Row => zip (zippedDim4Row._1) (zippedDim4Row._2)
                    |> mapLocal(0) (fun(zippedDim3Row => zip (zippedDim3Row._1) (zippedDim3Row._2)
                      |> mapSeq (fun(zippedDim2Row => zip (zippedDim2Row._1) (zippedDim2Row._2)
                        |> mapSeq (fun(zippedDim1Row => zippedDim1Row._1 + zippedDim1Row._2))))))) )))
            (zeros (m) (n) (o) (p) |> mapLocal(1) (mapLocal(0) (mapSeq (mapSeq (fun(x => x))))))
          |> mapLocal(1) (mapLocal(0) (mapSeq (mapSeq (fun(x => x)))))))

    gen.OpenCLKernel(LocalSize((2, 2, 1)), GlobalSize((8, 8, 1)))(e(8)(4)(4)(2)(2), "KERNEL")

    //Expected: Outer-most 2 dimensions have size of the ceiling of dividing by get_local_size(dim).
    // Inner dimensions stay the same.
  }
}
