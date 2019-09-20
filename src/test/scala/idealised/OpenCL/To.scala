package idealised.OpenCL

import idealised.util.gen
import lift.core.primitives._
import lift.core.DSL._
import lift.core.types.float
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
}
