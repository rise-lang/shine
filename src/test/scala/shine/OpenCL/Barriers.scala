package shine.OpenCL

import rise.core.DSL.Type._
import rise.core.DSL._
import rise.core.primitives.{scatter, _}
import rise.core.types.AddressSpace._
import rise.core.types.DataType._
import rise.core.types._
import rise.openCL.DSL._
import rise.openCL.primitives.oclReduceSeq
import util.gen

// scalastyle:off org.scalastyle.scalariform.MultipleStringLiteralsChecker
class Barriers extends test_util.Tests {
  private val sum = oclReduceSeq(Private)(add)(lf32(0.0f))
  private val id = fun(x => x)

  // TODO? removing these barriers requires more fine grain analysis to
  // notice that the threads do not actually communicate through the memory
  ignore("1D mapLocal toLocal seq without thread sharing") {
    val e = depFun((n: Nat, m: Nat) => fun(n`.`m`.`f32)(in =>
      in |> mapWorkGroup(0)(
        mapLocal(0)(id) >>
        toLocal >>
        mapLocal(0)(id)
      ))
    )
    val code = gen.opencl.kernel.asStringFromExpr(e)
    "barrier".r.findAllIn(code).length shouldBe 0
  }

  // TODO? removing these barriers requires more fine grain analysis to
  // notice that the threads do not actually communicate through the memory
  ignore("1D mapLocal toGlobal seq without thread sharing") {
    val e = depFun((n: Nat, m: Nat) => fun(n`.`m`.`f32)(in =>
      in |> mapWorkGroup(0)(
        mapLocal(0)(id) >>
        toGlobal >>
        mapLocal(0)(id)
      ))
    )
    val code = gen.opencl.kernel.asStringFromExpr(e)
    "barrier".r.findAllIn(code).length shouldBe 0
  }

  test("1D mapLocal toLocal seq with thread sharing") {
    val e = depFun((n: Nat, m: Nat) => fun(n`.`m`.`f32)(in =>
      in |> mapWorkGroup(0)(
        mapLocal(0)(id) >>
        toLocal >>
        slide(3)(1) >>
        mapLocal(0)(sum)
      ))
    )
    val code = gen.opencl.kernel.asStringFromExpr(e)
    "barrier".r.findAllIn(code).length shouldBe 2
    """barrier\(CLK_LOCAL_MEM_FENCE\)""".r.findAllIn(code).length shouldBe 2
  }

  test("1D mapLocal toGlobal seq with thread sharing") {
    val e = depFun((n: Nat, m: Nat) => fun(n`.`m`.`f32)(in =>
      in |> mapWorkGroup(0)(
        mapLocal(0)(id) >>
        toGlobal >>
        slide(3)(1) >>
        mapLocal(0)(sum)
      ))
    )
    val code = gen.opencl.kernel.asStringFromExpr(e)
    "barrier".r.findAllIn(code).length shouldBe 2
    """barrier\(CLK_GLOBAL_MEM_FENCE\)""".r.findAllIn(code).length shouldBe 2
  }

  test("2D mapLocal toLocal seq with thread sharing") {
    val e = depFun((n: Nat, m: Nat, o: Nat, p: Nat) =>
      fun(n`.`m`.`o`.`p`.`f32)(in =>
        in |> mapWorkGroup(1)(mapWorkGroup(0)(
          mapLocal(1)(mapLocal(0)(id)) >>
          toLocal >>
          map(slide(3)(1)) >>
          mapLocal(1)(mapLocal(0)(sum))
        ))
    ))

    // expected:
    // parForWorkgroup
    //  parForWorkgroup
    //   parForLocal
    //    parForLocal
    //   barrier(LOCAL)
    //   parForLocal
    //    parForLocal
    //   barrier(LOCAL)

    val code = gen.opencl.kernel.asStringFromExpr(e)
    "barrier".r.findAllIn(code).length shouldBe 2
    """barrier\(CLK_LOCAL_MEM_FENCE\)""".r.findAllIn(code).length shouldBe 2
  }

  // FIXME: barriers might not be reached by all threads
  test("nested 1D mapLocal toLocal seq with thread sharing") {
    val e = depFun((n: Nat, m: Nat, o: Nat, p: Nat) =>
      fun(n`.`m`.`o`.`p`.`f32)(in =>
        in |> mapWorkGroup(1)(mapWorkGroup(0)(
          mapLocal(1)(
            mapLocal(0)(id) >>
            toLocal >>
            slide(3)(1) >>
            mapLocal(0)(sum)
          )
        ))
    ))

    // expected:
    // parForWorkgroup
    //  parForWorkgroup
    //   parForLocal
    //    parForLocal
    //    barrier(LOCAL)
    //    parForLocal
    //    barrier(LOCAL)

    val code = gen.opencl.kernel.asStringFromExpr(e)
    "barrier".r.findAllIn(code).length shouldBe 2
    """barrier\(CLK_LOCAL_MEM_FENCE\)""".r.findAllIn(code).length shouldBe 2
  }

  // FIXME: barriers might not be reached by all threads
  test("2D and nested 1D mapLocal toLocal seq with thread sharing") {
    val e = depFun((n: Nat, m: Nat, o: Nat, p: Nat) =>
      fun(n`.`m`.`o`.`p`.`f32)(in =>
        in |> mapWorkGroup(1)(mapWorkGroup(0)(
          mapLocal(1)(
            mapLocal(0)(id) >>
            toLocal >>
            slide(3)(1) >>
            mapLocal(0)(sum)
          ) >> toLocal >>
          map(slide(3)(1)) >>
          mapLocal(1)(mapLocal(0)(sum))
        ))
    ))

    // expected:
    // parForWorkgroup
    //  parForWorkgroup
    //   parForLocal
    //    parForLocal
    //    barrier(LOCAL)
    //    parForLocal
    //    barrier(LOCAL)
    //   parForLocal
    //    parForLocal

    val code = gen.opencl.kernel.asStringFromExpr(e)
    "barrier".r.findAllIn(code).length shouldBe 2
    """barrier\(CLK_LOCAL_MEM_FENCE\)""".r.findAllIn(code).length shouldBe 2
  }

  // TODO? array sizes cannot be adjusted for allocation
  ignore("1D zip mapLocal toLocal seq with thread sharing") {
    val e = depFun((n: Nat, m: Nat) => fun(n`.`m`.`f32)(in =>
      in |> mapWorkGroup(0)(fun(a =>
        zip(mapLocal(0)(id)(a))(mapLocal(0)(id)(a)) |>
        toLocal >>
        slide(3)(1) >>
        mapLocal(0)(map(fst) >> sum)
      ))
    ))
    val code = gen.opencl.kernel.asStringFromExpr(e)
    "barrier".r.findAllIn(code).length shouldBe 2
    """barrier\(CLK_LOCAL_MEM_FENCE\)""".r.findAllIn(code).length shouldBe 2
  }

  //// ---- PORTED FROM LIFT ----

  // FIXME: mapSeq creates writing race, prevent mapSeq outside of mapLocal?
  ignore("sequentialMapFollowsParallelWithLocalMemory") {
    val e = depFun((n: Nat) => fun(n`.`f32)(in =>
      in |> split(128) |> mapWorkGroup(
        mapLocal(id) >> toLocal >> mapSeq(id)
      ) |> join
    ))
    val code = gen.opencl.kernel.asStringFromExpr(e)
    println(code)
  }

  // FIXME: mapSeq creates writing race, prevent mapSeq outside of mapLocal?
  ignore("sequentialMapFollowsParallel") {
    val e = depFun((n: Nat) => fun(n`.`f32)(in =>
      in |> split(128) |> mapWorkGroup(
        mapLocal(id) >> toGlobal >> mapSeq(id)
      ) |> join
    ))
    val code = gen.opencl.kernel.asStringFromExpr(e)
    println(code)
  }

  // NOTE: Lift only inserts one barrier, I think two are necessary
  // FIXME: reduceSeq creates a writing race, prevent reduceSeq outside of mapLocal?
  ignore("sequentialReduceFollowsParallelWithLocalMemory") {
    val e = depFun((n: Nat) => fun(n`.`f32)(in =>
      in |> split(128) |> mapWorkGroup(
        mapLocal(id) >> toLocal >> oclReduceSeq(Private)(fun((acc, next) => acc + next))(lf32(0))
      )
    ))
    val code = gen.opencl.kernel.asStringFromExpr(e)
    println(code)
    "barrier".r.findAllIn(code).length shouldBe 2
    """barrier\(CLK_LOCAL_MEM_FENCE\)""".r.findAllIn(code).length shouldBe 2
  }

  // NOTE: Lift only inserts one barrier, I think two are necessary
  // FIXME: reduceSeq creates a writing race, prevent reduceSeq outside of mapLocal?
  ignore("sequentialReduceFollowsParallel") {
    val e = depFun((n: Nat) => fun(n`.`f32)(in =>
      in |> split(128) |> mapWorkGroup(
        mapLocal(id) >> toGlobal >> oclReduceSeq(Private)(fun((acc, next) => acc + next))(lf32(0))
      )
    ))
    val code = gen.opencl.kernel.asStringFromExpr(e)
    println(code)
    "barrier".r.findAllIn(code).length shouldBe 2
    """barrier\(CLK_LOCAL_MEM_FENCE\)""".r.findAllIn(code).length shouldBe 2
  }

  private val reverse = impl { (n: Nat) => generate(fun(i =>
    natAsIndex(n)(l(n) - l(1: Nat) - indexAsNat(i))
  )) :: (n`.`IndexType(n)) }

  // NOTE: Lift also generates two barriers for this one
  // FIXME: all barriers could be removed, dependency analysis needs to be more precise
  test("reorderInLocalButSequential") {
    val e = depFun((n: Nat) => fun(n`.`f32)(in =>
      in |> split(128) |> mapWorkGroup(
        split(16) >> mapLocal(mapSeq(id)) >> toLocal >>
        map(gather(reverse)) >> mapLocal(mapSeq(id)) >> join
      ) |> join
    ))
    val code = gen.opencl.kernel.asStringFromExpr(e)
    "barrier".r.findAllIn(code).length shouldBe 2 // 0
    """barrier\(CLK_LOCAL_MEM_FENCE\)""".r.findAllIn(code).length shouldBe 2 // 0
  }

  // NOTE: Lift generates GLOBAL barrier instead of LOCAL barrier for workgroup loop
  // FIXME: one barrier could be removed, dependency analysis needs to be more precise
  test("copyToLocalInZip") {
    val e = depFun((n: Nat) => fun(n`.`n`.`f32)(a => fun(n`.`n`.`f32)(b =>
      zip(a)(b) |> mapWorkGroup(fun(pab =>
        zip(fst(pab) |> mapLocal(id) |> toLocal)(
            snd(pab) |> mapLocal(id) |> toLocal) |>
        mapLocal(fun(x => fst(x) + snd(x)))
      ))
    )))
    val code = gen.opencl.kernel.asStringFromExpr(e)
    "barrier".r.findAllIn(code).length shouldBe 3 // 2
    """barrier\(CLK_LOCAL_MEM_FENCE\)""".r.findAllIn(code).length shouldBe 3 // 2
  }

  // NOTE: Lift generates GLOBAL barrier instead of LOCAL barrier for workgroup loop
  // FIXME: one barrier could be removed, dependency analysis needs to be more precise
  test("copyToLocalInZipAndReorder") {
    val e = depFun((n: Nat) => fun(n`.`n`.`f32)(a => fun(n`.`n`.`f32)(b =>
      zip(a)(b) |> mapWorkGroup(fun(pab =>
        zip(fst(pab) |> mapLocal(id) |> toLocal)(
            snd(pab) |> mapLocal(id) |> toLocal) |>
        gather(reverse) |>
        mapLocal(fun(x => fst(x) + snd(x)))
      ))
    )))
    val code = gen.opencl.kernel.asStringFromExpr(e)
    println(code)
    "barrier".r.findAllIn(code).length shouldBe 3 // 2
    """barrier\(CLK_LOCAL_MEM_FENCE\)""".r.findAllIn(code).length shouldBe 3 // 2
  }

  // NOTE: Lift generates GLOBAL barrier instead of LOCAL barrier for workgroup loop
  // FIXME: one barrier could be removed, dependency analysis needs to be more precise
  test("copyToLocalAndReorderInZip") {
    val e = depFun((n: Nat) => fun(n`.`n`.`f32)(a => fun(n`.`n`.`f32)(b =>
      zip(a)(b) |> mapWorkGroup(fun(pab =>
        zip(fst(pab) |> mapLocal(id) |> toLocal |> gather(reverse))(
            snd(pab) |> mapLocal(id) |> toLocal) |>
        mapLocal(fun(x => fst(x) + snd(x)))
      ))
    )))
    val code = gen.opencl.kernel.asStringFromExpr(e)
    "barrier".r.findAllIn(code).length shouldBe 3 // 2
    """barrier\(CLK_LOCAL_MEM_FENCE\)""".r.findAllIn(code).length shouldBe 3 // 2
  }

  // NOTE: Lift generates a GLOBAL barrier instead of a LOCAL one at the end of workgroup loop
  // FIXME: one barrier could be removed, dependency analysis needs to be more precise
  test("copyToLocalAndReorderInZip2") {
    val e = depFun((n: Nat) => fun(n`.`n`.`f32)(a => fun(n`.`n`.`f32)(b =>
      zip(a)(b) |> mapWorkGroup(fun(pab =>
        zip(fst(pab) |> mapLocal(id) |> toLocal)(
            snd(pab) |> mapLocal(id) |> toLocal |> gather(reverse)) |>
        mapLocal(fun(x => fst(x) + snd(x)))
      ))
    )))
    val code = gen.opencl.kernel.asStringFromExpr(e)
    "barrier".r.findAllIn(code).length shouldBe 3 // 2
    """barrier\(CLK_LOCAL_MEM_FENCE\)""".r.findAllIn(code).length shouldBe 3 // 2
  }

  // NOTE: does not pass in Lift either
  // FIXME: one barrier could be removed, dependency analysis needs to be more precise
  test("copyToLocalAndReorderBothInZip") {
    val e = depFun((n: Nat) => fun(n`.`n`.`f32)(a => fun(n`.`n`.`f32)(b =>
      zip(a)(b) |> mapWorkGroup(fun(pab =>
        zip(fst(pab) |> mapLocal(id) |> toLocal |> gather(reverse))(
            snd(pab) |> mapLocal(id) |> toLocal |> gather(reverse)) |>
        mapLocal(fun(x => fst(x) + snd(x)))
      ))
    )))
    val code = gen.opencl.kernel.asStringFromExpr(e)
    "barrier".r.findAllIn(code).length shouldBe 3
    """barrier\(CLK_LOCAL_MEM_FENCE\)""".r.findAllIn(code).length shouldBe 3
  }

  // NOTE: Lift generates a GLOBAL barrier instead of a LOCAL one at the end of workgroup loop
  // FIXME: one barrier could be removed, dependency analysis needs to be more precise
  test("copyOneToLocalInZip") {
    val e = depFun((n: Nat) => fun(n`.`n`.`f32)(a => fun(n`.`n`.`f32)(b =>
      zip(a)(b) |> mapWorkGroup(fun(pab =>
        zip(fst(pab) |> mapLocal(id) |> toLocal)(snd(pab)) |>
        mapLocal(fun(x => fst(x) + snd(x)))
      ))
    )))
    val code = gen.opencl.kernel.asStringFromExpr(e)
    "barrier".r.findAllIn(code).length shouldBe 2 // 1
    """barrier\(CLK_LOCAL_MEM_FENCE\)""".r.findAllIn(code).length shouldBe 2 // 1
  }

  // FIXME: one barrier could be removed, dependency analysis needs to be more precise
  test("reorderLocal") {
    val e = depFun((n: Nat) => fun(n`.`f32)(in =>
      in |> split(128) |> mapWorkGroup(
        gather(reverse) >> mapLocal(id) >> toLocal >> mapLocal(id)
      ) |> join
    ))
    val code = gen.opencl.kernel.asStringFromExpr(e)
    "barrier".r.findAllIn(code).length shouldBe 2 // 1
    """barrier\(CLK_LOCAL_MEM_FENCE\)""".r.findAllIn(code).length shouldBe 2 // 1
  }

  // FIXME: one barrier could be removed, dependency analysis needs to be more precise
  test("reorderWriteLastLocal") {
    val e = depFun((n: Nat) => fun(n`.`f32)(in =>
      in |> split(128) |> mapWorkGroup(
        mapLocal(id) >> toLocal >> mapLocal(id) >> scatter(reverse)
      ) |> join
    ))
    val code = gen.opencl.kernel.asStringFromExpr(e)
    "barrier".r.findAllIn(code).length shouldBe 2 // 1
    """barrier\(CLK_LOCAL_MEM_FENCE\)""".r.findAllIn(code).length shouldBe 2 // 1
  }

  // TODO: original test makes sure that loops are only taken once
  // FIXME: avoid generating barrier at end of loop if only taken once
  test("noLoopReorderLastLocal") {
    val e = depFun((n: Nat) => fun(n`.`f32)(in =>
      in |> split(128) |> mapWorkGroup(
        mapLocal(id) >> toLocal >> gather(reverse) >> mapLocal(id)
      ) |> join
    ))
    val code = gen.opencl.kernel.asStringFromExpr(e)
    "barrier".r.findAllIn(code).length shouldBe 2 // 1
    """barrier\(CLK_LOCAL_MEM_FENCE\)""".r.findAllIn(code).length shouldBe 2 // 1
  }

  // TODO: original test makes sure that loops are only taken once
  // FIXME: avoid generating barrier at end of loop if only taken once
  // FIXME: one barrier could be removed, dependency analysis needs to be more precise
  test("noLoopReorderLocal") {
    val e = depFun((n: Nat) => fun(n`.`f32)(in =>
      in |> split(128) |> mapWorkGroup(
        gather(reverse) >> mapLocal(id) >> toLocal >> mapLocal(id)
      ) |> join
    ))
    val code = gen.opencl.kernel.asStringFromExpr(e)
    "barrier".r.findAllIn(code).length shouldBe 2 // 0
    """barrier\(CLK_LOCAL_MEM_FENCE\)""".r.findAllIn(code).length shouldBe 2 // 0
  }

  // FIXME: one barrier could be removed, dependency analysis needs to be more precise
  test("noReorderLocal") {
    val e = depFun((n: Nat) => fun(n`.`f32)(in =>
      in |> split(128) |> mapWorkGroup(
        mapLocal(id) >> toLocal >> mapLocal(id)
      ) |> join
    ))
    val code = gen.opencl.kernel.asStringFromExpr(e)
    "barrier".r.findAllIn(code).length shouldBe 2 // 1
    """barrier\(CLK_LOCAL_MEM_FENCE\)""".r.findAllIn(code).length shouldBe 2 // 1
  }

  // TODO: original test makes sure that loops are only taken once
  // FIXME: avoid generating barrier at end of loop if only taken once
  // FIXME: one barrier could be removed, dependency analysis needs to be more precise
  test("noLoopNoReorderLocal") {
    val e = depFun((n: Nat) => fun(n`.`f32)(in =>
      in |> split(128) |> mapWorkGroup(
        mapLocal(id) >> toLocal >> mapLocal(id)
      ) |> join
    ))
    val code = gen.opencl.kernel.asStringFromExpr(e)
    "barrier".r.findAllIn(code).length shouldBe 2 // 0
    """barrier\(CLK_LOCAL_MEM_FENCE\)""".r.findAllIn(code).length shouldBe 2 // 0
  }

  // FIXME: dependency analysis gives up on generateCont, dependency analysis needs to be more precise
  ignore("loopReorder2Local") {
    val e = depFun((n: Nat) => fun(n`.`f32)(in =>
      in |> split(128) |> mapWorkGroup(
        gather(reverse) >> mapLocal(id) >> toLocal >>
        gather(reverse) >> mapLocal(id) >> toGlobal >>
        mapLocal(id)
      ) |> join
    ))
    val code = gen.opencl.kernel.asStringFromExpr(e)
    "barrier".r.findAllIn(code).length shouldBe 2
    """barrier\(CLK_LOCAL_MEM_FENCE\)""".r.findAllIn(code).length shouldBe 1
    """barrier\(CLK_GLOBAL_MEM_FENCE\)""".r.findAllIn(code).length shouldBe 1
  }

  // TODO: original test makes sure that loops are only taken once
  // FIXME: avoid generating barrier at end of loop if only taken once
  test("noLoopReorder2Local") {
    val e = depFun((n: Nat) => fun(n`.`f32)(in =>
      in |> split(128) |> mapWorkGroup(
        gather(reverse) >> mapLocal(id) >> toLocal >>
        gather(reverse) >> mapLocal(id)
      ) |> join
    ))
    val code = gen.opencl.kernel.asStringFromExpr(e)
    "barrier".r.findAllIn(code).length shouldBe 2 // 1
    """barrier\(CLK_LOCAL_MEM_FENCE\)""".r.findAllIn(code).length shouldBe 2 // 1
  }

  // TODO: original test makes sure that loops are only taken once
  // FIXME: avoid generating barrier at end of loop if only taken once
  test("noLoopReorder3Local") {
    val e = depFun((n: Nat) => fun(n`.`f32)(in =>
      in |> split(128) |> mapWorkGroup(
        gather(reverse) >> mapLocal(id) >> toLocal >>
        gather(reverse) >> mapLocal(id) >> toLocal >>
        gather(reverse) >> mapLocal(id)
      ) |> join
    ))
    val code = gen.opencl.kernel.asStringFromExpr(e)
    "barrier".r.findAllIn(code).length shouldBe 3 // 2
    """barrier\(CLK_LOCAL_MEM_FENCE\)""".r.findAllIn(code).length shouldBe 3 // 2
  }

  test("reorderLastLocal") {
    val e = depFun((n: Nat) => fun(n`.`f32)(in =>
      in |> split(128) |> mapWorkGroup(
        mapLocal(id) >> toLocal >> gather(reverse) >> mapLocal(id)
      ) |> join
    ))
    val code = gen.opencl.kernel.asStringFromExpr(e)
    "barrier".r.findAllIn(code).length shouldBe 2
    """barrier\(CLK_LOCAL_MEM_FENCE\)""".r.findAllIn(code).length shouldBe 2
  }

  val tail = drop(1)

  // NOTE: Lift only generates 1 barrier but uses more global memory .. hard to compare different choices
  test("tail") {
    val e = depFun((n: Nat) => fun(n`.`f32)(in =>
      in |> split(128) |> mapWorkGroup(
        mapLocal(id) >> toGlobal >> tail >> mapLocal(id)
      ) |> join
    ))
    val code = gen.opencl.kernel.asStringFromExpr(e)
    "barrier".r.findAllIn(code).length shouldBe 2
    """barrier\(CLK_GLOBAL_MEM_FENCE\)""".r.findAllIn(code).length shouldBe 2
  }

  // NOTE: Lift generates a GLOBAL barrier instead of a LOCAL one at the end of workgroup loop
  test("tailInLocal") {
    val e = depFun((n: Nat) => fun(n`.`f32)(in =>
      in |> split(128) |> mapWorkGroup(
        mapLocal(id) >> toLocal >> tail >> mapLocal(id)
      ) |> join
    ))
    val code = gen.opencl.kernel.asStringFromExpr(e)
    "barrier".r.findAllIn(code).length shouldBe 2
    """barrier\(CLK_LOCAL_MEM_FENCE\)""".r.findAllIn(code).length shouldBe 2
  }

  test("basicBarrier") {
    val e = depFun((n: Nat) => fun(n`.`f32)(in =>
      in |> split(128) |> mapWorkGroup(mapLocal(id)) |> join
    ))
    val code = gen.opencl.kernel.asStringFromExpr(e)
    "barrier".r.findAllIn(code).length shouldBe 0
  }

  // NOTE: Lift only generates 1 barrier but uses more global memory .. hard to compare different choices
  test("reorderGlobalLast") {
    val e = depFun((n: Nat) => fun(n `.` f32)(in =>
      in |> split(128) |> mapWorkGroup(
        mapLocal(id) >> toGlobal >> gather(reverse) >> mapLocal(id)
      ) |> join
    ))
    val code = gen.opencl.kernel.asStringFromExpr(e)
    "barrier".r.findAllIn(code).length shouldBe 2
    """barrier\(CLK_GLOBAL_MEM_FENCE\)""".r.findAllIn(code).length shouldBe 2
  }

  // NOTE: Lift only generates 1 barrier but uses more global memory .. hard to compare different choices
  test("reorderGlobalFirst") {
    val e = depFun((n: Nat) => fun(n `.` f32)(in =>
      in |> split(128) |> mapWorkGroup(
        gather(reverse) >> mapLocal(id) >> toGlobal >> mapLocal(id)
      ) |> join
    ))
    val code = gen.opencl.kernel.asStringFromExpr(e)
    "barrier".r.findAllIn(code).length shouldBe 2
    """barrier\(CLK_GLOBAL_MEM_FENCE\)""".r.findAllIn(code).length shouldBe 2
  }

  // NOTE: Lift generates a GLOBAL barrier instead of a LOCAL one at the end of workgroup loop
  // FIXME: one barrier could be removed, dependency analysis needs to be more precise
  test("doubleNestedMapLcl") {
    val e = depFun((n: Nat) => fun(n`.`n`.`n`.`n`.`f32)(in =>
      in |> mapWorkGroup(0)(mapWorkGroup(1)(
        mapLocal(0)(mapLocal(1)(id)) >> toLocal >>
        mapLocal(0)(mapLocal(1)(id))
      ))
    ))
    val code = gen.opencl.kernel.asStringFromExpr(e)
    "barrier".r.findAllIn(code).length shouldBe 2 // 1
    """barrier\(CLK_LOCAL_MEM_FENCE\)""".r.findAllIn(code).length shouldBe 2 // 1
  }

  // FIXME: all barriers could be removed, dependency analysis needs to be more precise
  test("tripleNestedMapLcl") {
    val e = depFun((n: Nat) => fun(n`.`n`.`n`.`n`.`n`.`n`.`f32)(in =>
      in |> mapWorkGroup(0)(mapWorkGroup(1)(mapWorkGroup(2)(
        mapLocal(0)(mapLocal(1)(mapLocal(2)(id))) >> toLocal >>
        mapLocal(0)(mapLocal(1)(mapLocal(2)(id)))
      )))
    ))
    val code = gen.opencl.kernel.asStringFromExpr(e)
    "barrier".r.findAllIn(code).length shouldBe 2 // 0
    """barrier\(CLK_LOCAL_MEM_FENCE\)""".r.findAllIn(code).length shouldBe 2 // 0
  }

  // NOTE: Lift generates a GLOBAL barrier instead of a LOCAL one at the end of workgroup loop
  test("tripleNestedMapLclWithScatter") {
    val e = depFun((n: Nat) => fun(n`.`n`.`n`.`n`.`n`.`n`.`f32)(in =>
      in |> mapWorkGroup(0)(mapWorkGroup(1)(mapWorkGroup(2)(
        mapLocal(0)(mapLocal(1)(mapLocal(2)(id) >> scatter(reverse))) >> toLocal >>
        mapLocal(0)(mapLocal(1)(mapLocal(2)(id)))
      )))
    ))
    val code = gen.opencl.kernel.asStringFromExpr(e)
    "barrier".r.findAllIn(code).length shouldBe 2
    """barrier\(CLK_LOCAL_MEM_FENCE\)""".r.findAllIn(code).length shouldBe 2
  }

  // NOTE: Lift generates a GLOBAL barrier instead of a LOCAL one at the end of workgroup loop
  test("doubleNestedMapLclWithReorder") {
    val e = depFun((n: Nat) => fun(n`.`n`.`n`.`n`.`f32)(in =>
      in |> mapWorkGroup(0)(mapWorkGroup(1)(
        mapLocal(0)(mapLocal(1)(id)) >> toLocal >>
        map(gather(reverse)) >>
        mapLocal(0)(mapLocal(1)(id))
      ))
    ))
    val code = gen.opencl.kernel.asStringFromExpr(e)
    "barrier".r.findAllIn(code).length shouldBe 2
    """barrier\(CLK_LOCAL_MEM_FENCE\)""".r.findAllIn(code).length shouldBe 2
  }

  // NOTE: Lift generates a GLOBAL barrier instead of a LOCAL one at the end of workgroup loop
  // NOTE: wrong access annotations, delete test in favor of doubleNestedMapLclWithReorder3 ?
  ignore("doubleNestedMapLclWithReorder2") {
    val e = depFun((n: Nat) => fun(n`.`n`.`n`.`n`.`f32)(in =>
      in |> mapWorkGroup(0)(mapWorkGroup(1)(
        mapLocal(0)(mapLocal(1)(id) >> gather(reverse)) >> toLocal >>
        mapLocal(0)(mapLocal(1)(id))
      ))
    ))
    val code = gen.opencl.kernel.asStringFromExpr(e)
    println(code)
    "barrier".r.findAllIn(code).length shouldBe 2
    """barrier\(CLK_LOCAL_MEM_FENCE\)""".r.findAllIn(code).length shouldBe 2
  }

  // NOTE: Lift generates a GLOBAL barrier instead of a LOCAL one at the end of workgroup loop
  test("doubleNestedMapLclWithReorder3") {
    val e = depFun((n: Nat) => fun(n`.`n`.`n`.`n`.`f32)(in =>
      in |> mapWorkGroup(0)(mapWorkGroup(1)(
        mapLocal(0)(mapLocal(1)(id)) >> toLocal >>
        mapLocal(0)(gather(reverse) >> mapLocal(1)(id))
      ))
    ))
    val code = gen.opencl.kernel.asStringFromExpr(e)
    "barrier".r.findAllIn(code).length shouldBe 2
    """barrier\(CLK_LOCAL_MEM_FENCE\)""".r.findAllIn(code).length shouldBe 2
  }

  // NOTE: Lift only generates 1 barrier but uses more global memory .. hard to compare different choices
  test("doubleNestedMapLclWithReorderGlobalMem") {
    val e = depFun((n: Nat) => fun(n`.`n`.`n`.`n`.`f32)(in =>
      in |> mapWorkGroup(0)(mapWorkGroup(1)(
        mapLocal(0)(mapLocal(1)(id)) >> toGlobal >>
        mapLocal(0)(gather(reverse) >> mapLocal(1)(id))
      ))
    ))
    val code = gen.opencl.kernel.asStringFromExpr(e)
    "barrier".r.findAllIn(code).length shouldBe 2
    """barrier\(CLK_GLOBAL_MEM_FENCE\)""".r.findAllIn(code).length shouldBe 2
  }

  // NOTE: Lift only generates 1 barrier but uses more global memory .. hard to compare different choices
  test("doubleNestedMapLclWithReorderGlobalMem2") {
    val e = depFun((n: Nat) => fun(n`.`n`.`n`.`n`.`f32)(in =>
      in |> mapWorkGroup(0)(mapWorkGroup(1)(
        mapLocal(0)(gather(reverse) >> mapLocal(1)(id)) >> toGlobal >>
        mapLocal(0)(mapLocal(1)(id))
      ))
    ))
    val code = gen.opencl.kernel.asStringFromExpr(e)
    "barrier".r.findAllIn(code).length shouldBe 2
    """barrier\(CLK_GLOBAL_MEM_FENCE\)""".r.findAllIn(code).length shouldBe 2
  }

  // NOTE: Lift only generates 1 barrier but uses more global memory .. hard to compare different choices
  test("doubleNestedMapLclWithReorderGlobalMem3") {
    val e = depFun((n: Nat) => fun(n`.`n`.`n`.`n`.`f32)(in =>
      in |> mapWorkGroup(0)(mapWorkGroup(1)(
        mapLocal(0)(mapLocal(1)(id)) >> scatter(reverse) >> toGlobal >>
        mapLocal(0)(mapLocal(1)(id))
      ))
    ))
    val code = gen.opencl.kernel.asStringFromExpr(e)
    "barrier".r.findAllIn(code).length shouldBe 2
    """barrier\(CLK_GLOBAL_MEM_FENCE\)""".r.findAllIn(code).length shouldBe 2
  }

  // FIXME: record of arrays does not generate valid code
  ignore("tupleInside2MapLcl") {
    val e = depFun((n: Nat, m: Nat) => fun(n`.`n`.`m`.`m`.`f32)(in =>
      in |> mapWorkGroup(0)(mapWorkGroup(1)(fun(x =>
        zip(x)(x) |> mapLocal(1)(fun(p =>
          zip(fst(p))(snd(p)) |> mapLocal(0)(fun(p =>
            makePair(fst(p))(snd(p))
          )) |> unzip
        )) |> toLocal |> unzip |> fst |>
        mapLocal(0)(mapLocal(1)(id))
      )))
    ))
    val code = gen.opencl.kernel.asStringFromExpr(e)
    println(code)
  }

  // FIXME: record of arrays does not generate valid code
  ignore("tupleInsideMapLcl") {
    val e = depFun((n: Nat, m: Nat) => fun(n`.`n`.`m`.`m`.`f32)(in =>
      in |> mapWorkGroup(0)(mapWorkGroup(1)(fun(x =>
        zip(x)(x) |> mapLocal(1)(fun(p =>
          makePair(mapLocal(0)(id)(fst(p)))(mapLocal(0)(id)(snd(p)))
        )) |> toLocal |> unzip |> fst |>
        mapLocal(1)(mapLocal(0)(id))
      )))
    ))
    val code = gen.opencl.kernel.asStringFromExpr(e)
    println(code)
  }

  // FIXME: record of arrays does not generate valid code
  ignore("tupleWithAsVectorInsideMapLcl") {
    val e = depFun((n: Nat, m: Nat) => fun(n`.`n`.`m`.`m`.`f32)(in =>
      in |> mapWorkGroup(0)(mapWorkGroup(1)(fun(x =>
        zip(x)(x) |> mapLocal(1)(fun(p =>
          makePair(fst(p) |> asVectorAligned(4) |> mapLocal(0)(id) |> asScalar)(
                   snd(p) |> asVectorAligned(4) |> mapLocal(0)(id) |> asScalar)
        )) |> toLocal |> unzip |> fst |>
        mapLocal(1)(asVectorAligned(4) >> mapLocal(0)(id) >> asScalar)
      )))
    ))
    val code = gen.opencl.kernel.asStringFromExpr(e)
    println(code)
  }

  // TODO: original test makes sure that loops are only taken once
  // FIXME: avoid generating barrier at end of loop if only taken once
  test("tupleBarrierJustLocal") {
    val e = depFun((n: Nat) => fun(n`.`f32)(in =>
      in |> split(32) |> mapWorkGroup(fun(x =>
        zip(x)(x) |> mapLocal(fun(y =>
          makePair(fst(y)/* |> toLocal*/)(snd(y)/* |> toLocal*/)
        )) |> toLocal |> gather(reverse) |>
        mapLocal(fun(p => fst(p) + snd(p)))
      ))
    ))
    val code = gen.opencl.kernel.asStringFromExpr(e)
    "barrier".r.findAllIn(code).length shouldBe 2 // 1
    """barrier\(CLK_LOCAL_MEM_FENCE\)""".r.findAllIn(code).length shouldBe 2 // 1
  }

  // NOTE: Lift only generates 1 barrier but uses more global memory .. hard to compare different choices
  test("tupleBarrierJustGlobal") {
    val e = depFun((n: Nat) => fun(n`.`f32)(in =>
      in |> split(32) |> mapWorkGroup(fun(x =>
        zip(x)(x) |> mapLocal(fun(y =>
          makePair(fst(y))(snd(y))
        )) |> toGlobal |> gather(reverse) |>
        mapLocal(fun(p => fst(p) + snd(p)))
      ))
    ))
    val code = gen.opencl.kernel.asStringFromExpr(e)
    "barrier".r.findAllIn(code).length shouldBe 2
    """barrier\(CLK_GLOBAL_MEM_FENCE\)""".r.findAllIn(code).length shouldBe 2
  }

  // FIXME: invalid access annotations, need to think about it
  ignore("tupleBarrierBoth") {
    val e = depFun((n: Nat) => fun(n`.`f32)(in =>
      in |> split(32) |> mapWorkGroup(fun(x =>
        zip(x)(x) |> mapLocal(fun(y =>
          makePair(fst(y) |> toLocal)(snd(y) |> toGlobal)
        )) |> gather(reverse) |>
        mapLocal(fun(p => fst(p) + snd(p)))
      ))
    ))
    val code = gen.opencl.kernel.asStringFromExpr(e)
    println(code)
    "barrier".r.findAllIn(code).length shouldBe 1
    """barrier\(CLK_LOCAL_MEM_FENCE | CLK_GLOBAL_MEM_FENCE\)""".r.findAllIn(code).length shouldBe 1
  }
}
