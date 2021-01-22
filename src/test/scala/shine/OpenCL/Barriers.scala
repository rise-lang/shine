package shine.OpenCL

import util.gen
import rise.core.DSL._
import rise.core.DSL.Type._
import rise.core.types._
import rise.core.types.AddressSpace._
import rise.core.primitives._
import rise.openCL.primitives.oclReduceSeq
import rise.openCL.TypedDSL._

// scalastyle:off org.scalastyle.scalariform.MultipleStringLiteralsChecker
class Barriers extends test_util.Tests {
  private val sum = oclReduceSeq(Private)(add)(l(0.0f))

  // TODO? removing these barriers requires more fine grain analysis to
  // notice that the threads do not actually communicate through the memory
  ignore("1D mapLocal toLocal seq without thread sharing") {
    val e = depFun((n: Nat, m: Nat) => fun(n`.`m`.`f32)(in =>
      in |> mapWorkGroup(0)(
        mapLocal(0)(fun(x => x)) >>
        toLocal >>
        mapLocal(0)(fun(x => x))
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
        mapLocal(0)(fun(x => x)) >>
        toGlobal >>
        mapLocal(0)(fun(x => x))
      ))
    )
    val code = gen.opencl.kernel.asStringFromExpr(e)
    "barrier".r.findAllIn(code).length shouldBe 0
  }

  test("1D mapLocal toLocal seq with thread sharing") {
    val e = depFun((n: Nat, m: Nat) => fun(n`.`m`.`f32)(in =>
      in |> mapWorkGroup(0)(
        mapLocal(0)(fun(x => x)) >>
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
        mapLocal(0)(fun(x => x)) >>
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
          mapLocal(1)(mapLocal(0)(fun(x => x))) >>
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
            mapLocal(0)(fun(x => x)) >>
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
            mapLocal(0)(fun(x => x)) >>
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
        zip(mapLocal(0)(fun(x => x))(a))(mapLocal(0)(fun(x => x))(a)) |>
        toLocal >>
        slide(3)(1) >>
        mapLocal(0)(map(fst) >> sum)
      ))
    ))
    val code = gen.opencl.kernel.asStringFromExpr(e)
    "barrier".r.findAllIn(code).length shouldBe 2
    """barrier\(CLK_LOCAL_MEM_FENCE\)""".r.findAllIn(code).length shouldBe 2
  }
}
