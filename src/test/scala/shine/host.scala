package shine

import rise.core.DSL._
import rise.core.DSL.Type._
import rise.core.types._
import rise.core.primitives.{toMem, _}
import rise.openCL.primitives.oclRun
import rise.openCL.TypedDSL._
import util.gen

class host extends test_util.Tests {
  private def dumpModule(m: shine.OpenCL.Module): Unit = {
    m.kernels.foreach(km => println(gen.opencl.kernel.asString(km)))
    println(gen.c.function.asString(m.host))
  }

  test("basic kernel call with fixed size") {
    val n: Nat = 128
    val e = fun((n`.`i32) ->: (n`.`i32))(in =>
      oclRun(2)(1)(1)(32)(1)(1)(mapGlobal(add(li32(1)))(in))
    )
    dumpModule(gen.opencl.hosted.fromExpr(e))
  }

  test("basic kernel call with variable size and post-process") {
    val e = depFun((n: Nat) => fun((n`.`i32) ->: (n`.`i32))(in =>
      oclRun(2)(1)(1)(n/2)(1)(1)(mapGlobal(add(li32(1)))(in)) |> toMem |> mapSeq(mul(li32(2)))
    ))
    dumpModule(gen.opencl.hosted.fromExpr(e))
  }
}
