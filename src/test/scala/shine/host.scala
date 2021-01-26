package shine

import rise.core.DSL._
import rise.core.DSL.Type._
import rise.core.types._
import rise.core.primitives.{toMem, _}
import rise.openCL.DSL._
import shine.OpenCL.{GlobalSize, LocalSize, valToNatTuple}
import util.gen

class host extends test_util.Tests {
  private def dumpModule(m: shine.OpenCL.Module): Unit = {
    m.kernels.foreach(km => println(gen.opencl.kernel.asString(km)))
    println(gen.c.function.asString(m.host))
  }

  test("no kernel call") {
    val e = depFun((n: Nat) => fun((n`.`i32) ->: (n`.`i32))(in =>
      mapSeq(add(li32(1)))(in)
    ))
    dumpModule(gen.opencl.hosted.fromExpr(e))
  }

  test("basic kernel call with fixed size") {
    val n: Nat = 128
    val e = fun((n`.`i32) ->: (n`.`i32))(in =>
      oclRun(LocalSize(2), GlobalSize(32))(mapGlobal(add(li32(1)))(in))
    )
    dumpModule(gen.opencl.hosted.fromExpr(e))
  }

  test("basic kernel call with variable size and post-process") {
    val e = depFun((n: Nat) => fun((n`.`i32) ->: (n`.`i32))(in =>
      oclRun(LocalSize(2), GlobalSize(n/2))(mapGlobal(add(li32(1)))(in)) |> toMem |> mapSeq(mul(li32(2)))
    ))
    dumpModule(gen.opencl.hosted.fromExpr(e))
  }

  test("two kernel calls") {
    val e = depFun((n: Nat) => fun((n`.`i32) ->: (n`.`i32))(in =>
      oclRun(LocalSize(2), GlobalSize(n/2))(mapGlobal(add(li32(1)))(in)) |> store(x =>
      oclRun(LocalSize(4), GlobalSize(n/2))(mapGlobal(add(li32(2)))(x)))
    ))
    dumpModule(gen.opencl.hosted.fromExpr(e))
  }
}
