package shine

import rise.core.DSL._
import rise.core.DSL.Type._
import rise.core.types._
import rise.core.primitives.{toMem, _}
import rise.openCL.DSL._
import shine.OpenCL.{GlobalSize, LocalSize, valToNatTuple}
import util.gen

import scala.util.matching.Regex

class host extends test_util.Tests {
  private def dumpModule(m: shine.OpenCL.Module): Unit = {
    m.kernels.foreach(km => println(gen.opencl.kernel.asString(km)))
    println(gen.c.function.asString(m.host))
  }

  private val main = """
#include "host.c"

const int N = 64;
int main(int argc, char** argv) {
  Context ctx = createContext("Portable Computing Language", "cpu");
  Buffer input = createBuffer(ctx, N * sizeof(int32_t), HOST_READ | HOST_WRITE | TARGET_READ);
  Buffer output = createBuffer(ctx, N * sizeof(int32_t), HOST_READ | HOST_WRITE | TARGET_WRITE);

  int32_t* in = hostBufferSync(ctx, input, N * sizeof(int32_t), HOST_WRITE);
  for (int i = 0; i < N; i++) {
    in[i] = 0;
  }

  foo(ctx, output, N, input);

  int32_t* out = hostBufferSync(ctx, output, N * sizeof(int32_t), HOST_READ);

  for (int i = 0; i < N; i++) {
    if (out[i] != 3) {
      fprintf(stderr, "wrong output: %i\n", out[i]);
      exit(EXIT_FAILURE);
    }
  }

  destroyBuffer(ctx, input);
  destroyBuffer(ctx, output);
  destroyContext(ctx);
  return EXIT_SUCCESS;
}
"""

  private def checkOutput(m: shine.OpenCL.Module): Unit = {
    util.ExecuteOpenCL(m, "zero_copy", main)
    util.ExecuteOpenCL(m, "one_copy", main)
  }

  private def findCount(c: Int, r: Regex, in: String): Unit =
    r.findAllIn(in).length shouldBe c

  test("no kernel call") {
    val e = depFun((n: Nat) => fun((n`.`i32) ->: (n`.`i32))(in =>
      mapSeq(add(li32(3)))(in)
    ))
    val m = gen.opencl.hosted.fromExpr(e)
    val hostCode = gen.c.function.asString(m.host)
    // println(hostCode)
    findCount(1, """hostBufferSync\(.*, HOST_WRITE\)""".r, hostCode)
    findCount(1, """hostBufferSync\(.*, HOST_READ\)""".r, hostCode)
    checkOutput(m)
  }

  test("basic kernel call with fixed size") {
    val n: Nat = 128
    val e = fun((n`.`i32) ->: (n`.`i32))(in =>
      oclRun(LocalSize(2), GlobalSize(32))(mapGlobal(add(li32(3)))(in))
    )
    val m = gen.opencl.hosted.fromExpr(e)
    val hostCode = gen.c.function.asString(m.host)
    // println(hostCode)
    findCount(1, """targetBufferSync\(.*, TARGET_WRITE\)""".r, hostCode)
    findCount(1, """targetBufferSync\(.*, TARGET_READ\)""".r, hostCode)
    // m.kernels.foreach(km => println(gen.opencl.kernel.asString(km)))
  }

  test("basic kernel call with variable size and post-process") {
    val e = depFun((n: Nat) => fun((n`.`i32) ->: (n`.`i32))(in =>
      oclRun(LocalSize(2), GlobalSize(n/2))(mapGlobal(add(li32(1)))(in)) |> toMem |>
      mapSeq(add(li32(2)))
    ))
    val m = gen.opencl.hosted.fromExpr(e)
    val hostCode = gen.c.function.asString(m.host)
    // println(hostCode)
    findCount(1, """createBuffer\(.*, HOST_READ \| TARGET_WRITE\)""".r, hostCode)
    findCount(1, """targetBufferSync\(.*, TARGET_WRITE\)""".r, hostCode)
    findCount(1, """targetBufferSync\(.*, TARGET_READ\)""".r, hostCode)
    findCount(1, """hostBufferSync\(.*, HOST_WRITE\)""".r, hostCode)
    findCount(1, """hostBufferSync\(.*, HOST_READ\)""".r, hostCode)
    checkOutput(m)
  }

  test("two kernel calls") {
    val e = depFun((n: Nat) => fun((n`.`i32) ->: (n`.`i32))(in =>
      oclRun(LocalSize(2), GlobalSize(n/2))(mapGlobal(add(li32(1)))(in)) |> store(x =>
      oclRun(LocalSize(4), GlobalSize(n/2))(mapGlobal(add(li32(2)))(x)))
    ))
    val m = gen.opencl.hosted.fromExpr(e)
    val hostCode = gen.c.function.asString(m.host)
    // println(hostCode)
    findCount(1, """createBuffer\(.*, TARGET_WRITE \| TARGET_READ\)""".r, hostCode)
    findCount(2, """targetBufferSync\(.*, TARGET_WRITE\)""".r, hostCode)
    findCount(2, """targetBufferSync\(.*, TARGET_READ\)""".r, hostCode)
    checkOutput(m)
  }

  ignore("local memory") {
    // TODO: clSetKernelArg(k, i, localMemSize, NULL);
    val e = depFun((n: Nat) => fun((n`.`i32) ->: (n`.`i32))(in =>
      oclRun(LocalSize(16), GlobalSize(n))(
        in |> split(16) |> mapWorkGroup(0)(
          mapLocal(0)(add(li32(1))) >>
          toLocal >>
          mapLocal(0)(add(li32(2)))
        ) |> join
      )
    ))
    val m = gen.opencl.hosted.fromExpr(e)
    val hostCode = gen.c.function.asString(m.host)
    // println(hostCode)
    findCount(1, """targetBufferSync\(.*, TARGET_WRITE\)""".r, hostCode)
    findCount(1, """targetBufferSync\(.*, TARGET_READ\)""".r, hostCode)
    checkOutput(m)
  }
}
