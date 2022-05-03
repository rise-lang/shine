package shine

import rise.core.DSL.Type._
import rise.core.DSL._
import rise.core.primitives.{toMem, _}
import rise.core.types.DataType._
import rise.core.types._
import rise.openCL.DSL._
import shine.OpenCL.{GlobalSize, LocalSize, valToNatTuple}
import util.gen

import scala.util.matching.Regex

class host extends test_util.Tests {
  private val main = """
const int N = 64;
int main(int argc, char** argv) {
  Context ctx = createDefaultContext();
  Buffer input = createBuffer(ctx, N * sizeof(int32_t), HOST_READ | HOST_WRITE | DEVICE_READ);
  Buffer output = createBuffer(ctx, N * sizeof(int32_t), HOST_READ | HOST_WRITE | DEVICE_WRITE);

  int32_t* in = hostBufferSync(ctx, input, N * sizeof(int32_t), HOST_WRITE);
  for (int i = 0; i < N; i++) {
    in[i] = 0;
  }

  foo_init_run(ctx, output, N, input);

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
    val prog = shine.OpenCL.Module.translateToString(m) + main
    util.ExecuteOpenCL(prog, "zero_copy")
    util.ExecuteOpenCL(prog, "one_copy")
  }

  private def findCount(c: Int, r: Regex, in: String): Unit =
    r.findAllIn(in).length shouldBe c

  private def findDeviceBufferSyncWrite(c: Int, in: String): Unit =
    findCount(c, """deviceBufferSync\(.*, DEVICE_WRITE\)""".r, in)

  private def findDeviceBufferSyncRead(c: Int, in: String): Unit =
    findCount(c, """deviceBufferSync\(.*, DEVICE_READ\)""".r, in)

  test("no kernel call") {
    val e = depFun((n: Nat) => fun((n`.`i32) ->: (n`.`i32))(in =>
      mapSeq(add(li32(3)))(in)
    ))
    val m = gen.opencl.hosted.fromExpr(e)
    val hostCode = gen.c.function.asString(m.hostCode)
    // logger.debug(hostCode)
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
    val hostCode = gen.c.function.asString(m.hostCode)
    // logger.debug(hostCode)
    findDeviceBufferSyncWrite(1, hostCode)
    findDeviceBufferSyncRead(1, hostCode)
    // m.kernels.foreach(km => logger.debug(gen.opencl.kernel.asString(km)))
  }

  test("basic kernel call with variable size and post-process") {
    val e = depFun((n: Nat) => fun((n`.`i32) ->: (n`.`i32))(in =>
      oclRun(LocalSize(2), GlobalSize(n/2))(mapGlobal(add(li32(1)))(in)) |> toMem |>
      mapSeq(add(li32(2)))
    ))
    val m = gen.opencl.hosted.fromExpr(e)
    val hostCode = gen.c.function.asString(m.hostCode)
    // logger.debug(hostCode)
    findCount(1, """createBuffer\(.*, HOST_READ \| DEVICE_WRITE\)""".r, hostCode)
    findDeviceBufferSyncWrite(1, hostCode)
    findDeviceBufferSyncRead(1, hostCode)
    findCount(1, """hostBufferSync\(.*, HOST_WRITE\)""".r, hostCode)
    findCount(1, """hostBufferSync\(.*, HOST_READ\)""".r, hostCode)
    checkOutput(m)
  }

  test("basic kernel call with variable size and pre-process") {
    val e = depFun((n: Nat) => fun((n`.`i32) ->: (n`.`i32))(in =>
      mapSeq(add(li32(2)))(in) |> store(x =>
      oclRun(LocalSize(2), GlobalSize(n/2))(mapGlobal(add(li32(1)))(x)))
    ))
    val m = gen.opencl.hosted.fromExpr(e)
    val hostCode = gen.c.function.asString(m.hostCode)
    // logger.debug(hostCode)
    findCount(1, """createBuffer\(.*, HOST_WRITE \| DEVICE_READ\)""".r, hostCode)
    findDeviceBufferSyncWrite(1, hostCode)
    findDeviceBufferSyncRead(1, hostCode)
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
    val hostCode = gen.c.function.asString(m.hostCode)
    // logger.debug(hostCode)
    findCount(1, """createBuffer\(.*, DEVICE_WRITE \| DEVICE_READ\)""".r, hostCode)
    findDeviceBufferSyncWrite(2, hostCode)
    findDeviceBufferSyncRead(2, hostCode)
    checkOutput(m)
  }

  test("two kernel calls, intermediate processing") {
    val e = depFun((n: Nat) => fun((n`.`i32) ->: (n`.`i32))(in =>
      oclRun(LocalSize(2), GlobalSize(n/2))(mapGlobal(add(li32(3)))(in)) |> store(x1 =>
      mapSeq(fun(x => x - li32(1)))(x1) |> store(x2 =>
      oclRun(LocalSize(4), GlobalSize(n/2))(mapGlobal(add(li32(1)))(x2))))
    ))
    val m = gen.opencl.hosted.fromExpr(e)
    val hostCode = gen.c.function.asString(m.hostCode)
    // println(hostCode)
    findCount(1, """createBuffer\(.*, HOST_READ \| DEVICE_WRITE\)""".r, hostCode)
    findCount(1, """createBuffer\(.*, HOST_WRITE \| DEVICE_READ\)""".r, hostCode)
    findDeviceBufferSyncWrite(2, hostCode)
    findDeviceBufferSyncRead(2, hostCode)
    findCount(1, """hostBufferSync\(.*, HOST_WRITE\)""".r, hostCode)
    findCount(1, """hostBufferSync\(.*, HOST_READ\)""".r, hostCode)
    checkOutput(m)
  }

  test("local memory") {
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
    val hostCode = gen.c.function.asString(m.hostCode)
    // logger.debug(hostCode)
    findDeviceBufferSyncWrite(1, hostCode)
    findDeviceBufferSyncRead(1, hostCode)
    checkOutput(m)
  }

  test("global memory") {
    val e = depFun((n: Nat) => fun((n`.`i32) ->: (n`.`i32))(in =>
      oclRun(LocalSize(16), GlobalSize(n))(
        in |> split(16) |> mapWorkGroup(0)(
          mapLocal(0)(add(li32(1))) >>
          toGlobal >>
          mapLocal(0)(add(li32(2)))
        ) |> join
      )
    ))
    val m = gen.opencl.hosted.fromExpr(e)
    val hostCode = gen.c.function.asString(m.hostCode)
    // logger.debug(hostCode)
    findDeviceBufferSyncWrite(1, hostCode)
    findDeviceBufferSyncRead(1, hostCode)
    checkOutput(m)
  }
}
