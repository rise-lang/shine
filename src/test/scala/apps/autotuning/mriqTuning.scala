package apps.autotuning

import apps.mriQ.{computePhiMagOcl, computeQOcl}
import rise.autotune
import rise.autotune.{HostCode, Timeouts, tuningParam, wrapOclRun}
import rise.core.Expr
import rise.core.types.{Nat, TuningParameter}
import shine.OpenCL.{GlobalSize, LocalSize}

class mriqTuning extends test_util.Tests {

  val computePhiMagTuning:Expr =
    tuningParam("ls0", (ls0: Nat) =>
      tuningParam("ls1", (ls1: Nat) =>
        tuningParam("gs0", (gs0: Nat) =>
          tuningParam("gs1", (gs1: Nat) =>
            wrapOclRun(LocalSize(ls0, ls1), GlobalSize(gs0, gs1))(computePhiMagOcl)
          ))))

  val computeQTuning:Expr =
    tuningParam("ls0", (ls0: Nat) =>
      tuningParam("ls1", (ls1: Nat) =>
        tuningParam("gs0", (gs0: Nat) =>
          tuningParam("gs1", (gs1: Nat) =>
            wrapOclRun(LocalSize(ls0, ls1), GlobalSize(gs0, gs1))(computeQOcl)
          ))))

  // scalastyle:off
  val initPhiMag: (Int) => String = (K) => {
    s"""
       |  const int K = ${K};
       |  srand(time(NULL));
       |  Buffer phiR = createBuffer(ctx, K * sizeof(float), HOST_WRITE | DEVICE_READ);
       |  Buffer phiI = createBuffer(ctx, K * sizeof(float), HOST_WRITE | DEVICE_READ);
       |  Buffer output = createBuffer(ctx, K * sizeof(float), HOST_READ | DEVICE_WRITE);
       |
       |  float* in_phiR = hostBufferSync(ctx, phiR, K * sizeof(float), HOST_WRITE);
       |  for (int i = 0; i < K; i++) {
       |    in_phiR[i] = (float)(rand() % 100);
       |  }
       |
       |  float* in_phiI = hostBufferSync(ctx, phiI, K * sizeof(float), HOST_WRITE);
       |    for (int i = 0; i < K; i++) {
       |      in_phiI[i] = (float)(rand() % 100);
       |  }
       |
       |
       |  deviceBufferSync(ctx, phiR, K * sizeof(float), DEVICE_READ);
       |  deviceBufferSync(ctx, phiI, K * sizeof(float), DEVICE_READ);
       |""".stripMargin
  }
  val computePhiMag =
    s"""
       |    fun_init_run(ctx, output, K, phiR, phiI);
       |""".stripMargin

  val finishPhiMag =
    s"""
       |  // could add error checking
       |  destroyBuffer(ctx, phiR);
       |  destroyBuffer(ctx, phiI);
       |  destroyBuffer(ctx, output);
       |""".stripMargin

  val initQ: (Int, Int) => String = (K, X) => {
    s"""
       |  const int K = ${K};
       |  const int X = ${X};
       |  srand(time(NULL));
       |  Buffer x = createBuffer(ctx, X * sizeof(float), HOST_WRITE | DEVICE_READ);
       |  Buffer y = createBuffer(ctx, X * sizeof(float), HOST_WRITE | DEVICE_READ);
       |  Buffer z = createBuffer(ctx, X * sizeof(float), HOST_WRITE | DEVICE_READ);
       |  Buffer Qr = createBuffer(ctx, X * sizeof(float), HOST_WRITE | DEVICE_READ);
       |  Buffer Qi = createBuffer(ctx, X * sizeof(float), HOST_WRITE | DEVICE_READ);
       |  Buffer kvalues = createBuffer(ctx, 4 * K * sizeof(float), HOST_WRITE | DEVICE_READ);
       |  Buffer output = createBuffer(ctx, 4 * K * sizeof(float), HOST_READ | DEVICE_WRITE);
       |
       |  float* in_x = hostBufferSync(ctx, x, X * sizeof(float), HOST_WRITE);
       |  float* in_y = hostBufferSync(ctx, y, X * sizeof(float), HOST_WRITE);
       |  float* in_z = hostBufferSync(ctx, z, X * sizeof(float), HOST_WRITE);
       |  for (int i = 0; i < X; i++) {
       |    in_x[i] = (float)(rand() % 100);
       |    in_y[i] = (float)(rand() % 100);
       |    in_z[i] = (float)(rand() % 100);
       |  }
       |
       |  float* in_Qr = hostBufferSync(ctx, Qr, X * sizeof(float), HOST_WRITE);
       |  float* in_Qi = hostBufferSync(ctx, Qi, X * sizeof(float), HOST_WRITE);
       |  for (int i = 0; i < X; i++) {
       |      in_Qr[i] = (float)(rand() % 100);
       |      in_Qi[i] = (float)(rand() % 100);
       |  }
       |
       |  float* in_kvalues = hostBufferSync(ctx, Qi, 4 * K * sizeof(float), HOST_WRITE);
       |  for (int i = 0; i < 4 * K; i++) {
       |    in_kvalues[i] = (float)(rand() % 100);
       |  }
       |
       |  deviceBufferSync(ctx, x, X * sizeof(float), DEVICE_READ);
       |  deviceBufferSync(ctx, y, X * sizeof(float), DEVICE_READ);
       |  deviceBufferSync(ctx, z, X * sizeof(float), DEVICE_READ);
       |
       |  deviceBufferSync(ctx, Qr, X * sizeof(float), DEVICE_READ);
       |  deviceBufferSync(ctx, Qi, X * sizeof(float), DEVICE_READ);
       |
       |  deviceBufferSync(ctx, kvalues, 4 * K * sizeof(float), DEVICE_READ);
       |""".stripMargin
  }
  val computeQ =
    s"""
       |    fun_init_run(ctx, output, K, X, x, y, z, Qr, Qi, kvalues);
       |""".stripMargin

  val finishQ =
    s"""
       |  // could add error checking
       |
       |  destroyBuffer(ctx, x);
       |  destroyBuffer(ctx, y);
       |  destroyBuffer(ctx, z);
       |
       |  destroyBuffer(ctx, Qr);
       |  destroyBuffer(ctx, Qi);
       |
       |  destroyBuffer(ctx, kvalues);
       |
       |  destroyBuffer(ctx, output);
       |""".stripMargin
  // scalastyle:on


  test("execute computePhiMag"){

    println("nbody: \n" + computePhiMagTuning)

    val params = Map(
      TuningParameter("ls0") -> (256: Nat),
      TuningParameter("ls1") -> (1: Nat),
      TuningParameter("gs0") -> (512: Nat),
      TuningParameter("gs1") -> (1: Nat)
    )

    val phimag_replaced = rise.core.substitute.natsInExpr(params, computePhiMagTuning)
    println("phimag_replaced: \n" + phimag_replaced)

    val result = autotune.execution.execute(
      expression = phimag_replaced,
      hostCode = HostCode(initPhiMag(256), computePhiMag, finishPhiMag),
      timeouts = Timeouts(5000, 5000, 5000),
      executionIterations = 10,
      speedupFactor = 100
    )

    println("result: " + result)
  }

  test("execute computeQ"){

    println("nbody: \n" + computeQTuning)

    val params = Map(
      TuningParameter("ls0") -> (256: Nat),
      TuningParameter("ls1") -> (1: Nat),
      TuningParameter("gs0") -> (512: Nat),
      TuningParameter("gs1") -> (1: Nat)
    )

    val q_replaced = rise.core.substitute.natsInExpr(params, computeQTuning)
    println("q_replaced: \n" + q_replaced)

    val result = autotune.execution.execute(
      expression = q_replaced,
      hostCode = HostCode(initQ(256, 512), computeQ, finishQ),
      timeouts = Timeouts(5000, 5000, 5000),
      executionIterations = 10,
      speedupFactor = 100
    )

    println("result: " + result)
  }


}
