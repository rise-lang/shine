package apps.harrisCornerDetection2

object generateCode {
  val H = 1024
  val W = 2048
  val kappa = 0.04f

  def checkBinomial(m: shine.OpenCL.Module): Unit = {
    val prelude = s"""
#include "src/main/scala/apps/harrisCornerDetection2/common.cpp"
#define gold(o, i) binomial_gold(o, $H, $W, i)
#define generated(ctx, o, i) foo_init_run(ctx, o, $H, $W, i)
"""
    convolutions.check(prelude, m, H, W)
  }

  def checkSobelX(m: shine.OpenCL.Module): Unit = {
    val prelude = s"""
#include "src/main/scala/apps/harrisCornerDetection2/common.cpp"
#define gold(o, i) sobelX_gold(o, $H, $W, i)
#define generated(ctx, o, i) foo_init_run(ctx, o, $H, $W, i)
"""
    convolutions.check(prelude, m, H, W)
  }

  def checkSobelY(m: shine.OpenCL.Module): Unit = {
    val prelude = s"""
#include "src/main/scala/apps/harrisCornerDetection2/common.cpp"
#define gold(o, i) sobelY_gold(o, $H, $W, i)
#define generated(ctx, o, i) foo_init_run(ctx, o, $H, $W, i)
"""
    convolutions.check(prelude, m, H, W)
  }

  def main(args: Array[String]): Unit = {
    val kernels = Seq[(String, rise.core.DSL.ToBeTyped[rise.core.Expr], shine.OpenCL.Module => Unit)](
      ("binomial-base", convolutions.base(binomialWeights2d), checkBinomial),
      ("binomial-line-vec", convolutions.lineVec(binomialWeightsV, binomialWeightsH), checkBinomial),
      ("binomial-rotv-vec", convolutions.rotvVec(binomialWeightsV, binomialWeightsH), checkBinomial),
      ("binomial-tile", convolutions.tile(binomialWeights2d), checkBinomial),
      ("binomial-tile-vec", convolutions.tileVec(binomialWeights2d), checkBinomial),

      ("sobelX-base", convolutions.base(sobelXWeights2d), checkSobelX),
      ("sobelX-line-vec", convolutions.lineVec(sobelXWeightsV, sobelXWeightsH), checkSobelX),
      ("sobelX-rotv-vec", convolutions.rotvVec(sobelXWeightsV, sobelXWeightsH), checkSobelX),
      ("sobelX-tile", convolutions.tile(sobelXWeights2d), checkSobelX),
      ("sobelX-tile-vec", convolutions.tileVec(sobelXWeights2d), checkSobelX),

      ("sobelY-base", convolutions.base(sobelYWeights2d), checkSobelY),
      ("sobelY-line-vec", convolutions.lineVec(sobelYWeightsV, sobelYWeightsH), checkSobelY),
      ("sobelY-rotv-vec", convolutions.rotvVec(sobelYWeightsV, sobelYWeightsH), checkSobelY),
      ("sobelY-tile", convolutions.tile(sobelYWeights2d), checkSobelY),
      ("sobelY-tile-vec", convolutions.tileVec(sobelYWeights2d), checkSobelY),

      ("mul-base", mul.base, mul.check(_, H, W)),
      ("mul-vec", mul.vec, mul.check(_, H, W)),
      ("mul-tile", mul.tile, mul.check(_, H, W)),
      ("mul-tileVec", mul.tileVec, mul.check(_, H, W)),

      ("coarsity-base", coarsity.base, coarsity.check(_, H, W, kappa)),
      ("coarsity-vec", coarsity.vec, coarsity.check(_, H, W, kappa)),
      ("coarsity-tile", coarsity.tile, coarsity.check(_, H, W, kappa)),
      ("coarsity-tileVec", coarsity.tileVec, coarsity.check(_, H, W, kappa)),

      ////

      ("sobelXYMul-base", sobelXYMul.base, sobelXYMul.check(_, H, W)),
      ("sobelXYMul-line-vec", sobelXYMul.lineVec, sobelXYMul.check(_, H, W)),
      ("sobelXYMul-rotv-vec", sobelXYMul.rotvVec, sobelXYMul.check(_, H, W)),
      ("sobelXYMul-tile", sobelXYMul.tile, sobelXYMul.check(_, H, W)),
      ("sobelXYMul-tile-vec", sobelXYMul.tileVec, sobelXYMul.check(_, H, W)),

      ("binomialCoarsity-base", binomialCoarsity.base, binomialCoarsity.check(_, H, W, kappa)),
      ("binomialCoarsity-line-vec", binomialCoarsity.lineVec, binomialCoarsity.check(_, H, W, kappa)),
      ("binomialCoarsity-rotv-vec", binomialCoarsity.rotvVec, binomialCoarsity.check(_, H, W, kappa)),
      ("binomialCoarsity-tile", binomialCoarsity.tile, binomialCoarsity.check(_, H, W, kappa)),
      ("binomialCoarsity-tile-vec", binomialCoarsity.tileVec, binomialCoarsity.check(_, H, W, kappa)),

      ////

      ("sobelXY-base", sobelXY.base, sobelXY.check(_, H, W)),
      ("sobelXY-line-vec", sobelXY.lineVec, sobelXY.check(_, H, W)),
      ("sobelXY-rotv-vec", sobelXY.rotvVec, sobelXY.check(_, H, W)),
      ("sobelXY-tile", sobelXY.tile, sobelXY.check(_, H, W)),
      ("sobelXY-tile-vec", sobelXY.tileVec, sobelXY.check(_, H, W)),

      ("mulBinomialCoarsity-base", mulBinomialCoarsity.base, mulBinomialCoarsity.check(_, H, W, kappa)),
      ("mulBinomialCoarsity-line-vec", mulBinomialCoarsity.lineVec, mulBinomialCoarsity.check(_, H, W, kappa)),
      ("mulBinomialCoarsity-rotv-vec", mulBinomialCoarsity.rotvVec, mulBinomialCoarsity.check(_, H, W, kappa)),
      ("mulBinomialCoarsity-tile", mulBinomialCoarsity.tile, mulBinomialCoarsity.check(_, H, W, kappa)),
      ("mulBinomialCoarsity-tile-vec", mulBinomialCoarsity.tileVec, mulBinomialCoarsity.check(_, H, W, kappa)),

      ////

      // TODO
    )

    java.nio.file.Files.createDirectories(java.nio.file.Paths.get("/tmp/harris/"))

    for ((name, prog, check) <- kernels) {
      println(name)
      // val p: rise.core.Expr = rise.core.DSL.toBeTyped(
      //   rise.eqsat.Expr.toNamedUnique(rise.eqsat.Expr.fromNamed(prog.toUntypedExpr)))
      val m = util.gen.opencl.hosted.fromExpr(prog)
      val c = util.gen.opencl.hosted.asString(m)
      util.writeToPath(s"/tmp/harris/${name}.c", c)
      check(m)
    }
  }
}
