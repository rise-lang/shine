package apps.harrisCornerDetection2

object generateCode {
  def main(args: Array[String]): Unit = {
    val kernels = Seq[(String, rise.core.DSL.ToBeTyped[rise.core.Expr])](
      /*
      "binomial-base" -> convolutions.base(binomialWeights2d),
      "binomial-line-vec" -> convolutions.lineVec(binomialWeightsV, binomialWeightsH),
      "binomial-rotv-vec" -> convolutions.rotvVec(binomialWeightsV, binomialWeightsH),
      "binomial-tile" -> convolutions.tile(binomialWeights2d),
      "binomial-tile-vec" -> convolutions.tileVec(binomialWeights2d),

      "sobelX-base" -> convolutions.base(sobelXWeights2d),
      "sobelX-line-vec" -> convolutions.lineVec(sobelXWeightsV, sobelXWeightsH),
      "sobelX-rotv-vec" -> convolutions.rotvVec(sobelXWeightsV, sobelXWeightsH),
      "sobelX-tile" -> convolutions.tile(sobelXWeights2d),
      "sobelX-tile-vec" -> convolutions.tileVec(sobelXWeights2d),

      "sobelY-base" -> convolutions.base(sobelYWeights2d),
      "sobelY-line-vec" -> convolutions.lineVec(sobelYWeightsV, sobelYWeightsH),
      "sobelY-rotv-vec" -> convolutions.rotvVec(sobelYWeightsV, sobelYWeightsH),
      "sobelY-tile" -> convolutions.tile(sobelYWeights2d),
      "sobelY-tile-vec" -> convolutions.tileVec(sobelYWeights2d),

      "mul-base" -> mul.base,
      "mul-vec" -> mul.vec,
      "mul-tile" -> mul.tile,
      "mul-tileVec" -> mul.tileVec,

      "coarsity-base" -> coarsity.base,
      "coarsity-vec" -> coarsity.vec,
      "coarsity-tile" -> coarsity.tile,
      "coarsity-tileVec" -> coarsity.tileVec,

      ////

      "sobelXYMul-base" -> sobelXYMul.base,
      "sobelXYMul-line-vec" -> sobelXYMul.lineVec,
      "sobelXYMul-rotv-vec" -> sobelXYMul.rotvVec,
      "sobelXYMul-tile" -> sobelXYMul.tile,
      "sobelXYMul-tile-vec" -> sobelXYMul.tileVec,
*/
      "binomialCoarsity-base" -> binomialCoarsity.base,
      "binomialCoarsity-line-vec" -> binomialCoarsity.lineVec,
      "binomialCoarsity-rotv-vec" -> binomialCoarsity.rotvVec,
      "binomialCoarsity-tile" -> binomialCoarsity.tile,
      "binomialCoarsity-tile-vec" -> binomialCoarsity.tileVec,

      ////

      "sobelXY-base" -> sobelXY.base,
      "sobelXY-line-vec" -> sobelXY.lineVec,
      "sobelXY-rotv-vec" -> sobelXY.rotvVec,
      "sobelXY-tile" -> sobelXY.tile,
      "sobelXY-tile-vec" -> sobelXY.tileVec,

      "mulBinomialCoarsity-base" -> mulBinomialCoarsity.base,
      "mulBinomialCoarsity-line-vec" -> mulBinomialCoarsity.lineVec,
      "mulBinomialCoarsity-rotv-vec" -> mulBinomialCoarsity.rotvVec,
      "mulBinomialCoarsity-tile" -> mulBinomialCoarsity.tile,
      "mulBinomialCoarsity-tile-vec" -> mulBinomialCoarsity.tileVec,

      ////

      // TODO
    )

    java.nio.file.Files.createDirectories(java.nio.file.Paths.get("/tmp/harris/"))

    for ((name, prog) <- kernels) {
      println(name)
      // val p: rise.core.Expr = rise.core.DSL.toBeTyped(
      //   rise.eqsat.Expr.toNamedUnique(rise.eqsat.Expr.fromNamed(prog.toUntypedExpr)))
      val m = util.gen.opencl.hosted.fromExpr(prog)
      val c = util.gen.opencl.hosted.asString(m)
      util.writeToPath(s"/tmp/harris/${name}.c", c)
    }
  }
}
