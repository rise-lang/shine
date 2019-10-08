package apps

import lift.core._
import lift.core.DSL._
import lift.core.types._
import lift.core.primitives._
import lift.OpenCL.primitives._
import lift.core.HighLevelConstructs._
import util.gen

object harrisCornerDetection {
  private val C2D = separableConvolution2D
  private val mulT = C2D.mulT
  private val zip2D = zipND(2)

  private val szr = lift.arithmetic.RangeAdd(20, lift.arithmetic.PosInf, 1)
  private val sobelX = nFun(szr, h => nFun(szr, w => fun(
    (h`.`w`.`float) ->: (h`.`w`.`float)
  )(a =>
    C2D.regRotPar(C2D.sobelXWeightsV)(C2D.sobelXWeightsH)(a)
  )))
  private val sobelY = nFun(szr, h => nFun(szr, w => fun(
    (h`.`w`.`float) ->: (h`.`w`.`float)
  )(a =>
    C2D.regRotPar(C2D.sobelYWeightsV)(C2D.sobelYWeightsH)(a)
  )))
  private val mul = nFun(szr, h => nFun(szr, w => fun(
    (h`.`w`.`float) ->: (h`.`w`.`float) ->: (h`.`w`.`float)
  )((a, b) =>
    zip2D(a)(b) |> mapGlobal(mapSeq(mulT))
  )))
  private val gaussian = nFun(szr, h => nFun(szr, w => fun(
    (h`.`w`.`float) ->: (h`.`w`.`float)
  )(a =>
    C2D.regRotPar(C2D.binomialWeightsV)(C2D.binomialWeightsH)(a)
  )))
  private val coarsity = nFun(szr, h => nFun(szr, w => fun(
    (h`.`w`.`float) ->: (h`.`w`.`float) ->: (h`.`w`.`float) ->: float ->: (h`.`w`.`float)
  )((sxx, sxy, syy, kappa) =>
    zip2D(sxx)(zip2D(sxy)(syy)) |> mapGlobal(mapSeq(fun(s => {
      val sxx = fst(s)
      val sxy = fst(snd(s))
      val syy = snd(snd(s))

      val det = sxx * syy - sxy * sxy
      val trace = sxx + syy
      det - kappa * trace * trace
    })))
  )))

  /* TODO?
  val threshold = coarsity :>> mapSeq(mapSeq(fun(c =>
    if (c <= threshold) { 0.0f } else { c }
  )))

  val edge = slide3x3(coarsity) :>> mapSeq(mapSeq(fun(nbh =>
    forall i != center, nbh[center] > nbh[i] ?
  )))
  */

  import idealised.OpenCL._
  import util.{Time, TimeSpan}

  case class Kernels(sobelX: KernelNoSizes,
                     sobelY: KernelNoSizes,
                     mul: KernelNoSizes,
                     gaussian: KernelNoSizes,
                     coarsity: KernelNoSizes) {
    def run(input: Array[Array[Float]],
            kappa: Float): (Array[Float], Seq[(String, TimeSpan[Time.ms])]) = {
      val H = input.length
      val W = input(0).length

      val localSize = LocalSize(1)
      val globalSize = GlobalSize(H)

      def as2D[T]: ((Array[Float], T)) => (Array[Array[Float]], T) = {
        case (a, t) => (a.sliding(W, W).toArray, t)
      }

      val fSx = sobelX.as[ScalaFunction `(`
        Int `,` Int `,` Array[Array[Float]]
      `)=>` Array[Float]]
      val (ix, ixt) = as2D(fSx(localSize, globalSize)(H `,` W `,` input))

      val fSy = sobelY.as[ScalaFunction `(`
        Int `,` Int `,` Array[Array[Float]]
        `)=>` Array[Float]]
      val (iy, iyt) = as2D(fSy(localSize, globalSize)(H `,` W `,` input))

      val fMul = mul.as[ScalaFunction `(`
        Int `,` Int `,` Array[Array[Float]] `,` Array[Array[Float]]
        `)=>` Array[Float]]
      val (ixx, ixxt) = as2D(fMul(localSize, globalSize)(H `,` W `,` ix `,` ix))
      val (ixy, ixyt) = as2D(fMul(localSize, globalSize)(H `,` W `,` ix `,` iy))
      val (iyy, iyyt) = as2D(fMul(localSize, globalSize)(H `,` W `,` iy `,` iy))

      val fG = gaussian.as[ScalaFunction `(`
        Int `,` Int `,` Array[Array[Float]]
        `)=>` Array[Float]]
      val (sxx, sxxt) = as2D(fG(localSize, globalSize)(H `,` W `,` ixx))
      val (sxy, sxyt) = as2D(fG(localSize, globalSize)(H `,` W `,` ixy))
      val (syy, syyt) = as2D(fG(localSize, globalSize)(H `,` W `,` iyy))

      val fC = coarsity.as[ScalaFunction `(`
        Int `,` Int `,` Array[Array[Float]] `,` Array[Array[Float]] `,` Array[Array[Float]] `,` Float
        `)=>` Array[Float]]
      val (k, kt) = fC(localSize, globalSize)(H `,` W `,` sxx `,` sxy `,` syy `,` kappa)

      (k, Seq(
        "Ix" -> ixt,
        "Iy" -> iyt,
        "Ixx" -> ixxt,
        "Ixy" -> ixyt,
        "Iyy" -> iyyt,
        "Sxx" -> sxxt,
        "Sxy" -> sxyt,
        "Syy" -> syyt,
        "K" -> kt
      ))
    }
  }

  def genOCLKernels(): Kernels = Kernels(
    gen.OpenCLKernel(sobelX),
    gen.OpenCLKernel(sobelY),
    gen.OpenCLKernel(mul),
    gen.OpenCLKernel(gaussian),
    gen.OpenCLKernel(coarsity)
  )
}
