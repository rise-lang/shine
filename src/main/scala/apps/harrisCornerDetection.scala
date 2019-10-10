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
  private val id = C2D.id
  private val mulT = C2D.mulT
  private val sq = fun(x => x * x)
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

  private def ul(l: Expr): semantics.Data = l match {
    case Literal(d) => d
    case _ => ???
  }
  private val sobelXYMuls = nFun(szr, h => nFun(szr, w => fun(
    (h`.`w`.`float) ->: ((h`.`w`.`float) x ((h`.`w`.`float) x (h`.`w`.`float)))
  )(input => {
    val shuffle =
      asScalar >> drop(3) >> take(6) >> slide(4)(1) >> join >> asVector(4)
    input |>
    map(implN(w => fun(w`.`float)(x =>
      x |> asVectorAligned(4)
        |> padCst(1)(0)(vectorFromScalar(x `@` lidx(0, w)))
        |> padCst(0)(1)(vectorFromScalar(x `@` lidx(w - 1, w)))
    ))) |> padClamp(1)(1) |> slide(3)(1) |> mapGlobal(transpose >>
      map(fun(vNbh =>
        larr(Seq(ul(C2D.sobelXWeightsV), ul(C2D.sobelYWeightsV))) |>
        map(fun(vWs => C2D.weightsSeqVecUnroll(vWs)(vNbh)))
      )) >>
      oclSlideSeq(slideSeq.Values)(AddressSpace.Private)(3)(1)(mapSeqUnroll(id))(
        transpose >> map(shuffle) >>
          zip(larr(Seq(ul(C2D.sobelXWeightsH), ul(C2D.sobelYWeightsH)))) >>
          // TODO: this triggers an extra copy
          toPrivateFun(mapSeqUnroll(fun(hWsNbh =>
            C2D.weightsSeqVecUnroll(hWsNbh._1)(hWsNbh._2)
          ))) >>
          let(fun(ixiy => {
            val ix = ixiy `@` lidx(0, 2)
            val iy = ixiy `@` lidx(1, 2)
            pair(sq(ix), pair(ix * iy, sq(iy)))
          }))
      ) >>
      unzip >> mapSnd(unzip) >>
      mapFst(asScalar) >> mapSnd(mapFst(asScalar) >> mapSnd(asScalar))
    ) >>
    unzip >> mapSnd(unzip)
  })))

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

  case class NoPipe(sobelX: KernelNoSizes,
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

  object NoPipe {
    def create: NoPipe = NoPipe(
      gen.OpenCLKernel(sobelX),
      gen.OpenCLKernel(sobelY),
      gen.OpenCLKernel(mul),
      gen.OpenCLKernel(gaussian),
      gen.OpenCLKernel(coarsity)
    )
  }

  case class HalfPipe2(sobelXYMuls: KernelNoSizes) {
    def run(input: Array[Array[Float]],
            kappa: Float): (Array[Float], Seq[(String, TimeSpan[Time.ms])]) = {
      ???
    }
  }

  object HalfPipe2 {
    def create: HalfPipe2 = HalfPipe2(
      gen.OpenCLKernel(sobelXYMuls)
    )
  }
}
