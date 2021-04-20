package apps

import apps.separableConvolution2D._
import apps.separableConvolution2DCheck.wrapExpr
import rise.core.DSL._
import rise.core._
import rise.core.semantics.FloatData
import rise.core.types._
import rise.eqsat.rules
import rise.eqsat.Basic.proveEquiv

object separableConvolution2DEqsat {
  // TODO: use full expressions instead of proxy primitives
  object weights2d extends Builder {
    private final case class Primitive()
                                      (override val t: Type = TypePlaceholder)
      extends rise.core.Primitive
    {
      override val name: String = "weights2d"
      override def setType(ty: Type): Primitive = Primitive()(ty)
      override def typeScheme: Type = binomialWeights2d.t
    }

    override def primitive: rise.core.Primitive = Primitive()()
    override def apply: ToBeTyped[rise.core.Primitive] = toBeTyped(Primitive()())
  }
  object weightsV extends Builder {
    private final case class Primitive()
                                      (override val t: Type = TypePlaceholder)
      extends rise.core.Primitive
    {
      override val name: String = "weightsV"
      override def setType(ty: Type): Primitive = Primitive()(ty)
      override def typeScheme: Type = binomialWeightsV.t
    }

    override def primitive: rise.core.Primitive = Primitive()()
    override def apply: ToBeTyped[rise.core.Primitive] = toBeTyped(Primitive()())
  }
  object weightsH extends Builder {
    private final case class Primitive()
                                      (override val t: Type = TypePlaceholder)
      extends rise.core.Primitive
    {
      override val name: String = "weightsH"
      override def setType(ty: Type): rise.core.Primitive = Primitive()(ty)
      override def typeScheme: Type = binomialWeightsH.t
    }

    override def primitive: rise.core.Primitive = Primitive()()
    override def apply: ToBeTyped[rise.core.Primitive] = toBeTyped(Primitive()())
  }
}

class separableConvolution2DEqsat extends test_util.Tests {
  import separableConvolution2DEqsat._

  private val (separateDot, separateDotT) = {
    import rise.eqsat.NamedRewrite
    import rise.eqsat.NamedRewriteDSL._

    def mulT(xName: String) = lam(xName, app(app(mul, app(fst, xName)), app(snd, xName)))
    def *(p: Pattern) = app(map, p)
    val sum = app(app(reduce, add), l(FloatData(0)))
    def dot(xName: String, a: Pattern, b: Pattern) =
      app(sum, app(*(mulT(xName)), app(app(zip, a), b)))
    val w2d = weights2d.primitive
    val wV = weightsV.primitive
    val wH = weightsH.primitive

    (NamedRewrite.init("separate-dot-hv",
      dot("x", app(join, w2d :: ("t": Type)), app(join, ("nbh": Pattern) :: ("t": Type)))
        -->
      dot("x2", wV, app(*(lam("y", dot("x3", wH, "y"))), "nbh"))
    ),
    NamedRewrite.init("separate-dot-vh",
      dot("x", app(join, w2d :: ("t": Type)), app(join, ("nbh": Pattern) :: ("t": Type)))
        -->
      dot("x2", wH, app(*(lam("y", dot("x3", wV, "y"))), app(transpose, "nbh")))
    ))
  }

  // -- algorithmic

  test("base to factorised") {
    proveEquiv(wrapExpr(base(weights2d)), wrapExpr(factorised(weightsV)(weightsH)),
      Seq(separateDot))
  }

  test("base to factorised (VH)") {
    proveEquiv(wrapExpr(base(weights2d)), wrapExpr(factorisedVH(weightsV)(weightsH)),
      Seq(separateDotT))
  }

  test("base to scanline") {
    proveEquiv(
      base(weights2d),
      scanline(weightsV)(weightsH),
      Seq(
      rules.eta, rules.beta, rules.removeTransposePair,
      rules.mapFusion, rules.mapFission,
      rules.slideBeforeMap, rules.mapSlideBeforeTranspose, rules.slideBeforeMapMapF,
      separateDotT
    ))
  }

  test("scanline to separated") {
    proveEquiv(wrapExpr(scanline(weightsV)(weightsH)), wrapExpr(separated(weightsV)(weightsH)), Seq(
      rules.eta, rules.beta, rules.mapFission, rules.mapFusion
    ))
  }

  // -- lowering

  test("base to baseSeq") {
    proveEquiv(wrapExpr(base(weights2d)), wrapExpr(baseSeq(weights2d)), Seq(
      rules.reduceSeqUnroll, rules.mapSeq
    ))
  }

  test("factorised to factorisedSeq") {
    proveEquiv(wrapExpr(factorised(weightsV)(weightsH)), wrapExpr(factorisedSeq(weightsV)(weightsH)), Seq(
      rules.reduceSeqUnroll, rules.mapSeq
    ))
  }

  test("separated to separatedSeq") {
    proveEquiv(wrapExpr(separated(weightsV)(weightsH)), wrapExpr(separatedSeq(weightsV)(weightsH)), Seq(
      rules.reduceSeqUnroll, rules.mapSeq, rules.toMemAfterMapSeq
    ))
  }

  test("scanline to scanlineSeq") {
    proveEquiv(wrapExpr(scanline(weightsV)(weightsH)), wrapExpr(scanlineSeq(weightsV)(weightsH)), Seq(
      rules.reduceSeqUnroll, rules.mapSeq
    ))
  }

  /* TODO: rotate values
  test("scanline to regRotSeq") {
    proveEquiv(scanline(weightsV)(weightsH), regRotSeq(weightsV)(weightsH), Seq(
      rules.reduceSeqUnroll, rules.mapSeq, rules.rotateValues, rules.iterateStream
    ))
  } */
}

