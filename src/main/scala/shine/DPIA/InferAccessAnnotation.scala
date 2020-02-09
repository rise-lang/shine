package shine.DPIA

import rise.{core => r}
import rise.core.{types => rt}
import rise.core.{primitives => rp}
import shine.DPIA.Types._
import shine.DPIA.Types.TypeCheck.SubTypeCheckHelper
import shine.DPIA.fromRise._

import scala.annotation.tailrec
import scala.collection.mutable
import scala.language.implicitConversions

object inferAccess {
  def apply(e: r.Expr): Map[r.Expr, PhraseType] = new InferAccessAnnotation()(e)
}

private class InferAccessAnnotation() {

  def apply(e: r.Expr): Predef.Map[r.Expr, PhraseType] = {
    inferPhraseTypes(e, Map.empty, isKernelParamFun = true)
    ptAnnotationMap.toMap
  }

  private type Context = Map[r.Identifier, PhraseType]
  private type SubstMap = Predef.Map[AccessTypeIdentifier, AccessType]
  //No occurs check needed, because rw annotations cannot contain variables
  private case class Subst(
    private val substMap: SubstMap = Predef.Map()) {

    def apply(pt: PhraseType): PhraseType = applySubstMap(pt, substMap)

    @tailrec
    private def applySubstMap(
      pt: PhraseType,
      substMap: SubstMap
    ): PhraseType =  {
      if (substMap.isEmpty) pt
      else {
        val substPhraseType =
          PhraseType.substitute(substMap.head._2, substMap.head._1, pt)
        applySubstMap(substPhraseType, substMap.tail)
      }
    }

    def apply(ctx: Context): Context =
      ctx.map({ case (ri, pt) => (ri, apply(pt)) })

    def apply(s: Subst): Subst = {
      val appended = appendSubstMap(s.substMap, this.substMap)
      Subst(appended)
    }

    @tailrec
    private def appendSubstMap(
      s: SubstMap,
      res: SubstMap): SubstMap = {
      if (s.nonEmpty)
        appendSubstMap(
          s.tail,
          if (!res.contains(s.head._1)) res + s.head
          else error(
            s"Unexpectedly assigning var ${s.head._1} multiple times."))
      else res
    }

    def +(i: AccessTypeIdentifier, pt: AccessType): Subst = {
      if (substMap contains i)
        throw new Exception(
          s"Substitution for phrase type identifier $i exists already.")
      else
        Subst(substMap updated (i, pt))
    }
  }

  private val ptAnnotationMap: mutable.Map[r.Expr, PhraseType] = mutable.Map()

  private def inferPhraseTypes(
    e: r.Expr,
    ctx: Context,
    isKernelParamFun: Boolean
  ): (PhraseType, Subst) = {
    e match {
      case i: r.Identifier =>
        ptAnnotationMap update(i, ctx(i))
        (ctx(i), Subst())
      case lit: r.Literal =>
        val lpt = ExpType(dataType(e.t.asInstanceOf[rt.DataType]), read)
        ptAnnotationMap update(lit, lpt)
        (lpt, Subst())
      case l: r.Lambda =>
        inferLambda(l, e.t.asInstanceOf[rt.FunType[rt.Type, _]].inT,
          ctx, isKernelParamFun)
      case appl: r.App =>
        inferApp(appl, ctx, addsKernelParam(e, isKernelParamFun))
      case depL: r.DepLambda[_] =>
        inferDepLambda(depL, ctx, isKernelParamFun)
      case depA: r.DepApp[_] =>
        inferDepApp(depA, ctx, addsKernelParam(e, isKernelParamFun))
      case p: r.Primitive => inferPrimitive(p)
    }
  }

  private def inferLambda(
    lambda: r.Lambda,
    inT: rt.Type,
    ctx: Context,
    isKernelParamFun: Boolean
  ): (PhraseType, Subst) = {
    val xType =
      if (isKernelParamFun) {
        //Functions that define kernel parameters are allowed to take
        //arguments of ExpType only.
        val dt = inT.asInstanceOf[rt.DataType]
        ExpType(dataType(dt), read)
      } else `type`(lambda.x.t)

    val ctxWithX = ctx updated(lambda.x, xType)
    val (eType, eSubst) =
      inferPhraseTypes(lambda.e, ctxWithX, isKernelParamFun)

    val lambdaType = FunType(eSubst(xType), eType)
    ptAnnotationMap update(lambda, lambdaType)
    (lambdaType, eSubst)
  }

  private def inferApp(
    app: r.App,
    ctx: Context,
    isKernelParamFun: Boolean
  ): (PhraseType, Subst) = {
    val (fType, fSubst) = inferPhraseTypes(app.f, ctx, isKernelParamFun)
    val (eType, eSubst) = inferPhraseTypes(app.e, fSubst(ctx), isKernelParamFun)
    val eSubstFType =
      eSubst(fType).asInstanceOf[FunType[_ <: PhraseType, _ <: PhraseType]]

    val subst = subUnifyPhraseType(eType, eSubstFType.inT)
    val appType = subst(eSubstFType.outT)
    ptAnnotationMap update(app, appType)
    (appType, subst(eSubst(fSubst)))
  }

  private def inferDepLambda(
    depLambda: r.DepLambda[_],
    ctx: Context,
    kernelParamFun: Boolean
  ): (PhraseType, Subst) = {
    val (eType, eSubst) = inferPhraseTypes(depLambda.e, ctx, kernelParamFun)
    val depLambdaType =
      depLambda.x match {
        case n: rt.NatIdentifier =>
          DepFunType[NatKind, PhraseType](natIdentifier(n), eType)
        case dt: rt.DataTypeIdentifier =>
          DepFunType[DataKind, PhraseType](dataTypeIdentifier(dt), eType)
        case ad: rt.AddressSpaceIdentifier =>
          DepFunType[AddressSpaceKind, PhraseType](
            addressSpaceIdentifier(ad), eType)
        case n2n: rt.NatToNatIdentifier =>
          DepFunType[NatToNatKind, PhraseType](natToNatIdentifier(n2n), eType)
        case n2d: rt.NatToDataIdentifier =>
          DepFunType[NatToDataKind, PhraseType](natToDataIdentifier(n2d), eType)
      }
    ptAnnotationMap update(depLambda, depLambdaType)
    (depLambdaType, eSubst)
  }

  private def inferDepApp(
    depApp: r.DepApp[_],
    ctx: Context,
    kernelParamFun: Boolean
  ): (PhraseType, Subst) = {
    val (fType, fSubst) = inferPhraseTypes(depApp.f, ctx, kernelParamFun)
    val depAppType =
      depApp.x match {
        case dt: rt.DataKind#T =>
          Lifting.liftDependentFunctionType[DataKind](fType)(dataType(dt))
        case addr: rt.AddressSpaceKind#T =>
          Lifting.liftDependentFunctionType[AddressSpaceKind](fType)(
            addressSpace(addr))
        case n: rt.NatKind#T =>
          Lifting.liftDependentFunctionType[NatKind](fType)(n)
        case n2n: rt.NatToNatKind#T =>
          Lifting.liftDependentFunctionType[NatToNatKind](fType)(ntn(n2n))
        case n2d: rt.NatToDataKind#T =>
          Lifting.liftDependentFunctionType[NatToDataKind](fType)(ntd(n2d))
      }
    ptAnnotationMap update(depApp, depAppType)
    (depAppType, fSubst)
  }

  private def inferPrimitive(p: r.Primitive): (PhraseType, Subst) = {
    val primitiveType = p match {
      case rp.MapSeq() =>
        val rMapSeqT = p.t.asInstanceOf[
          rt.FunType[rt.FunType[rt.DataType, rt.DataType],
            rt.FunType[rt.DataType, rt.DataType]]
        ]
        val rFInT = rMapSeqT.inT.inT
        val rFOutT = rMapSeqT.inT.outT
        val rArrT = rMapSeqT.outT.inT
        val rMapOutT = rMapSeqT.outT.outT

        FunType(
          FunType(ExpType(dataType(rFInT), read),
            ExpType(dataType(rFOutT), write)),
          FunType(ExpType(dataType(rArrT), read),
            ExpType(dataType(rMapOutT), write)))

      case rp.Map() =>
        val rMapT = p.t.asInstanceOf[
          rt.FunType[rt.FunType[rt.DataType, rt.DataType],
            rt.FunType[rt.DataType, rt.DataType]]
        ]
        val rFInT = rMapT.inT.inT
        val rFOutT = rMapT.inT.outT
        val rArrT = rMapT.outT.inT
        val rMapOutT = rMapT.outT.outT

        val ai = accessTypeIdentifier()
        FunType(
          FunType(ExpType(dataType(rFInT), ai),
            ExpType(dataType(rFOutT), ai)),
          FunType(ExpType(dataType(rArrT), ai),
            ExpType(dataType(rMapOutT), ai)))

      case rp.Transpose() =>
        val rTransposeT = p.t.asInstanceOf[rt.FunType[rt.DataType, rt.DataType]]

        val ai = accessTypeIdentifier()
        FunType(ExpType(dataType(rTransposeT.inT), ai),
          ExpType(dataType(rTransposeT.outT), ai))

      case rp.ToMem() =>
        val rToMemT = p.t.asInstanceOf[rt.FunType[rt.DataType, rt.DataType]]

        FunType(ExpType(dataType(rToMemT.inT), write),
          ExpType(dataType(rToMemT.outT), read))
    }

    ptAnnotationMap update(p, primitiveType)
    (primitiveType, Subst())
  }

  private def addsKernelParam(
    expr: r.Expr,
    kernelParamFun: Boolean
  ): Boolean =
    if (kernelParamFun)
      expr.t match {
        case _: rt.FunType[_, _] | _: rt.DepFunType[_, _] => true
        case _ => false
      }
    else false

  private def subUnifyPhraseType(
    less: PhraseType,
    larger: PhraseType,
  ): Subst = (less, larger) match {
    case (le@ExpType(ldt, la), re@ExpType(rdt, ra)) if ldt == rdt =>
      (la, ra) match {
        case (li: AccessTypeIdentifier, _) => Subst() + (li, ra)
        case (_, ri: AccessTypeIdentifier) => Subst() + (ri, la)
        case _ => if (le `<=` re) Subst()
                    else error(s"Cannot subunify $less and $larger.")
      }
    case (FunType(lin, lout), FunType(rin, rout)) =>
      val argSubst = subUnifyPhraseType(rin, lin)
      subUnifyPhraseType(argSubst(lout), argSubst(rout))
    case (DepFunType(lx, la), DepFunType(rx, ra)) if lx == rx =>
      subUnifyPhraseType(la, ra)
    case _ => error(s"Cannot subunify $less and $larger.")
  }
}
