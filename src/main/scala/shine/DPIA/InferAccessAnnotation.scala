package shine.DPIA

import rise.{core => r}
import rise.core.{types => rt}
import rise.core.{primitives => rp}
import rise.OpenCL.{primitives => roclp}
import shine.DPIA.Phrases.{DepApply, Phrase}
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
    val (ePt, substAcc) =
      inferPhraseTypes(e, Map.empty, isKernelParamFun = true)
    val fullySubstitutedPtMap = substAcc(ptAnnotationMap.toMap)
    if (!specWritingOut(ePt))
      error(s"The program $e does not specify how to write the result" +
        "into output")
    fullySubstitutedPtMap
  }

  @tailrec
  private def specWritingOut(ePt: PhraseType): Boolean = ePt match {
    case DepFunType(_, t) => specWritingOut(t)
    case FunType(_, t) => specWritingOut(t)
    case expT: ExpType => expT `<=` ExpType(expT.dataType, write)
    case _ => throw error("This should never happen.")
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

    def apply[A <: r.Expr](ctx: Map[A, PhraseType]): Map[A, PhraseType] =
      ctx.map({ case (ri, pt) => (ri, apply(pt)) })

    def apply(s: Subst): Subst = {
      val merged =
        mergeSubstMap(applyToSubstMap(s.substMap), this.substMap)
      Subst(merged)
    }

    private def applyToSubstMap(substMap: SubstMap): SubstMap =
      substMap.map({
        case (ai, ati: AccessTypeIdentifier) =>
          if (this.substMap.contains(ati))
            (ai, this.substMap(ati))
          else (ai, ati)
        case subst => subst
      })

    @tailrec
    private def mergeSubstMap(
      s: SubstMap,
      res: SubstMap
    ): SubstMap = {
      if (s.nonEmpty) {
        mergeSubstMap(
          s.tail,
          if (!res.contains(s.head._1)) res + s.head
          else if(res(s.head._1) == s.head._2) res
          else error(
            s"Unexpectedly assigning var ${s.head._1} multiple times."))
      } else res
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
    ptAnnotationMap update(lambda.x, xType)
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
    val resSubst = subst(eSubst(fSubst))
    ptAnnotationMap update(app, appType)
    (appType, resSubst)
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
      case roclp.MapGlobal(_) | roclp.MapWorkGroup(_)
           | roclp.MapLocal(_) | rp.MapSeq() | rp.MapSeqUnroll() =>
        val rMapT = p.t.asInstanceOf[
          rt.FunType[rt.FunType[rt.DataType, rt.DataType],
            rt.FunType[rt.DataType, rt.DataType]]]
        val rFInT = rMapT.inT.inT
        val rFOutT = rMapT.inT.outT
        val rArrT = rMapT.outT.inT
        val rMapOutT = rMapT.outT.outT

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

      case rp.ToMem() =>
        val rToMemT = p.t.asInstanceOf[rt.FunType[rt.DataType, rt.DataType]]

        FunType(ExpType(dataType(rToMemT.inT), write),
          ExpType(dataType(rToMemT.outT), read))

      case roclp.OclToMem() =>
        val rToMemT = p.t.asInstanceOf[
          rt.DepFunType[rt.AddressSpaceKind,
            rt.FunType[rt.DataType, rt.DataType]]]

        DepFunType[AddressSpaceKind, PhraseType](
          addressSpaceIdentifier(rToMemT.x),
          FunType(ExpType(dataType(rToMemT.t.inT), write),
            ExpType(dataType(rToMemT.t.outT), read)))

      case rp.Join() | rp.Fst() | rp.Snd() | rp.Transpose() | rp.AsScalar()
           | rp.Unzip() =>
        val rT = p.t.asInstanceOf[rt.FunType[rt.DataType, rt.DataType]]

        val ai = accessTypeIdentifier()
        FunType(ExpType(dataType(rT.inT), ai),
          ExpType(dataType(rT.outT), ai))

      case rp.VectorFromScalar() | rp.Cast() | rp.Neg() | rp.Not() =>
        val rT = p.t.asInstanceOf[rt.FunType[rt.DataType, rt.DataType]]
        FunType(ExpType(dataType(rT.inT), read),
          ExpType(dataType(rT.outT), read))

      case rp.Let() =>
        val rT = p.t.asInstanceOf[
          rt.FunType[rt.FunType[rt.DataType, rt.DataType],
            rt.FunType[rt.DataType, rt.DataType]]]
        val rBoundT = rT.inT.outT
        val rOutT = rT.outT.outT

        val ai = accessTypeIdentifier()
        FunType(
          FunType(ExpType(dataType(rBoundT), read),
            ExpType(dataType(rOutT), ai)),
          FunType(ExpType(dataType(rBoundT), read),
            ExpType(dataType(rOutT), ai)))

      case rp.Split() | rp.AsVector() | rp.AsVectorAligned() | rp.Drop()
           | rp.Take() =>
        val rT = p.t.asInstanceOf[
          rt.DepFunType[rt.NatKind, rt.FunType[rt.DataType, rt.DataType]]]

        val ai = accessTypeIdentifier()
        DepFunType[NatKind, PhraseType](
          natIdentifier(rT.x),
          FunType(ExpType(dataType(rT.t.inT), ai),
            ExpType(dataType(rT.t.outT), ai)))

      case rp.Zip() | rp.Pair() =>
        val rT = p.t.asInstanceOf[
          rt.FunType[rt.DataType, rt.FunType[rt.DataType, rt.DataType]]]
        val fstT = rT.inT
        val sndT = rT.outT.inT
        val outT = rT.outT.outT

        val ai = accessTypeIdentifier()
        FunType(ExpType(dataType(fstT), ai),
          FunType(ExpType(dataType(sndT), ai), ExpType(dataType(outT), ai)))

      case rp.Idx() | rp.Add() | rp.Sub() | rp.Mul() | rp.Div() | rp.Gt()
           | rp.Lt() | rp.Equal() =>
        val rT = p.t.asInstanceOf[
          rt.FunType[rt.DataType, rt.FunType[rt.DataType, rt.DataType]]]

        FunType(ExpType(dataType(rT.inT), read),
          FunType(ExpType(dataType(rT.outT.inT), read),
            ExpType(dataType(rT.outT.outT), read)))

      case rp.ReduceSeq() | rp.ReduceSeqUnroll() =>
        val rT = p.t.asInstanceOf[
          rt.FunType[
            rt.FunType[rt.DataType, rt.FunType[rt.DataType, rt.DataType]],
            rt.FunType[rt.DataType, rt.FunType[rt.DataType, rt.DataType]]]]
        val initDt = dataType(rT.outT.inT)
        val arrDt = dataType(rT.outT.outT.inT)
        val elemDt = arrDt.asInstanceOf[ArrayType].elemType

        FunType(
          FunType(ExpType(initDt, read),
            FunType(ExpType(elemDt, read), ExpType(initDt, write))),
          FunType(ExpType(initDt, write),
            FunType(ExpType(arrDt, read), ExpType(initDt, read))))

      case roclp.OclReduceSeq() | roclp.OclReduceSeqUnroll() =>
        val rT = p.t.asInstanceOf[
          rt.DepFunType[rt.AddressSpaceKind,
            rt.FunType[
              rt.FunType[rt.DataType, rt.FunType[rt.DataType, rt.DataType]],
              rt.FunType[rt.DataType, rt.FunType[rt.DataType, rt.DataType]]]]]

        val initDt = dataType(rT.t.outT.inT)
        val arrDt = dataType(rT.t.outT.outT.inT)
        val elemDt = arrDt.asInstanceOf[ArrayType].elemType
        DepFunType[AddressSpaceKind, PhraseType](
          addressSpaceIdentifier(rT.x),
          FunType(
            FunType(ExpType(initDt, read),
              FunType(ExpType(elemDt, read), ExpType(initDt, write))),
            FunType(ExpType(initDt, write),
              FunType(ExpType(arrDt, read), ExpType(initDt, read)))))

      case rp.Slide() =>
        val rT = p.t.asInstanceOf[
          rt.DepFunType[rt.NatKind, rt.DepFunType[rt.NatKind,
            rt.FunType[rt.DataType, rt.DataType]]]]

        DepFunType[NatKind, PhraseType](natIdentifier(rT.x),
          DepFunType[NatKind, PhraseType](natIdentifier(rT.t.x),
            FunType(ExpType(dataType(rT.t.t.inT), read),
              ExpType(dataType(rT.t.t.outT), read))))

      case rp.Select() =>
        val rT = p.t.asInstanceOf[
          rt.FunType[rt.DataType, rt.FunType[rt.DataType, rt.Type]]]
        val b = dataType(rT.inT)
        val dt = dataType(rT.outT.inT)

        FunType(ExpType(b, read), FunType(ExpType(dt, read),
          FunType(ExpType(dt, read), ExpType(dt, read))))

      case rp.Generate() =>
        val rT = p.t.asInstanceOf[
          rt.FunType[rt.FunType[rt.DataType, rt.DataType], rt.DataType]]
        val idxDt = dataType(rT.inT.inT)
        val elemDt = dataType(rT.inT.outT)
        val arrDt = dataType(rT.outT)

        FunType(FunType(ExpType(idxDt, read), ExpType(elemDt, read)),
          ExpType(arrDt, read))

      case f@r.ForeignFunction(decl) =>
        val (inTs, outT) = foreignFunIO(f.t)
        val wF = wrapForeignFun(decl, inTs, outT, Vector())
        wF.t

      case arr@rp.MakeArray(_) =>
        val wArr = wrapArray(arr.t, Vector())
        wArr.t
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
      val outSubst = subUnifyPhraseType(argSubst(lout), argSubst(rout))
      val resSubst = outSubst(argSubst)
      resSubst
    case (DepFunType(lx, la), DepFunType(rx, ra)) if lx == rx =>
      subUnifyPhraseType(la, ra)
    case _ => error(s"Cannot subunify $less and $larger.")
  }

  def `type`(ty: rt.Type): PhraseType = ty match {
    case dt: rt.DataType => ExpType(dataType(dt), accessTypeIdentifier())
    case rt.FunType(i, o) => `type`(i) ->: `type`(o)
    case rt.DepFunType(i, t) => i match {
      case dt: rt.DataTypeIdentifier =>
        dataTypeIdentifier(dt) ->: `type`(t)
      case n: rt.NatIdentifier =>
        natIdentifier(n) ->: `type`(t)
      case n2n: rt.NatToNatIdentifier =>
        natToNatIdentifier(n2n) ->: `type`(t)
      case n2d: rt.NatToDataIdentifier =>
        natToDataIdentifier(n2d) ->: `type`(t)
    }
    case rt.TypeIdentifier(_) | rt.TypePlaceholder =>
      throw new Exception("This should not happen")
  }
}
