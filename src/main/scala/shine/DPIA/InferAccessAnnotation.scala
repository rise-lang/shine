package shine.DPIA

import rise.{core => r}
import rise.core.{types => rt, TypeLevelDSL => rtdsl}
import rise.core.TypeLevelDSL.->:
import rise.core.{primitives => rp}
import rise.openMP.{primitives => rompp}
import rise.openCL.{primitives => roclp}
import shine.DPIA.Types._
import shine.DPIA.Types.TypeCheck.SubTypeCheckHelper
import shine.DPIA.fromRise._

import scala.annotation.tailrec
import scala.collection.mutable

object inferAccess {
  def apply(e: r.Expr): MutableIdentityHashMap[r.Expr, PhraseType] =
    new InferAccessAnnotation()(e)
}

private class InferAccessAnnotation {
  private val ptAnnotationMap = MutableIdentityHashMap[r.Expr, PhraseType]()

  def apply(e: r.Expr): MutableIdentityHashMap[r.Expr, PhraseType] = {
    val (ePt, substAcc) =
      inferPhraseTypes(e, Map.empty, isKernelParamFun = true)
    substAcc(ptAnnotationMap)
    if (!funOutIsWrite(ePt))
      error("The program does not specify how to write the result " +
        s"of the program into output:\n $e")
    ptAnnotationMap
  }

  @tailrec
  private def funOutIsWrite(ePt: PhraseType): Boolean = ePt match {
    case DepFunType(_, t) => funOutIsWrite(t)
    case FunType(_, t) => funOutIsWrite(t)
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

    def apply[A <: r.Expr](m: Map[A, PhraseType]): Map[A, PhraseType] =
      m mapValues apply

    def apply(m: mutable.Map[r.Expr, PhraseType]): Unit =
      m.foreach({ case (i, pt) => m.update(i, apply(pt))})

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

  private def inferPhraseTypes(
    e: r.Expr,
    ctx: Context,
    isKernelParamFun: Boolean
  ): (PhraseType, Subst) = {
    e match {
      case i: r.Identifier =>
        val pt = ctx(i)
        ptAnnotationMap update(i, pt)
        (pt, Subst())
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
      case roclp.MapGlobal(_) | roclp.MapWorkGroup(_) | roclp.MapLocal(_)
           | rompp.MapPar() | rp.MapSeq() | rp.MapSeqUnroll()
           | rp.MapStream() => p.t match {
        case ((s: rt.DataType) ->: (t: rt.DataType)) ->:
          rt.ArrayType(n, _) ->: rt.ArrayType(_, _) =>

          (expT(s, read) ->: expT(t, write)) ->:
            expT(rt.ArrayType(n, s), read) ->:
            expT(rt.ArrayType(n, t), write)
        case _ => error()
      }

      case rp.Map() | rp.MapFst() | rp.MapSnd() => p.t match {
        case ((s: rt.DataType) ->: (t: rt.DataType)) ->:
          rt.ArrayType(n, _) ->: rt.ArrayType(_, _) =>

          val ai = accessTypeIdentifier()
          (expT(s, ai) ->: expT(t, ai)) ->:
            expT(rt.ArrayType(n, s), ai) ->:
            expT(rt.ArrayType(n, t), ai)
        case _ => error()
      }

      case rp.ToMem() => p.t match {
        case (t: rt.DataType) ->: (_: rt.DataType) =>
          expT(t, write) ->: expT(t, read)
        case _ => error()
      }

      case roclp.OclToMem() => p.t match {
        case rtdsl.aFunT(a, (t: rt.DataType) ->: (_: rt.DataType)) =>
          aFunT(a, expT(t, write) ->: expT(t, read))
        case _ => error()
      }

      case rp.Join() | rp.Transpose() | rp.AsScalar()
           | rp.Unzip() => p.t match {
        case (dt1: rt.DataType) ->: (dt2: rt.DataType) =>
          val ai = accessTypeIdentifier()
          expT(dt1, ai) ->: expT(dt2, ai)
        case _ => error()
      }

      case rp.VectorFromScalar() | rp.Neg() | rp.Not()
           | rp.IndexAsNat() | rp.Fst() | rp.Snd()  | rp.Cast() => p.t match {
        case (dt1: rt.DataType) ->: (dt2: rt.DataType) =>
          expT(dt1, read) ->: expT(dt2, read)
        case _ => error()
      }

      case rp.Let() => p.t match {
        case (s: rt.DataType) ->:
          ((_: rt.DataType) ->: (t: rt.DataType)) ->:
          (_: rt.DataType) =>

          val ai = accessTypeIdentifier()
          expT(s, read) ->: (expT(s, read) ->: expT(t, ai)) ->: expT(t, ai)
        case _ => error()
      }

      case rp.Split() | rp.AsVector() | rp.AsVectorAligned() => p.t match {
        case rtdsl.nFunT(n, (dt1: rt.DataType) ->: (dt2: rt.DataType)) =>

          val ai = accessTypeIdentifier()
          nFunT(n, expT(dt1, ai) ->: expT(dt2, ai))
        case _ => error()
      }

      case rp.Zip() | rp.Pair() => p.t match {
        case (dt1: rt.DataType) ->: (dt2: rt.DataType) ->: (dt3: rt.DataType) =>

          val ai = accessTypeIdentifier()
          expT(dt1, ai) ->: expT(dt2, ai) ->: expT(dt3, ai)
        case _ => error()
      }

      case rp.Idx() | rp.Add() | rp.Sub() | rp.Mul() | rp.Div() | rp.Gt()
           | rp.Lt() | rp.Equal() | rp.Mod() | rp.Gather() => p.t match {
        case (dt1: rt.DataType) ->: (dt2: rt.DataType) ->: (dt3: rt.DataType) =>
          expT(dt1, read) ->: expT(dt2, read) ->: expT(dt3, read)
        case _ => error()
      }

      case rp.NatAsIndex() | rp.Take() | rp.Drop() => p.t match {
        case rtdsl.nFunT(n, (dt1: rt.DataType) ->: (dt2: rt.DataType)) =>
          nFunT(n, expT(dt1, read) ->: expT(dt2, read))
        case _ => error()
      }

      case rp.ReduceSeq() | rp.ReduceSeqUnroll() => p.t match {
        case ((t: rt.DataType) ->: (s: rt.DataType) ->: (_: rt.DataType)) ->:
          (_: rt.DataType) ->: rt.ArrayType(n, _) ->: (_: rt.DataType) =>

          (expT(t, read) ->: expT(s, read) ->: expT(t, write)) ->:
            expT(t, write) ->:
            expT(rt.ArrayType(n, s), read) ->:
            expT(t, read)
        case _ => error()
      }

      case  rp.ScanSeq() => p.t match {
        case ((s: rt.DataType) ->: (t: rt.DataType) ->: (_: rt.DataType)) ->:
          (_: rt.DataType) ->: rt.ArrayType(n, _) ->: rt.ArrayType(_, _) =>

          (expT(s, read) ->: expT(t, read) ->: expT(t, write)) ->:
            expT(t, write) ->:
            expT(rt.ArrayType(n, s), read) ->:
            expT(rt.ArrayType(n, t), write)
        case _ => error()
      }

      case roclp.OclReduceSeq() | roclp.OclReduceSeqUnroll() => p.t match {
        case rtdsl.aFunT(a,
          ((t: rt.DataType) ->: (s: rt.DataType) ->: (_: rt.DataType)) ->:
          (_: rt.DataType) ->: rt.ArrayType(n, _) ->: (_: rt.DataType)) =>

          aFunT(a,
            (expT(t, read) ->: expT(s, read) ->: expT(t, write)) ->:
            expT(t, write) ->:
            expT(rt.ArrayType(n, s), read) ->:
            expT(t, read))
        case _ => error()
      }

        //TODO Circular Buffer and OCL versions
      case rp.RotateValues() => p.t match {
        case  rtdsl.nFunT(sz,
                      ((s: rt.DataType) ->: (_: rt.DataType)) ->:
                      (inT: rt.ArrayType) ->:
                      (outT: rt.ArrayType)) =>
          nFunT(sz,
            (expT(s, read) ->: expT(s, write)) ->:
            expT(inT, read) ->:
            expT(outT, read))
        case _ => error()
      }

      case rp.CircularBuffer() => p.t match {
        case rtdsl.nFunT(alloc, rtdsl.nFunT(sz,
                      ((s: rt.DataType) ->: (t: rt.DataType)) ->:
                      (inT: rt.ArrayType) ->:
                      (outT: rt.ArrayType))) =>
          nFunT(alloc, nFunT(sz,
            (expT(s, read) ->: expT(s, write)) ->:
            expT(inT, read) ->:
            expT(outT, read)))
        case _ => error()
      }

      case roclp.OclRotateValues() => p.t match {
        case rtdsl.aFunT(a, rtdsl.nFunT(sz,
                   ((s: rt.DataType) ->: (_: rt.DataType)) ->:
                   (inT: rt.ArrayType) ->:
                   (outT: rt.ArrayType))) =>
          aFunT(a,
            nFunT(sz,
              (expT(s, read) ->: expT(s, write)) ->:
              expT(inT, read) ->:
              expT(outT, read)))
        case _ => error()
      }

      case rp.Slide() | rp.PadClamp() => p.t match {
        case rtdsl.nFunT(sz, rtdsl.nFunT(sp,
                    (dt1: rt.DataType) ->: (dt2: rt.DataType))) =>
          nFunT(sz, nFunT(sp,
            expT(dt1, read) ->: expT(dt2, read)))
        case _ => error()
      }

      case rp.Iterate() => p.t match {
        case rtdsl.nFunT(k,
              rtdsl.nFunT(l, (at1: rt.ArrayType) ->: (at2: rt.ArrayType)) ->:
              (at3: rt.ArrayType) ->:
              (at4: rt.ArrayType) ) =>
          nFunT(k,
            nFunT(l, expT(at1, read) ->: expT(at2, write)) ->:
            expT(at3, read) ->:
            expT(at4, write) )
        case _ => error()
      }

      case roclp.OclIterate() => p.t match {
        case rtdsl.aFunT(a, rtdsl.nFunT(k,
              rtdsl.nFunT(l, (at1: rt.ArrayType) ->: (at2: rt.ArrayType)) ->:
              (at3: rt.ArrayType) ->:
              (at4: rt.ArrayType) )) =>
          aFunT(a, nFunT(k,
            nFunT(l, expT(at1, read) ->: expT(at2, write)) ->:
              expT(at3, read) ->:
              expT(at4, write) ))
        case _ => error()
      }

      case rp.Select() => p.t match {
        case rt.bool ->: (t: rt.DataType) ->:
          (_: rt.DataType) ->: (_: rt.DataType) =>

          expT(rt.bool, read) ->:
            expT(t, read) ->: expT(t, read) ->: expT(t, read)
        case _ => error()
      }

      case rp.PadCst() => p.t match {
        case rtdsl.nFunT(l, rtdsl.nFunT(q,
          (t: rt.DataType) ->: rt.ArrayType(n, _) ->: rt.ArrayType(_, _) )) =>

          nFunT(l, nFunT(q,
            expT(t, read) ->:
              expT(rt.ArrayType(n, t), read) ->:
              expT(rt.ArrayType(l + n + q, t), read)))
        case _ => error()
      }

      case rp.Generate() => p.t match {
        case (rt.IndexType(n) ->: (t: rt.DataType)) ->: rt.ArrayType(_, _) =>
          (expT(rt.IndexType(n), read) ->: expT(t, read)) ->:
            expT(rt.ArrayType(n, t), read)
        case _ => error()
      }

      case rp.Reorder() => p.t match {
        case (rt.IndexType(n) ->: rt.IndexType(_)) ->:
          (rt.IndexType(_) ->: rt.IndexType(_)) ->:
          rt.ArrayType(_, t) ->: rt.ArrayType(_, _) =>

          val ai = accessTypeIdentifier()
          (expT(rt.IndexType(n), read) ->: expT(rt.IndexType(n), read)) ->:
            (expT(rt.IndexType(n), read) ->: expT(rt.IndexType(n), read)) ->:
            expT(rt.ArrayType(n, t), ai) ->: expT(rt.ArrayType(n, t), ai)
        case _ => error()
      }

      case roclp.OclReduceByIndexSeq() |
           roclp.OclReduceByIndexPar() |
           roclp.OclSegmentedReduce() => p.t match {
        case rtdsl.aFunT(a,
        ((t: rt.DataType) ->: (_: rt.DataType) ->: (_: rt.DataType)) ->:
          rt.ArrayType(k, _) ->: rt.ArrayType(n, rt.PairType(rt.IndexType(_), _)) ->: rt.ArrayType(_, _)) =>

          aFunT(a,
            (expT(t, read) ->: expT(t, read) ->: expT(t, write)) ->:
              expT(rt.ArrayType(k, t), write) ->:
                expT(rt.ArrayType(n, rt.PairType(rt.IndexType(k), t)), read) ->:
                    expT(rt.ArrayType(k, t), read))
        case _ => error()
      }

      case r.ForeignFunction(_) =>
        def buildType(t: rt.Type): PhraseType = t match {
          case dt: rt.DataType =>
            expT(dataType(dt), read)
          case rt.FunType(in: rt.DataType, out) =>
            expT(in, read) ->: buildType(out)
          case _ =>
            throw new Exception("This should not happen")
        }
        buildType(p.t)

      case rp.MakeArray(_) =>
        def buildType(t: rt.Type): PhraseType = t match {
          case rt.FunType(in: rt.DataType, out) =>
            expT(dataType(in), read) ->: buildType(out)
          case rt.ArrayType(n, dt) => expT(ArrayType(n, dataType(dt)), read)
          case _ => error(s"did not expect t")
        }
        buildType(p.t)
    }

    checkConsistency(p.t, primitiveType)

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
                  else error(s"Cannot subunify $less <: $larger.")
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
      error()
  }

  private def checkConsistency(t: rt.Type, pt: PhraseType): Unit = (t, pt) match {
    case (rt.FunType(inT, outT), FunType(inPT, outPT)) =>
      checkConsistency(inT, inPT)
      checkConsistency(outT, outPT)
    case (rt.DepFunType(x, t), DepFunType(y, pt)) =>
      if (x.name != y.name) error(s"Identifiers $x and $y differ")
      checkConsistency(t, pt)
    case (dt: rt.DataType, ExpType(dpt: DataType, _)) =>

    case _ => error(s"Types $t and $pt not compatible")
  }
}
