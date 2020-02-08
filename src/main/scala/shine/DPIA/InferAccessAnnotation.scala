package shine.DPIA

import rise.{core => r}
import rise.core.{types => rt}
import rise.core.{primitives => rp}
import shine.DPIA.Types._
import shine.DPIA.Types.TypeCheck.SubTypeCheckHelper

import scala.annotation.tailrec
import scala.collection.mutable
import scala.language.implicitConversions

object inferAccess {
  def apply(e: r.Expr): Map[r.Expr, PhraseType] = {
    val infer = new InferAccessAnnotation()
    infer(e)
  }
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
        lambda(l, e.t.asInstanceOf[rt.FunType[rt.Type, _]].inT,
          ctx, isKernelParamFun)
      case appl: r.App =>
        app(appl, ctx, addsKernelParam(e, isKernelParamFun))
      case depL: r.DepLambda[_] =>
        depLambda(depL, ctx, isKernelParamFun)
      case depA: r.DepApp[_] =>
        depApp(depA, ctx, addsKernelParam(e, isKernelParamFun))
      case p: r.Primitive => primitive(p)
    }
  }

  private def lambda(
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

  private def app(
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

  private def depLambda(
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

  private def depApp(
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

  private def primitive(
    p: r.Primitive,
  ): (PhraseType, Subst) = {
    p match {
      case rp.MapSeq() =>
        val rMapSeqT = p.t.asInstanceOf[
          rt.FunType[rt.FunType[rt.DataType, rt.DataType],
            rt.FunType[rt.DataType, rt.DataType]]
        ]
        val rFInT = rMapSeqT.inT.inT
        val rFOutT = rMapSeqT.inT.outT
        val rArrT = rMapSeqT.outT.inT
        val rMapOutT = rMapSeqT.outT.outT

        val mapSeqType = FunType(
          FunType(ExpType(dataType(rFInT), read),
            ExpType(dataType(rFOutT), write)),
          FunType(ExpType(dataType(rArrT), read),
            ExpType(dataType(rMapOutT), write)))
        (mapSeqType, Subst())

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
        val mapType = FunType(
          FunType(ExpType(dataType(rFInT), ai),
            ExpType(dataType(rFOutT), ai)),
          FunType(ExpType(dataType(rArrT), ai),
            ExpType(dataType(rMapOutT), ai)))
        (mapType, Subst())

      case rp.Transpose() =>
        val rTransposeT = p.t.asInstanceOf[rt.FunType[rt.DataType, rt.DataType]]

        val ai = accessTypeIdentifier()
        val transposeType = FunType(ExpType(dataType(rTransposeT.inT), ai),
          ExpType(dataType(rTransposeT.outT), ai))
        (transposeType, Subst())

      case rp.ToMem() =>
        val rToMemT = p.t.asInstanceOf[rt.FunType[rt.DataType, rt.DataType]]

        val toMemType = FunType(ExpType(dataType(rToMemT.inT), write),
          ExpType(dataType(rToMemT.outT), read))
        (toMemType, Subst())
    }
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

  def dataTypeIdentifier(dt: rt.DataTypeIdentifier): DataTypeIdentifier =
    DataTypeIdentifier(dt.name)
  def natIdentifier(n: rt.NatIdentifier): NatIdentifier =
    NatIdentifier(n.name, n.range)
  def addressSpaceIdentifier(
    a: rt.AddressSpaceIdentifier
  ): AddressSpaceIdentifier = AddressSpaceIdentifier(a.name)
  def natToNatIdentifier(n: rt.NatToNatIdentifier): NatToNatIdentifier =
    NatToNatIdentifier(n.name)
  def natToDataIdentifier(n: rt.NatToDataIdentifier): NatToDataIdentifier =
    NatToDataIdentifier(n.name)
  def accessTypeIdentifier(): AccessTypeIdentifier =
    AccessTypeIdentifier(freshName("access"))

  def `type`(ty: rt.Type): PhraseType = ty match {
    case dt: rt.DataType => ExpType(dataType(dt), read)
    case rt.FunType(i, o) => `type`(i) ->: `type`(o)
    case rt.DepFunType(i, t) => i match {
      case dt: rt.DataTypeIdentifier =>
        dataTypeIdentifier(dt) `()->:` `type`(t)
      case n: rt.NatIdentifier =>
        natIdentifier(n) `()->:` `type`(t)
      case n2n: rt.NatToNatIdentifier =>
        natToNatIdentifier(n2n) `()->:` `type`(t)
      case n2d: rt.NatToDataIdentifier =>
        natToDataIdentifier(n2d) `()->:` `type`(t)
    }
    case rt.TypeIdentifier(_) | rt.TypePlaceholder =>
      throw new Exception("This should not happen")
  }

  def dataType(t: rt.DataType): DataType = t match {
    case bt: rt.BasicType => basicType(bt)
    case i: rt.DataTypeIdentifier => dataTypeIdentifier(i)
    case rt.ArrayType(sz, et) => ArrayType(sz, dataType(et))
    case rt.DepArrayType(sz, f) => DepArrayType(sz, ntd(f))
    case rt.PairType(a, b) => PairType(dataType(a), dataType(b))
    case rt.NatToDataApply(f, n) => NatToDataApply(ntd(f), n)
  }

  def basicType(t: rt.BasicType): BasicType = t match {
    case st: rt.ScalarType => scalarType(st)
    case rt.IndexType(sz) => IndexType(sz)
    case rt.VectorType(sz, et) => et match {
      case e : rt.ScalarType => VectorType(sz, scalarType(e))
      case _ => ???
    }
  }

  def scalarType(t: rt.ScalarType): ScalarType = t match {
    case rt.bool => bool
    case rt.int => int
    case rt.i8 => i8
    case rt.i16 => i16
    case rt.i32 => i32
    case rt.i64 => i64
    case rt.u8 => u8
    case rt.u16 => u16
    case rt.u32 => u32
    case rt.u64 => u64
    case rt.f16 => f16
    case rt.f32 => f32
    case rt.f64 => f64
    case rt.NatType => NatType
  }

  def addressSpace(a: rt.AddressSpace): AddressSpace = a match {
    case rt.AddressSpace.Global => AddressSpace.Global
    case rt.AddressSpace.Local => AddressSpace.Local
    case rt.AddressSpace.Private => AddressSpace.Private
    case rt.AddressSpace.Constant => AddressSpace.Constant
    case rt.AddressSpaceIdentifier(name, _) => AddressSpaceIdentifier(name)
  }

  def ntd(ntd: rt.NatToData): NatToData= ntd match {
    case rt.NatToDataLambda(n, body) =>
      NatToDataLambda(natIdentifier(n), dataType(body))
    case rt.NatToDataIdentifier(x, _) => NatToDataIdentifier(x)
  }

  def ntn(ntn: rt.NatToNat): NatToNat= ntn match {
    case rt.NatToNatLambda(n, body) => NatToNatLambda(natIdentifier(n), body)
    case rt.NatToNatIdentifier(x, _) => NatToNatIdentifier(x)
  }
}
