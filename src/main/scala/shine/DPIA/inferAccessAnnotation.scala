package shine.DPIA

import rise.{core => r}
import rise.core.{Expr, lifting, types => rt}
import rise.core.types.Kind
import rise.core.primitives.MapSeq
import shine.DPIA.Types._
import shine.DPIA.Types.TypeCheck._
import shine.DPIA.{RiseExprAnnotated => rea}
import shine.DPIA.fromRise._

import scala.annotation.tailrec
import scala.collection.mutable
import scala.language.implicitConversions

private case object inferAccessAnnotation {
//  val mapSeqAccessType: PhraseType =
//    FunType(
//      FunType(ExpType(read), DataType(write)),
//      FunType(DataType(read), DataType(write)))

  def apply(e: r.Expr): Map[r.Expr, PhraseType] = {
    inferPhraseTypes(e, Map.empty, isKernelParamFun = true)
    ptAnnotationMap.toMap
  }

  private type Context = Map[r.Identifier, PhraseType]

  private type SubstMap = Predef.Map[PhraseTypeIdentifier, PhraseType]
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

    def apply(s: Subst): Subst =
      Subst(s.substMap.map({ case (i, pt) =>
        (i, if (this.substMap contains i) this.substMap(i) else apply(pt))
      }))

    def +(i: PhraseTypeIdentifier, pt: PhraseType): Subst = {
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
    (e, e.t) match {
      case (i: r.Identifier, _) =>
        ptAnnotationMap update(i, ctx(i))
        (ctx(i), Subst())
      case (lit: r.Literal, dt: rt.DataType) =>
        val lpt = ExpType(dataType(dt), read)
        ptAnnotationMap update(lit, lpt)
        (lpt, Subst())
      case (l: r.Lambda, rt.FunType(inT, _)) =>
        lambda(l, inT, ctx, isKernelParamFun)
      case (appl: r.App, _) =>
        app(appl, ctx, addsKernelParam(e, isKernelParamFun))
      case (depL: r.DepLambda[_], _) =>
        depLambda(depL, ctx, isKernelParamFun)
      case (depA: r.DepApp[_], _) =>
        depApp(depA, ctx, addsKernelParam(e, isKernelParamFun))
      case (p: r.Primitive, dt: rt.DataType) => ??? //primitive(p, ctx)
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
      } else freshPhraseTypeIdentifier()

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
    val outT = freshPhraseTypeIdentifier()

    val subst =
      biunifyAccess(eSubst(fType), FunType(eType, outT), contraVariant = false)

    val appType = subst(outT)
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
          Lifting.liftDependentFunctionType(fType)(kind(dt))
        case addr: rt.AddressSpaceKind#T =>
          Lifting.liftDependentFunctionType(fType)(kind(addr))
        case n: rt.NatKind#T =>
          Lifting.liftDependentFunctionType(fType)(kind(n))
        case n2n: rt.NatToNatKind#T =>
          Lifting.liftDependentFunctionType(fType)(kind(n2n))
        case n2d: rt.NatToDataKind#T => ???
        case _ => ???
      }
    ptAnnotationMap update(depApp, depAppType)
    (depAppType, fSubst)
  }

  private def primitive(
    p: r.Primitive,
    ctx: Context,
  ): rea.Expr = {
    p match {
      case MapSeq() => ??? //rea.Primitive(mapSeqAccessType)
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

  private def freshPhraseTypeIdentifier(): PhraseTypeIdentifier =
    PhraseTypeIdentifier(freshName("free"))

  //FIXME occurs check missing!
  private def biunifyAccess(
    l: PhraseType,
    r: PhraseType,
    contraVariant: Boolean
  ): Subst = ??? //(l, r) match {
//      case (_, i: PhraseTypeIdentifier) => Subst()
//      case (i: PhraseTypeIdentifier, _) => Subst()
//      case (le@ExpType(ldt, la), re@ExpType(rdt, ra)) if ldt == rdt =>
//        (la, ra) match {
//          case (_, ri: AccessTypeIdentifier) => Subst(Map(ri, la))
//          case (li: AccessTypeIdentifier, _) => Subst(Map(li, ra))
//          case _ => ???
////            if (le `<=` re)
////              if (!contraVariant) re else le
////            else if (re `<=` le)
////              if (!contraVariant) le else re
////            else error(s"Cannot unify ${le} and ${re}.")
//        }
//      case (FunType(lin, lout), FunType(rin, rout)) =>
//        FunType(biunifyAccess(lin, rin, !contraVariant),
//          biunifyAccess(lout, rout, contraVariant))
//      case (DepFunType(lx, la), DepFunType(rx, ra)) if lx == rx =>
//        lx match {
//          case n: NatIdentifier =>
//            DepFunType[NatKind, PhraseType](
//              n, biunifyAccess(la, ra, contraVariant))
//          case dt: DataTypeIdentifier =>
//            DepFunType[DataKind, PhraseType](
//              dt, biunifyAccess(la, ra, contraVariant))
//          case ad: AddressSpaceIdentifier =>
//            DepFunType[AddressSpaceKind, PhraseType](
//              ad, biunifyAccess(la, ra, contraVariant))
//          case n2n: NatToNatIdentifier =>
//            DepFunType[NatToNatKind, PhraseType](
//              n2n, biunifyAccess(la, ra, contraVariant))
//        }
//    }

  def kind[K <: Kind](k: rt.Kind#T): K#T = k match {
    case addr: rt.AddressSpaceKind#T => addressSpace(addr).asInstanceOf[K#T]
    case dt: rt.DataKind#T => dataType(dt).asInstanceOf[K#T]
    case nat: rt.NatKind#T => nat.asInstanceOf[K#T]
    //TODO NatToNat and NatToData Missing!
    case _ => throw new Exception("This should never happen")
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
    case rt.float => float
    case rt.double => double
    case rt.NatType => NatType
  }

  def addressSpace(a: rt.AddressSpace): AddressSpace = a match {
    case rt.AddressSpace.Global => AddressSpace.Global
    case rt.AddressSpace.Local => AddressSpace.Local
    case rt.AddressSpace.Private => AddressSpace.Private
    case rt.AddressSpace.Constant => AddressSpace.Constant
    case rt.AddressSpaceIdentifier(name, _) => AddressSpaceIdentifier(name)
  }
}
