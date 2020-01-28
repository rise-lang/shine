package shine.DPIA

import rise.{core => r}
import rise.core.{Expr, types => rt}
import rise.core.types.Kind
import rise.core.primitives.MapSeq

import shine.DPIA.Types._
import shine.DPIA.{access => acc, RiseExprAnnotated => rea}
import shine.DPIA.access.AccessTypeAnnotation

import scala.language.implicitConversions

private case object inferDPIATypes {
  val mapSeqAccessType: AccessTypeAnnotation =
    acc.FunType(
      acc.FunType(acc.DataType(read), acc.DataType(write)),
      acc.FunType(acc.DataType(read), acc.DataType(write)))

  object a {
    type Expr = rea.Expr[acc.AccessTypeAnnotation]
    type Identifier = rea.Identifier[acc.AccessTypeAnnotation]
    type App = rea.App[acc.AccessTypeAnnotation]
    type DepApp[K <: rt.Kind] = rea.DepApp[acc.AccessTypeAnnotation, K]
    type Lambda = rea.Lambda[acc.AccessTypeAnnotation]
    type DepLambda[K <: rt.Kind] = rea.DepLambda[acc.AccessTypeAnnotation, K]
    type Literal = rea.Literal[acc.AccessTypeAnnotation]
    type Primitive = rea.Primitive[acc.AccessTypeAnnotation]
  }

  def apply(e: r.Expr): a.Expr =
    inferAccessTypeAnnotations(e, Map(), kernelParamFun = true)

  private type IdentMap = Map[r.Identifier, a.Identifier]

  private def inferAccessTypeAnnotations(
    e: r.Expr,
    ctx: IdentMap,
    kernelParamFun: Boolean
  ): a.Expr = {
    e match {
      case i: r.Identifier => ctx(i)
      case _: r.Literal => rea.Literal(acc.DataType(read))
      case r.Lambda(x, e) =>
        val annotatedDT =
          if (kernelParamFun) acc.DataType(read)
          else dataWithoutInferredAccessType()
        val annoId = rea.Identifier(x.name, annotatedDT)
        val idCtx = ctx updated(x, annoId)
        val annoBody = inferAccessTypeAnnotations(e, idCtx, kernelParamFun)
        val annoFunType = acc.FunType(annotatedDT, annoBody.meta)
        rea.Lambda(annoId, annoBody, annoFunType)
      case r.App(f, arg) =>
        inferAccessTypeAnnoForApp(
          f, arg, ctx, addsKernelParam(e, kernelParamFun))
      case r.DepLambda(x, e) =>
        inferAccessTypeAnnoForDepLambda(x, e, ctx, kernelParamFun)
      case r.DepApp(f, x) =>
        inferAccessTypeAnnoForDepApp(
          f, x, ctx, addsKernelParam(e, kernelParamFun))
      case p: r.Primitive => primitive(p, ctx)
    }
  }

  private def primitive(
    p: r.Primitive,
    ctx: IdentMap,
  ): a.Expr = {
    p match {
      case MapSeq() => rea.Primitive(mapSeqAccessType)
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

  private def inferAccessTypeAnnoForDepLambda(
    x: Kind#I with Kind.Explicitness,
    e: Expr,
    ctx: IdentMap,
    kernelParamFun: Boolean
  ): a.DepLambda[_] = {
    val annoBody = inferAccessTypeAnnotations(e, ctx, kernelParamFun)

    x match {
      case n: rt.NatIdentifier =>
        val accDepFunType = acc.DepFunType[rt.NatKind](n, annoBody.meta)
        rea.DepLambda[AccessTypeAnnotation, rt.NatKind](
          n, annoBody, accDepFunType)
      case dt: rt.DataTypeIdentifier =>
        val accDepFunType = acc.DepFunType[rt.DataKind](dt, annoBody.meta)
        rea.DepLambda[AccessTypeAnnotation, rt.DataKind](
          dt, annoBody, accDepFunType)
      case ad: rt.AddressSpaceIdentifier =>
        val accDepFunType = acc.DepFunType[rt.AddressSpaceKind](
          ad, annoBody.meta)
        rea.DepLambda[AccessTypeAnnotation, rt.AddressSpaceKind](
          ad, annoBody, accDepFunType)
      case n2n: rt.NatToNatIdentifier =>
        val accDepFunType = acc.DepFunType[rt.NatToNatKind](
          n2n, annoBody.meta)
        rea.DepLambda[AccessTypeAnnotation, rt.NatToNatKind](
          n2n, annoBody, accDepFunType)
    }
  }

  private def inferAccessTypeAnnoForDepApp(
    f: r.Expr,
    x: rt.Kind#T,
    ctx: IdentMap,
    kernelParamFun: Boolean
  ): a.DepApp[_] = {
    val annotatedFun = inferAccessTypeAnnotations(f, ctx, kernelParamFun)
    val accAppType = annotatedFun.meta.asInstanceOf[acc.DepFunType[_]].a
    rea.DepApp(annotatedFun, x, accAppType)
  }

  private def inferAccessTypeAnnoForApp(
    f: r.Expr,
    arg: r.Expr,
    ctx: IdentMap,
    kernelParamFun: Boolean
  ): a.App = {
    val annoArg = inferAccessTypeAnnotations(arg, ctx, kernelParamFun)
    val annoFun = inferAccessTypeAnnotations(f, ctx, kernelParamFun)
    val unifiedFun =
      unifyDPIATypeStructure(
        annoFun.meta,
        acc.FunType(annoArg.meta, acc.PlaceHolder))
          .asInstanceOf[acc.FunType]
    val substFun = substAccessTypeAnno(annoFun, unifiedFun)
    val substArg = substAccessTypeAnno(annoArg, unifiedFun.in)

    rea.App(substFun, substArg, unifiedFun.out)
  }

  private def substAccessTypeAnno(
    expr: a.Expr,
    accessTypeAnno: AccessTypeAnnotation
  ): a.Expr = {
    expr match {
      case i: a.Identifier =>
        rea.Identifier[AccessTypeAnnotation](i.name, accessTypeAnno)
      case l: a.Lambda =>
        val accFunType = accessTypeAnno.asInstanceOf[acc.FunType]
        val substIdent =
          substAccessTypeAnno(l.x, accFunType.in).asInstanceOf[a.Identifier]
        val substBody = substAccessTypeAnno(l.e, accFunType.out)
        rea.Lambda(substIdent, substBody, accessTypeAnno)
      case dl: a.DepLambda[_] =>
        val accDepFunType = accessTypeAnno.asInstanceOf[acc.DepFunType[_]]
        val substBody = substAccessTypeAnno(dl.e, accDepFunType.a)
        dl.x match {
          case n: rt.NatIdentifier =>
            rea.DepLambda[AccessTypeAnnotation, rt.NatKind](
              n, substBody, accessTypeAnno)
          case dt: rt.DataTypeIdentifier =>
            rea.DepLambda[AccessTypeAnnotation, rt.DataKind](
              dt, substBody, accessTypeAnno)
          case ad: rt.AddressSpaceIdentifier =>
            rea.DepLambda[AccessTypeAnnotation, rt.AddressSpaceKind](
              ad, substBody, accessTypeAnno)
          case n2n: rt.NatToNatIdentifier =>
            rea.DepLambda[AccessTypeAnnotation, rt.NatToNatKind](
              n2n, substBody, accessTypeAnno)
        }
      case app: a.App =>
        val funArgAccType = app.f.meta.asInstanceOf[acc.FunType].in
        val substFun =
          substAccessTypeAnno(app.f, acc.FunType(funArgAccType, accessTypeAnno))
        rea.App(substFun, app.e, accessTypeAnno)
      case depApp: a.DepApp[_] =>
        val substDepFun = depApp.x match {
          case n: rt.NatIdentifier =>
            substAccessTypeAnno(
              depApp.f,
              acc.DepFunType[rt.NatKind](n, accessTypeAnno))
          case dt: rt.DataTypeIdentifier =>
            substAccessTypeAnno(
              depApp.f,
              acc.DepFunType[rt.DataKind](dt, accessTypeAnno))
          case ad: rt.AddressSpaceIdentifier =>
            substAccessTypeAnno(
              depApp.f,
              acc.DepFunType[rt.AddressSpaceKind](ad, accessTypeAnno))
          case n2n: rt.NatToNatIdentifier =>
            substAccessTypeAnno(
              depApp.f,
              acc.DepFunType[rt.NatToNatKind](n2n, accessTypeAnno))
        }
        rea.DepApp(substDepFun, depApp.x, accessTypeAnno)
      //Literals are always read. No need to change anything.
      case l: a.Literal => l
      case p: a.Primitive => p //TODO same for primitives?
    }
  }

  private def dataWithoutInferredAccessType(): AccessTypeAnnotation =
    acc.DataType(AccessTypeIdentifier(freshName("access")))

  private def unifyDPIATypeStructure(
    l: AccessTypeAnnotation,
    r: AccessTypeAnnotation
  ): AccessTypeAnnotation = {
    import shine.DPIA.access._

    (l, r) match {
      case (ll, PlaceHolder) => ll
      case (PlaceHolder, rr) => rr
      case (DataType(la), DataType(ra)) =>
        (la, ra) match {
          //TODO this assumes that Identifiers cannot be introduced by DepLambdas
          // i.e., every AccessTypeIdentifier is `implicit`
          case (_: AccessTypeIdentifier,
                _: AccessTypeIdentifier) => DataType(la)
          case (_: AccessTypeIdentifier, _) => DataType(ra)
          case (_, _: AccessTypeIdentifier) => DataType(la)
          //FIXME ignores subtyping
          case (lla, rra) if lla == rra => DataType(lla)
        }
      case (FunType(lin, lout), FunType(rin, rout)) =>
        FunType(unifyDPIATypeStructure(lin, rin),
          unifyDPIATypeStructure(lout, rout))
      case (DepFunType(lx, la), DepFunType(rx, ra)) if lx == rx =>
        lx match {
          case n: rt.NatIdentifier =>
            DepFunType[rt.NatKind](n, unifyDPIATypeStructure(la, ra))
          case dt: rt.DataTypeIdentifier =>
            DepFunType[rt.DataKind](dt, unifyDPIATypeStructure(la, ra))
          case ad: rt.AddressSpaceIdentifier =>
            DepFunType[rt.AddressSpaceKind](ad, unifyDPIATypeStructure(la, ra))
          case n2n: rt.NatToNatIdentifier =>
            DepFunType[rt.NatToNatKind](n2n, unifyDPIATypeStructure(la, ra))
        }
    }
  }
}
