package shine.DPIA

import rise.{core => r}
import rise.core.{Opaque, TypeAnnotation, TypeAssertion, primitives => rp, types => rt}
import rise.core.DSL.Type.{->:, ArrayTypeConstructors, TupleTypeConstructors, `(Addr)->:`, `(Nat)->:`, `(NatToNat)->:`, `.`, x}
import rise.openMP.{primitives => rompp}
import rise.openCL.{primitives => roclp}
import rise.Cuda.{primitives => rocup}
import shine.DPIA.Types._
import shine.DPIA.Types.TypeCheck.SubTypeCheckHelper
import shine.DPIA.fromRise._
import util.Execute.Exception

import scala.annotation.tailrec
import scala.collection.mutable
import scala.util.{Failure, Success, Try}

object inferAccess {
  def apply(e: r.Expr): java.util.IdentityHashMap[r.Expr, PhraseType] =
    new InferAccessAnnotation()(e)
}

private class InferAccessAnnotation {
  private val ptAnnotationMap =
    new java.util.IdentityHashMap[r.Expr, PhraseType]()

  def apply(e: r.Expr): java.util.IdentityHashMap[r.Expr, PhraseType] = {
    import scala.jdk.CollectionConverters.MapHasAsScala
    val (ePt, substAcc) =
      inferPhraseTypes(e, Map.empty, isKernelParamFun = true)
    substAcc(ptAnnotationMap.asScala)
    if (!funOutIsWrite(ePt))
      error("The program does not specify how to write the result " +
        s"of the program into output:\n $e")
    ptAnnotationMap
  }

  @tailrec
  private def funOutIsWrite(ePt: PhraseType): Boolean = ePt match {
    case DepFunType(_, _, t) => funOutIsWrite(t)
    case FunType(_, t) => funOutIsWrite(t)
    case expT: ExpType => expT `<=` ExpType(expT.dataType, write)
    case _ => throw error("This should never happen.")
  }

  private type Context = Predef.Map[r.Identifier, PhraseType]
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
      m.view.mapValues(apply).toMap

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

    def add(i: AccessTypeIdentifier, pt: AccessType): Subst = {
      if (substMap contains i)
        throw Exception(
          s"Substitution for phrase type identifier $i exists already.")
      else
        Subst(substMap.updated(i, pt))
    }
  }

  private def inferPhraseTypes(
    e: r.Expr,
    ctx: Context,
    isKernelParamFun: Boolean
  ): (PhraseType, Subst) = {
    val (pt, s) = e match {
      case i: r.Identifier =>
        val pt = ctx(i)
        ptAnnotationMap.put(i, pt)
        (pt, Subst())
      case lit: r.Literal =>
        val lpt = ExpType(dataType(e.t.asInstanceOf[rt.DataType]), read)
        ptAnnotationMap.put(lit, lpt)
        (lpt, Subst())
      case l: r.Lambda =>
        inferLambda(l, e.t.asInstanceOf[rt.FunType[rt.Type, _]].inT,
          ctx, isKernelParamFun)
      case appl: r.App =>
        inferApp(appl, ctx, addsKernelParam(e, isKernelParamFun))
      case depL: r.DepLambda[_, _, _] =>
        inferDepLambda(depL, ctx, isKernelParamFun)
      case depA: r.DepApp[_, _] =>
        inferDepApp(depA, ctx, addsKernelParam(e, isKernelParamFun))
      case _: TypeAnnotation => throw new Exception("Type annotations should be gone.")
      case _: TypeAssertion => throw new Exception("Type assertions should be gone.")
      case _: Opaque => throw new Exception("Opaque expressions should be gone.")
      case p: r.Primitive => inferPrimitive(p)
    }
    // the kernel output must be 'write'
    if (isKernelParamFun) {
      pt match {
        case et: ExpType => subUnifyPhraseType(et, ExpType(et.dataType, write)) match {
          case Success(subst) => return (subst(pt), subst(s))
          case Failure(exception) =>
            error(s"The program does not specify how to write the result " +
              s"of the program into its output: $exception")
        }
        case _ =>
      }
    }
    (pt, s)
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

    val ctxWithX = ctx.updated(lambda.x, xType)
    val (eType, eSubst) =
      inferPhraseTypes(lambda.e, ctxWithX, isKernelParamFun)

    val lambdaType = FunType(eSubst(xType), eType)
    ptAnnotationMap.put(lambda.x, xType)
    ptAnnotationMap.put(lambda, lambdaType)
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

    val subst = subUnifyPhraseType(eType, eSubstFType.inT) match {
      case Success(subst) => subst
      case Failure(exception) =>
        error(s"Failure when inferring access annotations for application:\n" +
          s"$app\nwith context:\n$ctx\n${exception.getMessage}")
    }
    val appType = subst(eSubstFType.outT)
    val resSubst = subst(eSubst(fSubst))
    ptAnnotationMap.put(app, appType)
    (appType, resSubst)
  }

  private def inferDepLambda(
    depLambda: r.DepLambda[_, _, _],
    ctx: Context,
    kernelParamFun: Boolean
  ): (PhraseType, Subst) = {
    val (eType, eSubst) = inferPhraseTypes(depLambda.e, ctx, kernelParamFun)
    val depLambdaType =
      depLambda.x match {
        case n: rt.NatIdentifier =>
          DepFunType(NatKind, natIdentifier(n), eType)
        case dt: rt.DataTypeIdentifier =>
          DepFunType(DataKind, dataTypeIdentifier(dt), eType)
        case ad: rt.AddressSpaceIdentifier =>
          DepFunType(AddressSpaceKind, addressSpaceIdentifier(ad), eType)
        case n2n: rt.NatToNatIdentifier =>
          DepFunType(NatToNatKind, natToNatIdentifier(n2n), eType)
        case n2d: rt.NatToDataIdentifier =>
          DepFunType(NatToDataKind, natToDataIdentifier(n2d), eType)
      }
    ptAnnotationMap.put(depLambda, depLambdaType)
    (depLambdaType, eSubst)
  }

  private def inferDepApp(
    depApp: r.DepApp[_, _],
    ctx: Context,
    kernelParamFun: Boolean
  ): (PhraseType, Subst) = {
    val (fType, fSubst) = inferPhraseTypes(depApp.f, ctx, kernelParamFun)
    val depAppType =
      depApp.x match {
        case dt: rt.DataType =>
          Lifting.liftDependentFunctionType[DataType](fType)(dataType(dt))
        case addr: rt.AddressSpace =>
          Lifting.liftDependentFunctionType[AddressSpace](fType)(
            addressSpace(addr))
        case n: rt.Nat =>
          Lifting.liftDependentFunctionType[Nat](fType)(n)
        case n2n: rt.NatToNat =>
          Lifting.liftDependentFunctionType[NatToNat](fType)(ntn(n2n))
        case n2d: rt.NatToData =>
          Lifting.liftDependentFunctionType[NatToData](fType)(ntd(n2d))
      }
    ptAnnotationMap.put(depApp, depAppType)
    (depAppType, fSubst)
  }

  private def inferPrimitive(p: r.Primitive): (PhraseType, Subst) = {
    val primitiveType = p match {
      case roclp.mapGlobal(_) | roclp.mapWorkGroup(_) | roclp.mapLocal(_)
           | rocup.mapGlobal(_) | rocup.mapBlock(_) | rocup.mapThreads(_)
           | rocup.mapWarp(_) | rocup.mapLane(_) |rompp.mapPar()
           | rp.mapSeq() | rp.mapSeqUnroll() | rp.iterateStream() => p.t match {
        case ((s: rt.DataType) ->: (t: rt.DataType)) ->: (n`.`_) ->: (_`.`_) =>
          (expT(s, read) ->: expT(t, write)) ->:
            expT(n`.`s, read) ->: expT(n`.`t, write)
        case _ => error()
      }

      case rp.map() => p.t match {
        case ((s: rt.DataType) ->: (t: rt.DataType)) ->: (n`.`_) ->: (_`.`_) =>

          val ai = accessTypeIdentifier()
          (expT(s, ai) ->: expT(t, ai)) ->: expT(n`.`s, ai) ->: expT(n`.`t, ai)
        case _ => error()
      }

      case rp.mapFst() => p.t match {
        case ((dt1: rt.DataType) ->: (dt3: rt.DataType)) ->:
          (_ x dt2) ->: (_ x _) =>

          val ai = accessTypeIdentifier()
          (expT(dt1, ai) ->: expT(dt3, ai)) ->:
            expT(dt1 x dt2, ai) ->: expT(dt3 x dt2, ai)
        case _ => error()
      }

      case rp.mapSnd() => p.t match {
        case ((dt2: rt.DataType) ->: (dt3: rt.DataType)) ->:
          (dt1 x _) ->: (_ x _) =>

          val ai = accessTypeIdentifier()
          (expT(dt2, ai) ->: expT(dt3, ai)) ->:
            expT(dt1 x dt2, ai) ->: expT(dt1 x dt3, ai)
        case _ => error()
      }

      case rp.mapStream() => p.t match {
        case ((s: rt.DataType) ->: (t: rt.DataType)) ->: (n`.`_) ->: (_`.`_) =>

          (expT(s, read) ->: expT(t, write)) ->:
            expT(n`.`s, read) ->: expT(n`.`t, read)
        case _ => error()
      }

      case rp.toMem() => p.t match {
        case (t: rt.DataType) ->: (_: rt.DataType) =>
          expT(t, write) ->: expT(t, read)
        case _ => error()
      }

      case roclp.oclRunPrimitive() => p.t match {
        case ls1 `(Nat)->:` (ls2 `(Nat)->:` (ls3 `(Nat)->:`
          (gs1 `(Nat)->:` (gs2 `(Nat)->:` (gs3 `(Nat)->:`
          ((t: rt.DataType) ->: (_: rt.DataType))
        ))))) =>
          nFunT(ls1, nFunT(ls2, nFunT(ls3,
            nFunT(gs1, nFunT(gs2, nFunT(gs3,
              expT(t, write) ->: expT(t, write)))))))
        case _ => error()
      }

      case roclp.oclToMem() => p.t match {
        case a `(Addr)->:` ((t: rt.DataType) ->: (_: rt.DataType)) =>
          aFunT(a, expT(t, write) ->: expT(t, read))
        case _ => error()
      }

      case rp.join() | rp.transpose() | rp.asScalar()
           | rp.unzip() => p.t match {
        case (dt1: rt.DataType) ->: (dt2: rt.DataType) =>
          val ai = accessTypeIdentifier()
          expT(dt1, ai) ->: expT(dt2, ai)
        case _ => error()
      }

      case rp.vectorFromScalar() | rp.neg() | rp.not()
           | rp.indexAsNat() | rp.fst() | rp.snd()  | rp.cast() => p.t match {
        case (dt1: rt.DataType) ->: (dt2: rt.DataType) =>
          expT(dt1, read) ->: expT(dt2, read)
        case _ => error()
      }

      case rp.concat() => p.t match {
        case (dt1: rt.DataType) ->: (dt2: rt.DataType) ->: (dt3: rt.DataType) =>
          expT(dt1, write) ->: expT(dt2, write) ->: expT(dt3, write)
        case _ => error()
      }

      case rp.let() => p.t match {
        case (s: rt.DataType) ->:
          ((_: rt.DataType) ->: (t: rt.DataType)) ->: (_: rt.DataType) =>

          val ai = accessTypeIdentifier()
          expT(s, read) ->: (expT(s, read) ->: expT(t, ai)) ->: expT(t, ai)
        case _ => error()
      }

      case rp.split() | rp.asVector() | rp.asVectorAligned() => p.t match {
        case n `(Nat)->:` ((dt1: rt.DataType) ->: (dt2: rt.DataType)) =>

          val ai = accessTypeIdentifier()
          nFunT(n, expT(dt1, ai) ->: expT(dt2, ai))
        case _ => error()
      }

      case rp.zip() | rp.makePair() => p.t match {
        case (dt1: rt.DataType) ->: (dt2: rt.DataType) ->: (dt3: rt.DataType) =>

          val ai = accessTypeIdentifier()
          expT(dt1, ai) ->: expT(dt2, ai) ->: expT(dt3, ai)
        case _ => error()
      }

      case rp.idx() | rp.add() | rp.sub() | rp.mul() | rp.div() | rp.gt()
           | rp.lt() | rp.equal() | rp.mod() | rp.gather() => p.t match {
        case (dt1: rt.DataType) ->: (dt2: rt.DataType) ->: (dt3: rt.DataType) =>
          expT(dt1, read) ->: expT(dt2, read) ->: expT(dt3, read)
        case _ => error()
      }

      case rp.scatter() => p.t match {
        case (dt1: rt.DataType) ->: (dt2: rt.DataType) ->: (dt3: rt.DataType) =>
          expT(dt1, read) ->: expT(dt2, write) ->: expT(dt3, write)
        case _ => error()
      }

      case rp.natAsIndex() | rp.take() | rp.drop() => p.t match {
        case n `(Nat)->:` ((dt1: rt.DataType) ->: (dt2: rt.DataType)) =>
          nFunT(n, expT(dt1, read) ->: expT(dt2, read))
        case _ => error()
      }

      case rp.reduceSeq() | rp.reduceSeqUnroll() => p.t match {
        case ((t: rt.DataType) ->: (s: rt.DataType) ->: (_: rt.DataType)) ->:
          (_: rt.DataType) ->: (n`.`_) ->: (_: rt.DataType) =>

          (expT(t, read) ->: expT(s, read) ->: expT(t, write)) ->:
            expT(t, write) ->: expT(n`.`s, read) ->: expT(t, read)
        case _ => error()
      }

      case  rp.scanSeq() => p.t match {
        case ((s: rt.DataType) ->: (t: rt.DataType) ->: (_: rt.DataType)) ->:
          (_: rt.DataType) ->: (n`.`_) ->: (_`.`_) =>

          (expT(s, read) ->: expT(t, read) ->: expT(t, write)) ->:
            expT(t, write) ->: expT(n`.`s, read) ->: expT(n`.`t, write)
        case _ => error()
      }

      case roclp.oclReduceSeq() | roclp.oclReduceSeqUnroll() => p.t match {
        case a `(Addr)->:`
          (((t: rt.DataType) ->: (s: rt.DataType) ->: (_: rt.DataType)) ->:
            (_: rt.DataType) ->: (n`.`_) ->: (_: rt.DataType)) =>

          aFunT(a,
            (expT(t, read) ->: expT(s, read) ->: expT(t, write)) ->:
            expT(t, write) ->: expT(n`.`s, read) ->: expT(t, read))
        case _ => error()
      }

      case rp.depTile() => p.t match {
        case tile `(Nat)->:`
          (((s: rt.DataType) ->: (t: rt.DataType)) ->:
            (inT: rt.ArrayType) ->: (outT: rt.ArrayType)) =>
          nFunT(tile,
            (expT(s, read) ->: expT(t, write)) ->:
            expT(inT, read) ->: expT(outT, write))
        case _ => error()
      }

        //TODO Circular Buffer and OCL versions
      case rp.rotateValues() => p.t match {
        case  sz `(Nat)->:`
          (((s: rt.DataType) ->: (_: rt.DataType)) ->:
            (inT: rt.ArrayType) ->: (outT: rt.ArrayType)) =>
          nFunT(sz,
            (expT(s, read) ->: expT(s, write)) ->:
            expT(inT, read) ->: expT(outT, read))
        case _ => error()
      }

      case rp.circularBuffer() => p.t match {
        case alloc `(Nat)->:` (sz `(Nat)->:`
          (((s: rt.DataType) ->: (t: rt.DataType)) ->:
            (inT: rt.ArrayType) ->: (outT: rt.ArrayType))) =>
          nFunT(alloc, nFunT(sz,
            (expT(s, read) ->: expT(t, write)) ->:
            expT(inT, read) ->: expT(outT, read)))
        case _ => error()
      }

      case roclp.oclRotateValues() => p.t match {
        case a `(Addr)->:` (sz `(Nat)->:`
          (((s: rt.DataType) ->: (_: rt.DataType)) ->:
            (inT: rt.ArrayType) ->: (outT: rt.ArrayType))) =>
          aFunT(a,
            nFunT(sz,
              (expT(s, read) ->: expT(s, write)) ->:
              expT(inT, read) ->: expT(outT, read)))
        case _ => error()
      }

      case roclp.oclCircularBuffer() => p.t match {
        case a `(Addr)->:` (alloc `(Nat)->:` (sz `(Nat)->:`
          (((s: rt.DataType) ->: (t: rt.DataType)) ->:
            (inT: rt.ArrayType) ->: (outT: rt.ArrayType)))) =>

          aFunT(a, nFunT(alloc, nFunT(sz,
            (expT(s, read) ->: expT(t, write)) ->:
              expT(inT, read) ->: expT(outT, read))))
        case _ => error()
      }

      case rp.slide() | rp.padClamp() => p.t match {
        case sz `(Nat)->:` (sp `(Nat)->:`
          ((dt1: rt.DataType) ->: (dt2: rt.DataType))) =>
          nFunT(sz, nFunT(sp,
            expT(dt1, read) ->: expT(dt2, read)))
        case _ => error()
      }

      case rp.iterate() => p.t match {
        case k `(Nat)->:`
          ((l `(Nat)->:` ((at1: rt.ArrayType) ->: (at2: rt.ArrayType))) ->:
            (at3: rt.ArrayType) ->: (at4: rt.ArrayType)) =>
          nFunT(k,
            nFunT(l, expT(at1, read) ->: expT(at2, write)) ->:
            expT(at3, read) ->: expT(at4, write) )
        case _ => error()
      }

      case roclp.oclIterate() => p.t match {
        case a `(Addr)->:` (k `(Nat)->:`
          ((l `(Nat)->:` ((at1: rt.ArrayType) ->: (at2: rt.ArrayType))) ->:
            (at3: rt.ArrayType) ->: (at4: rt.ArrayType))) =>
          aFunT(a, nFunT(k,
            nFunT(l, expT(at1, read) ->: expT(at2, write)) ->:
              expT(at3, read) ->: expT(at4, write) ))
        case _ => error()
      }

      case rp.select() => p.t match {
        case rt.bool ->: (t: rt.DataType) ->:
          (_: rt.DataType) ->: (_: rt.DataType) =>

          expT(rt.bool, read) ->:
            expT(t, read) ->: expT(t, read) ->: expT(t, read)
        case _ => error()
      }

      case rp.padEmpty() => p.t match {
        case r `(Nat)->:` ((n`.`t) ->: (_`.`_)) =>
          nFunT(r, expT(n`.`t, write) ->: expT((n + r)`.`t, write))
        case _ => error()
      }

      case rp.padCst() => p.t match {
        case l `(Nat)->:` (q `(Nat)->:`
          ((t: rt.DataType) ->: (n`.`_) ->: (_`.`_))) =>

          nFunT(l, nFunT(q,
            expT(t, read) ->: expT(n`.`t, read) ->:
              expT((l + n + q)`.`t, read)))
        case _ => error()
      }

      case rp.generate() => p.t match {
        case (rt.IndexType(n) ->: (t: rt.DataType)) ->: (_`.`_) =>
          (expT(rt.IndexType(n), read) ->: expT(t, read)) ->:
            expT(n`.`t, read)
        case _ => error()
      }

      case rp.reorder() => p.t match {
        case (n `(Nat)->:` (idxF `(NatToNat)->:` (idxFinv `(NatToNat)->:` ((_`.`t) ->: (_`.`_) )))) =>

          val ai = accessTypeIdentifier()
          nFunT(n, n2nFunT(idxF, n2nFunT(idxFinv, expT(n`.`t, ai) ->: expT(n`.`t, ai))))
        case _ => error()
      }

      case rp.foreignFunction(_, _) =>
        def buildType(t: rt.Type): PhraseType = t match {
          case dt: rt.DataType =>
            expT(dataType(dt), read)
          case rt.FunType(in: rt.DataType, out) =>
            expT(in, read) ->: buildType(out)
          case rt.DepFunType(_, d: rt.DataTypeIdentifier, t) =>
            dFunT(d, buildType(t))
          case _ => throw Exception("This should not happen")
        }
        buildType(p.t)

      case rp.makeArray(_) =>
        def buildType(t: rt.Type): PhraseType = t match {
          case rt.FunType(in: rt.DataType, out) =>
            expT(dataType(in), read) ->: buildType(out)
          case n`.`dt => expT(n`.`dt, read)
          case _ => error(s"did not expect t")
        }
        buildType(p.t)

      case rp.depMapSeq() =>
        def buildType(t: rt.Type): PhraseType = t match {
          case rt.FunType(rt.DepFunType(_, i: rt.NatIdentifier, rt.FunType(elemInT:rt.DataType, elemOutT:rt.DataType)),
            rt.FunType(inArr@rt.DepArrayType(_, _), outArr@rt.DepArrayType(_, _))) =>
            val iNat = natIdentifier(i)
            nFunT(iNat, expT(dataType(elemInT), read) ->: expT(dataType(elemOutT), write)) ->:
              expT(dataType(inArr), read) ->: expT(dataType(outArr), write)
          case _ => error("did not expect t")
        }
        buildType(p.t)

      case rp.dmatch() =>
        val a = accessTypeIdentifier()
        def buildType(t: rt.Type): PhraseType = t match {
          case rt.FunType(rt.DepPairType(_, x: rt.NatIdentifier, elemT),
            rt.FunType(rt.DepFunType(_, i: rt.NatIdentifier,
              rt.FunType(app1:rt.DataType, outT:rt.DataType)), retT:rt.DataType)) =>

            val i_ = natIdentifier(i.asInstanceOf[rt.NatIdentifier])
            expT(DepPairType(natIdentifier(x), dataType(elemT)), read) ->:
              nFunT(i_, expT(dataType(app1), read) ->: expT(dataType(outT), a)) ->:
                expT(dataType(retT), a)
          case _ => error(s"did not expect t")
        }
        buildType(p.t)

      case rp.makeDepPair() =>
        def buildType(t: rt.Type): PhraseType = t match {
          case rt.DepFunType(_, fst: rt.NatIdentifier, rt.FunType(sndT:rt.DataType, outT:rt.DataType)) =>
            val a1 = accessTypeIdentifier()
            val fst_ = natIdentifier(fst)
            nFunT(fst_, expT(dataType(sndT), a1) ->: expT(dataType(outT), a1))

          case _ => error(s"did not expect $t")
        }
        buildType(p.t)

      case rocup.asFragment() => p.t match {
        case (aMatrix: rt.ArrayType) ->: (resultMatrix: rt.FragmentType) =>
          expT(aMatrix, read) ->: expT(resultMatrix, read)
      }

      case rocup.asMatrix() => p.t match {
        case (accMatrix: rt.FragmentType) ->: (resultArray: rt.ArrayType) =>
          expT(accMatrix, read) ->: expT(resultArray, write)
      }

      case rocup.generateFragment() => p.t match {
        case (dt: rt.DataType) ->: (resultMatrix: rt.FragmentType) =>
          expT(dt, read) ->: expT(resultMatrix, read)
      }

      case rocup.tensorMMA() => p.t match {
        case (aMatrix: rt.FragmentType) ->: (bMatrix: rt.FragmentType) ->:
          (cMatrix: rt.FragmentType) ->: (resultMatrix: rt.FragmentType) =>
          expT(aMatrix, read) ->: expT(bMatrix, read) ->: expT(cMatrix, read) ->: expT(resultMatrix, write)
      }

      case rocup.globalToShared() => p.t match {
        case (dt: rt.DataType) ->: _ =>
          expT(dt, write) ->: expT(dt, read)
      }

      case rocup.mapFragment() => p.t match {
        case ((dt: rt.DataType) ->: _) ->: (fragType: rt.FragmentType) ->: _ =>
          (expT(dt, read) ->: expT(dt, write)) ->: expT(fragType, read) ->: expT(fragType, write)
      }
    }

    checkConsistency(p.t, primitiveType)

    ptAnnotationMap.put(p, primitiveType)
    (primitiveType, Subst())
  }

  private def addsKernelParam(
    expr: r.Expr,
    kernelParamFun: Boolean
  ): Boolean =
    if (kernelParamFun)
      expr.t match {
        case _: rt.FunType[_, _] | _: rt.DepFunType[_, _, _, _] => true
        case _ => false
      }
    else false

  private def subUnifyPhraseType(
    less: PhraseType,
    larger: PhraseType,
  ): Try[Subst] = (less, larger) match {
    case (le@ExpType(ldt, la), re@ExpType(rdt, ra)) if ldt == rdt =>
      (la, ra) match {
        case (li: AccessTypeIdentifier, _) => Try(Subst().add(li, ra))
        case (_, ri: AccessTypeIdentifier) => Try(Subst().add(ri, la))
        case _ => if (le `<=` re) Try(Subst())
                  else Try(error(s"Cannot subunify $less <: $larger."))
      }
    case (FunType(lin, lout), FunType(rin, rout)) =>
      subUnifyPhraseType(rin, lin).flatMap(argSubst =>
        subUnifyPhraseType(argSubst(lout), argSubst(rout)).flatMap(outSubst =>
          Success(outSubst(argSubst))
        )
      )
    case (DepFunType(_, lx, la), DepFunType(_, rx, ra)) if lx == rx =>
      subUnifyPhraseType(la, ra)
    case _ => Try(error(s"Cannot subunify $less and $larger."))
  }

  def `type`(ty: rt.Type): PhraseType = ty match {
    case dt: rt.DataType => ExpType(dataType(dt), accessTypeIdentifier())
    case rt.FunType(i, o) => `type`(i) ->: `type`(o)
    case rt.DepFunType(_, i, t) => i match {
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
    case (rt.DepFunType(k, x, t), DepFunType(_, y, pt)) =>
      if (rt.Kind.idName(k, x) != y.name) error(s"Identifiers $x and $y differ")
      checkConsistency(t, pt)
    case (dt: rt.DataType, ExpType(dpt: DataType, _)) =>

    case _ => error(s"Types $t and $pt not compatible")
  }
}
