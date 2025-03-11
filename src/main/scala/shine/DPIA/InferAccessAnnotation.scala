package shine.DPIA

import rise.{core => r}
import rise.core.{Opaque, TypeAnnotation, TypeAssertion, primitives => rp, types => rt}
import rise.core.types.{Access, AccessIdentifier, DataType, NatKind, read, write}
import rise.core.types.DataType._
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
  private type SubstMap = Predef.Map[AccessIdentifier, Access]
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
          shine.DPIA.Types.substitute(substMap.head._2, substMap.head._1, pt)
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
        case (ai, ati: AccessIdentifier) =>
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

    def add(i: AccessIdentifier, pt: Access): Subst = {
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
        val lpt = ExpType(e.t.asInstanceOf[DataType], read)
        ptAnnotationMap.put(lit, lpt)
        (lpt, Subst())
      case l: r.Lambda =>
        inferLambda(l, e.t.asInstanceOf[rt.FunType[rt.ExprType, _]].inT,
          ctx, isKernelParamFun)
      case appl: r.App =>
        inferApp(appl, ctx, addsKernelParam(e, isKernelParamFun))
      case depL: r.DepLambda[_, _] =>
        inferDepLambda(depL, ctx, isKernelParamFun)
      case depA: r.DepApp[_] =>
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
                           inT: rt.ExprType,
                           ctx: Context,
                           isKernelParamFun: Boolean
  ): (PhraseType, Subst) = {
    val xType =
      if (isKernelParamFun) {
        //Functions that define kernel parameters are allowed to take
        //arguments of ExpType only.
        val dt = inT.asInstanceOf[DataType]
        ExpType(dt, read)
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
    depLambda: r.DepLambda[_, _],
    ctx: Context,
    kernelParamFun: Boolean
  ): (PhraseType, Subst) = {
    val (eType, eSubst) = inferPhraseTypes(depLambda.e, ctx, kernelParamFun)
    val depLambdaType = DepFunType(depLambda.kind, depLambda.x, eType)
    ptAnnotationMap.put(depLambda, depLambdaType)
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
        case dt: DataType =>       Lifting.liftDependentFunctionType[DataType](fType)(dt)
        case addr: rt.AddressSpace => Lifting.liftDependentFunctionType[rt.AddressSpace](fType)(addr)
        case n: rt.Nat =>             Lifting.liftDependentFunctionType[Nat](fType)(n)
        case n2n: rt.NatToNat =>      Lifting.liftDependentFunctionType[rt.NatToNat](fType)(n2n)
        case n2d: rt.NatToData =>     Lifting.liftDependentFunctionType[rt.NatToData](fType)(n2d)
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
        case ((s: DataType) ->: (t: DataType)) ->: (n`.`_) ->: (_`.`_) =>
          (expT(s, read) ->: expT(t, write)) ->:
            expT(n`.`s, read) ->: expT(n`.`t, write)
        case _ => error()
      }

      case rp.map() => p.t match {
        case ((s: DataType) ->: (t: DataType)) ->: (n`.`_) ->: (_`.`_) =>

          val ai = accessTypeIdentifier()
          (expT(s, ai) ->: expT(t, ai)) ->: expT(n`.`s, ai) ->: expT(n`.`t, ai)
        case _ => error()
      }

      case rp.mapFst() => p.t match {
        case ((dt1: DataType) ->: (dt3: DataType)) ->:
          (_ x dt2) ->: (_ x _) =>

          val ai = accessTypeIdentifier()
          (expT(dt1, ai) ->: expT(dt3, ai)) ->:
            expT(dt1 x dt2, ai) ->: expT(dt3 x dt2, ai)
        case _ => error()
      }

      case rp.mapSnd() => p.t match {
        case ((dt2: DataType) ->: (dt3: DataType)) ->:
          (dt1 x _) ->: (_ x _) =>

          val ai = accessTypeIdentifier()
          (expT(dt2, ai) ->: expT(dt3, ai)) ->:
            expT(dt1 x dt2, ai) ->: expT(dt1 x dt3, ai)
        case _ => error()
      }

      case rp.mapStream() => p.t match {
        case ((s: DataType) ->: (t: DataType)) ->: (n`.`_) ->: (_`.`_) =>

          (expT(s, read) ->: expT(t, write)) ->:
            expT(n`.`s, read) ->: expT(n`.`t, read)
        case _ => error()
      }

      case rp.toMem() => p.t match {
        case (t: DataType) ->: (_: DataType) =>
          expT(t, write) ->: expT(t, read)
        case _ => error()
      }

      case roclp.oclRunPrimitive() => p.t match {
        case ls1 `(Nat)->:` (ls2 `(Nat)->:` (ls3 `(Nat)->:`
          (gs1 `(Nat)->:` (gs2 `(Nat)->:` (gs3 `(Nat)->:`
          ((t: DataType) ->: (_: DataType))
        ))))) =>
          nFunT(ls1, nFunT(ls2, nFunT(ls3,
            nFunT(gs1, nFunT(gs2, nFunT(gs3,
              expT(t, write) ->: expT(t, write)))))))
        case _ => error()
      }

      case roclp.oclToMem() => p.t match {
        case a `(Addr)->:` ((t: DataType) ->: (_: DataType)) =>
          aFunT(a, expT(t, write) ->: expT(t, read))
        case _ => error()
      }

      case rp.join() | rp.transpose() | rp.asScalar()
           | rp.unzip() => p.t match {
        case (dt1: DataType) ->: (dt2: DataType) =>
          val ai = accessTypeIdentifier()
          expT(dt1, ai) ->: expT(dt2, ai)
        case _ => error()
      }

      case rp.vectorFromScalar() | rp.neg() | rp.not()
           | rp.indexAsNat() | rp.fst() | rp.snd()  | rp.cast() => p.t match {
        case (dt1: DataType) ->: (dt2: DataType) =>
          expT(dt1, read) ->: expT(dt2, read)
        case _ => error()
      }

      case rp.concat() => p.t match {
        case (dt1: DataType) ->: (dt2: DataType) ->: (dt3: DataType) =>
          expT(dt1, write) ->: expT(dt2, write) ->: expT(dt3, write)
        case _ => error()
      }

      case rp.let() => p.t match {
        case (s: DataType) ->:
          ((_: DataType) ->: (t: DataType)) ->: (_: DataType) =>

          val ai = accessTypeIdentifier()
          expT(s, read) ->: (expT(s, read) ->: expT(t, ai)) ->: expT(t, ai)
        case _ => error()
      }

      case rp.split() | rp.asVector() | rp.asVectorAligned() => p.t match {
        case n `(Nat)->:` ((dt1: DataType) ->: (dt2: DataType)) =>

          val ai = accessTypeIdentifier()
          nFunT(n, expT(dt1, ai) ->: expT(dt2, ai))
        case _ => error()
      }

      case rp.zip() | rp.makePair() => p.t match {
        case (dt1: DataType) ->: (dt2: DataType) ->: (dt3: DataType) =>

          val ai = accessTypeIdentifier()
          expT(dt1, ai) ->: expT(dt2, ai) ->: expT(dt3, ai)
        case _ => error()
      }

      case rp.idx() | rp.add() | rp.sub() | rp.mul() | rp.div() | rp.gt()
           | rp.lt() | rp.equal() | rp.mod() | rp.gather() => p.t match {
        case (dt1: DataType) ->: (dt2: DataType) ->: (dt3: DataType) =>
          expT(dt1, read) ->: expT(dt2, read) ->: expT(dt3, read)
        case _ => error()
      }

      case rp.scatter() => p.t match {
        case (dt1: DataType) ->: (dt2: DataType) ->: (dt3: DataType) =>
          expT(dt1, read) ->: expT(dt2, write) ->: expT(dt3, write)
        case _ => error()
      }

      case rp.natAsIndex() | rp.take() | rp.drop() => p.t match {
        case n `(Nat)->:` ((dt1: DataType) ->: (dt2: DataType)) =>
          nFunT(n, expT(dt1, read) ->: expT(dt2, read))
        case _ => error()
      }

      case rp.reduceSeq() | rp.reduceSeqUnroll() => p.t match {
        case ((t: DataType) ->: (s: DataType) ->: (_: DataType)) ->:
          (_: DataType) ->: (n`.`_) ->: (_: DataType) =>

          (expT(t, read) ->: expT(s, read) ->: expT(t, write)) ->:
            expT(t, write) ->: expT(n`.`s, read) ->: expT(t, read)
        case _ => error()
      }

      case  rp.scanSeq() => p.t match {
        case ((s: DataType) ->: (t: DataType) ->: (_: DataType)) ->:
          (_: DataType) ->: (n`.`_) ->: (_`.`_) =>

          (expT(s, read) ->: expT(t, read) ->: expT(t, write)) ->:
            expT(t, write) ->: expT(n`.`s, read) ->: expT(n`.`t, write)
        case _ => error()
      }

      case roclp.oclReduceSeq() | roclp.oclReduceSeqUnroll() => p.t match {
        case a `(Addr)->:`
          (((t: DataType) ->: (s: DataType) ->: (_: DataType)) ->:
            (_: DataType) ->: (n`.`_) ->: (_: DataType)) =>

          aFunT(a,
            (expT(t, read) ->: expT(s, read) ->: expT(t, write)) ->:
            expT(t, write) ->: expT(n`.`s, read) ->: expT(t, read))
        case _ => error()
      }

      case rp.depTile() => p.t match {
        case tile `(Nat)->:`
          (((s: DataType) ->: (t: DataType)) ->:
            (inT: ArrayType) ->: (outT: ArrayType)) =>
          nFunT(tile,
            (expT(s, read) ->: expT(t, write)) ->:
            expT(inT, read) ->: expT(outT, write))
        case _ => error()
      }

      case rp.rotateValues() => p.t match {
        case  sz `(Nat)->:`
          (((s: DataType) ->: (_: DataType)) ->:
            (inT: ArrayType) ->: (outT: ArrayType)) =>
          nFunT(sz,
            (expT(s, read) ->: expT(s, write)) ->:
            expT(inT, read) ->: expT(outT, read))
        case _ => error()
      }

      case rp.circularBuffer() => p.t match {
        case alloc `(Nat)->:` (sz `(Nat)->:`
          (((s: DataType) ->: (t: DataType)) ->:
            (inT: ArrayType) ->: (outT: ArrayType))) =>
          nFunT(alloc, nFunT(sz,
            (expT(s, read) ->: expT(t, write)) ->:
            expT(inT, read) ->: expT(outT, read)))
        case _ => error()
      }

      case roclp.oclRotateValues() => p.t match {
        case a `(Addr)->:` (sz `(Nat)->:`
          (((s: DataType) ->: (_: DataType)) ->:
            (inT: ArrayType) ->: (outT: ArrayType))) =>
          aFunT(a,
            nFunT(sz,
              (expT(s, read) ->: expT(s, write)) ->:
              expT(inT, read) ->: expT(outT, read)))
        case _ => error()
      }

      case roclp.oclCircularBuffer() => p.t match {
        case a `(Addr)->:` (alloc `(Nat)->:` (sz `(Nat)->:`
          (((s: DataType) ->: (t: DataType)) ->:
            (inT: ArrayType) ->: (outT: ArrayType)))) =>

          aFunT(a, nFunT(alloc, nFunT(sz,
            (expT(s, read) ->: expT(t, write)) ->:
              expT(inT, read) ->: expT(outT, read))))
        case _ => error()
      }

      case rp.slide() | rp.padClamp() => p.t match {
        case sz `(Nat)->:` (sp `(Nat)->:`
          ((dt1: DataType) ->: (dt2: DataType))) =>
          nFunT(sz, nFunT(sp,
            expT(dt1, read) ->: expT(dt2, read)))
        case _ => error()
      }

      case rp.iterate() => p.t match {
        case k `(Nat)->:`
          ((l `(Nat)->:` ((at1: ArrayType) ->: (at2: ArrayType))) ->:
            (at3: ArrayType) ->: (at4: ArrayType)) =>
          nFunT(k,
            nFunT(l, expT(at1, read) ->: expT(at2, write)) ->:
            expT(at3, read) ->: expT(at4, write) )
        case _ => error()
      }

      case roclp.oclIterate() => p.t match {
        case a `(Addr)->:` (k `(Nat)->:`
          ((l `(Nat)->:` ((at1: ArrayType) ->: (at2: ArrayType))) ->:
            (at3: ArrayType) ->: (at4: ArrayType))) =>
          aFunT(a, nFunT(k,
            nFunT(l, expT(at1, read) ->: expT(at2, write)) ->:
              expT(at3, read) ->: expT(at4, write) ))
        case _ => error()
      }

      case rp.select() => p.t match {
        case `bool` ->: (t: DataType) ->:
          (_: DataType) ->: (_: DataType) =>

          expT(bool, read) ->:
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
          ((t: DataType) ->: (n`.`_) ->: (_`.`_))) =>

          nFunT(l, nFunT(q,
            expT(t, read) ->: expT(n`.`t, read) ->:
              expT((l + n + q)`.`t, read)))
        case _ => error()
      }

      case rp.generate() => p.t match {
        case (IndexType(n) ->: (t: DataType)) ->: (_`.`_) =>
          (expT(IndexType(n), read) ->: expT(t, read)) ->:
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
        def buildType(t: rt.ExprType): PhraseType = t match {
          case dt: DataType => expT(dt, read)
          case rt.FunType(in: DataType, out) => expT(in, read) ->: buildType(out)
          case rt.DepFunType(rt.DataKind, d: DataTypeIdentifier, t) => dFunT(d, buildType(t))
          case _ => throw Exception("This should not happen")
        }
        buildType(p.t)

      case rp.makeArray(_) =>
        def buildType(t: rt.ExprType): PhraseType = t match {
          case rt.FunType(in: DataType, out) => expT(in, read) ->: buildType(out)
          case n`.`dt => expT(n`.`dt, read)
          case _ => error(s"did not expect t")
        }
        buildType(p.t)

      case rp.depMapSeq() =>
        def buildType(t: rt.ExprType): PhraseType = t match {
          case rt.FunType(rt.DepFunType(rt.NatKind, i: rt.NatIdentifier, rt.FunType(elemInT:DataType, elemOutT:DataType)),
            rt.FunType(inArr@DepArrayType(_, _), outArr@DepArrayType(_, _))) =>
            nFunT(i, expT(elemInT, read) ->: expT(elemOutT, write)) ->:
              expT(inArr, read) ->: expT(outArr, write)
          case _ => error("did not expect t")
        }
        buildType(p.t)

      case rp.dmatch() =>
        val a = accessTypeIdentifier()
        def buildType(t: rt.ExprType): PhraseType = t match {
          case rt.FunType(DepPairType(_, x: rt.NatIdentifier, elemT),
            rt.FunType(rt.DepFunType(rt.NatKind, i: rt.NatIdentifier,
              rt.FunType(app1:DataType, outT:DataType)), retT:DataType)) =>

            expT(DepPairType(NatKind, x, elemT), read) ->:
              nFunT(i, expT(app1, read) ->: expT(outT, a)) ->:
                expT(retT, a)
          case _ => error(s"did not expect t")
        }
        buildType(p.t)

      case rp.makeDepPair() =>
        def buildType(t: rt.ExprType): PhraseType = t match {
          case rt.DepFunType(rt.NatKind, fst: rt.NatIdentifier, rt.FunType(sndT:DataType, outT:DataType)) =>
            val a1 = accessTypeIdentifier()
            nFunT(fst, expT(sndT, a1) ->: expT(outT, a1))

          case _ => error(s"did not expect $t")
        }
        buildType(p.t)

      case rocup.asFragment() => (p.t : @unchecked) match {
        case (aMatrix: ArrayType) ->: (resultMatrix: FragmentType) =>
          expT(aMatrix, read) ->: expT(resultMatrix, read)
      }

      case rocup.asMatrix() => (p.t : @unchecked) match {
        case (accMatrix: FragmentType) ->: (resultArray: ArrayType) =>
          expT(accMatrix, read) ->: expT(resultArray, write)
      }

      case rocup.generateFragment() => (p.t : @unchecked) match {
        case (dt: DataType) ->: (resultMatrix: FragmentType) =>
          expT(dt, read) ->: expT(resultMatrix, read)
      }

      case rocup.tensorMMA() => (p.t : @unchecked) match {
        case (aMatrix: FragmentType) ->: (bMatrix: FragmentType) ->:
          (cMatrix: FragmentType) ->: (resultMatrix: FragmentType) =>
          expT(aMatrix, read) ->: expT(bMatrix, read) ->: expT(cMatrix, read) ->: expT(resultMatrix, write)
      }

      case rocup.globalToShared() => (p.t : @unchecked) match {
        case (dt: DataType) ->: _ =>
          expT(dt, write) ->: expT(dt, read)
      }

      case rocup.mapFragment() =>  (p.t : @unchecked) match {
        case ((dt: DataType) ->: _) ->: (fragType: FragmentType) ->: _ =>
          (expT(dt, read) ->: expT(dt, write)) ->: expT(fragType, read) ->: expT(fragType, write)
      }

      case rise.GAP8.primitives.gap8RunPrimitive() => p.t match {
        case cores `(Nat)->:` ((t: rt.DataType) ->: (_: rt.DataType)) =>
          nFunT(cores, expT(t, write) ->: expT(t, write))
        case _ => error()
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
        case _: rt.FunType[_, _] | _: rt.DepFunType[_, _, _] => true
        case _ => false
      }
    else false

  private def subUnifyPhraseType(
    less: PhraseType,
    larger: PhraseType,
  ): Try[Subst] = (less, larger) match {
    case (le@ExpType(ldt, la), re@ExpType(rdt, ra)) if ldt == rdt =>
      (la, ra) match {
        case (li: AccessIdentifier, _) => Try(Subst().add(li, ra))
        case (_, ri: AccessIdentifier) => Try(Subst().add(ri, la))
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

  def `type`(ty: rt.ExprType): PhraseType = ty match {
    case dt: DataType => ExpType(dt, accessTypeIdentifier())
    case rt.FunType(i, o) => `type`(i) ->: `type`(o)
    case rt.DepFunType(_, i, t) => i match {
      case dt: DataTypeIdentifier => dt ->: `type`(t)
      case n: rt.NatIdentifier => n ->: `type`(t)
      case n2n: rt.NatToNatIdentifier => n2n ->: `type`(t)
      case n2d: rt.NatToDataIdentifier => n2d ->: `type`(t)
    }
    case rt.TypeIdentifier(_) | rt.TypePlaceholder => error()
  }

  private def checkConsistency(t: rt.ExprType, pt: PhraseType): Unit = (t, pt) match {
    case (rt.FunType(inT, outT), FunType(inPT, outPT)) =>
      checkConsistency(inT, inPT)
      checkConsistency(outT, outPT)
    case (rt.DepFunType(kx, x, t), DepFunType(ky, y, pt)) =>
      if (rt.Kind.idName(kx, x) != rt.Kind.idName(ky, y)) error(s"Identifiers $x and $y differ")
      checkConsistency(t, pt)
    case (dt: DataType, ExpType(dpt: DataType, _)) =>

    case _ => error(s"Types $t and $pt not compatible")
  }
}
