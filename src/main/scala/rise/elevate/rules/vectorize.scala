package rise.elevate.rules

import arithexpr.arithmetic.Cst
import elevate.core._
import elevate.macros.RuleMacro.rule
import rise.core.DSL._
import rise.core._
import rise.core.primitives._
import rise.core.types.DataType._
import rise.core.types._
import rise.elevate._

object vectorize {
  // FIXME: sometimes assuming loads or stores will be aligned

  // _ -> asVector >> asScalar
  @rule def after(n: Nat): Strategy[Rise] = e => (e.t: @unchecked) match {
    // FIXME: m + n hack
    case ArrayType(m, _: ScalarType) if (m + n) % n == (0: Nat) =>
      Success(asScalar(asVector(n)(e)) !: e.t)
  }

  // _ -> padEmpty >> asVector >> asScalar >> take
  @rule def roundUpAfter(n: Nat): Strategy[Rise] = e => e.t match {
    case ArrayType(m, _: ScalarType) =>
      val roundUp = padEmpty(n - ((m + n) % n)) // FIXME: m + n hack
      Success(take(m)(asScalar(asVector(n)(roundUp(e)))) !: e.t)
    case _ => Failure(after(n))
  }

  // _ -> asVectorAligned >> asScalar
  @rule def alignedAfter(n: Nat): Strategy[Rise] = e => e.t match {
    // FIXME: m + n hack
    case ArrayType(m, _: ScalarType) if (m + n) % n == (0: Nat) =>
      Success(asScalar(asVectorAligned(n)(e)) !: e.t)
    case _ => Failure(alignedAfter(n))
  }

  // asScalar >> asVector -> _
  @rule def asScalarAsVectorId: Strategy[Rise] = {
    case e @ App(v, App(asScalar(), in)) if isAsVector(v) && e.t =~= in.t =>
      Success(in)
  }

  // map (reduce f init) >> asVector -> asVector >> map (reduce f init)
  @rule def beforeMapReduce: Strategy[Rise] = {
    case e @ App(v, App(App(map(), App(App(reduce(), f), init)), in))
    if isAsVector(v) && isScalarFun(f.t) =>
      // TODO: generalize?
      val inV = preserveType(in) |> transpose |> map(eraseType(v)) |> transpose
      val fV = vectorizeScalarFun(f, Set())
      Success(map(reduce(fV)(vectorFromScalar(init)))(inV) !: e.t)
  }

  // TODO: express as a combination of beforeMapReduce, beforeMap, and others.
  // a |> map (zip b) |> map (reduce f init) |> asVector
  // -> a |> transpose |> map(asVector) |> transpose |> ..
  @rule def beforeMapDot: Strategy[Rise] = {
    case e @ App(v, App(App(map(), App(r @ App(ReduceX(), f), init)),
      App(App(map(), App(zip(), b)), a)
    )) if isAsVector(v) && isScalarFun(f.t) =>
      val aV = preserveType(a) |> transpose |> map(eraseType(v)) |> transpose
      val bV = map(vectorFromScalar)(b)
      val rV = vectorizeScalarFun(r, Set())
      Success(map(zip(bV) >> rV(vectorFromScalar(init)))(aV) !: e.t)
  }

  // map f >> asVector -> asVector >> map f
  @rule def beforeMap: Strategy[Rise] = {
    case e @ App(v, App(App(map(), f), in))
    if isAsVector(v) && isScalarFun(f.t) =>
      val inV = makeAsVector(v)(in.t)(in)
      val fV = vectorizeScalarFun(f, Set())
      Success(map(fV)(inV) !: e.t)
  }

  // pair (asScalar a) (asScalar b)
  // -> pair a b >> mapFst asScalar >> mapSnd asScalar
  // TODO: can get any function out, see takeOutsidePair
  @rule def asScalarOutsidePair: Strategy[Rise] = {
    case e @ App(App(makePair(), App(asScalar(), a)), App(asScalar(), b)) =>
      Success((makePair(a)(b) |> mapFst(asScalar) |> mapSnd(asScalar)) !: e.t)
  }

  // zip (asScalar a) (asScalar b)
  // -> pair a b >> mapFst asScalar >> mapSnd asScalar
  @rule def asScalarOutsideZip: Strategy[Rise] = {
    case e @ App(App(makePair(), App(asScalar(), a)), App(asScalar(), b)) =>
      Success((makePair(a)(b) |> mapFst(asScalar) |> mapSnd(asScalar)) !: e.t)
  }

  // padEmpty (p*v) (asScalar in) -> asScalar (padEmpty p in)
  @rule def padEmptyBeforeAsScalar: Strategy[Rise] = {
    case App(DepApp(NatKind, padEmpty(), pv: Nat), App(asScalar(), in)) =>
      in.t match {
        case ArrayType(_, VectorType(v, _)) if (pv % v) == (0: Nat) =>
          Success(asScalar(padEmpty(pv / v)(in)))
        case _ => Failure(padEmptyBeforeAsScalar)
      }
  }

  // padEmpty p (asVector v in) -> asVector v (padEmpty (p*v) in)
  @rule def padEmptyBeforeAsVector: Strategy[Rise] = {
    case e @ App(DepApp(NatKind, padEmpty(), p: Nat), App(asV @ DepApp(NatKind, _, v: Nat), in))
    if isAsVector(asV) =>
      Success(eraseType(asV)(padEmpty(p*v)(in)) !: e.t)
  }

  // TODO: express as a combination of smaller rules
  @rule def alignSlide: Strategy[Rise] = {
    case e @ App(transpose(),
      App(App(map(), DepApp(NatKind, asVector(), Cst(v))),
        App(join(), App(App(map(), transpose()),
          App(App(map(), DepApp(NatKind, padEmpty(), Cst(p))),
            App(App(map(), DepApp(NatKind, DepApp(NatKind, slide(), Cst(3)), Cst(1))),
              in
            )
          )
        ))
      )
    ) if p <= v =>
      val inW = in.t.asInstanceOf[ArrayType].elemType
        .asInstanceOf[ArrayType].size
      val pV = if (((inW + v) % v).eval == 0) { // TODO: generalize
        Cst(v)
      } else {
        Cst(v + v) - ((inW + v) % v)
      }
      val r = preserveType(in) |>
        map(padEmpty(pV) >> asVectorAligned(v) >> slide(2)(1)) >>
        transpose >>
        map(
          map(asScalar >> take(v+2) >> slide(v)(1) >> join >> asVector(v)) >>
          join
        )
      Success(r !: e.t)

    case e @ App(transpose(),
      App(App(map(), DepApp(NatKind, asVector(), Cst(v))),
        App(transpose(), App(DepApp(NatKind, padEmpty(), Cst(p)),
          App(DepApp(NatKind, DepApp(NatKind, slide(), Cst(3)), Cst(1)), in)
        ))
      )
    ) if p <= v =>
      val inW = in.t.asInstanceOf[ArrayType].size
      val pV = if (((inW + v) % v).eval == 0) { // TODO: generalize
        Cst(v)
      } else {
        Cst(v + v) - ((inW + v) % v)
      }
      val r = preserveType(in) |>
        padEmpty(pV) >> asVectorAligned(v) >> slide(2)(1) >>
        map(asScalar >> take(v+2) >> slide(v)(1) >> join >> asVector(v))
      Success(r !: e.t)
  }

  // TODO: express as a combination of smaller rules
  // FIXME: function f needs to be element-wise (a hidden mapVec)
  @rule def mapAfterShuffle: Strategy[Rise] = {
    case e @ App(DepApp(NatKind, asVector(), v: Nat),
      App(join(), App(DepApp(NatKind, DepApp(NatKind, slide(), v2: Nat), Cst(1)),
        App(DepApp(NatKind, take(), t: Nat), App(asScalar(),
          App(App(map(), f), in)
        ))
      ))
    ) if v == v2 =>
      val shuffle = makeShuffle(
        asScalar >> take(t) >>
        slide(v)(1) >> join >> asVector(v)
      )(in.t)
      Success((preserveType(in) |> shuffle |> map(f)) !: e.t)
  }

  // FIXME: this is very specific
  @rule def padEmptyBeforeZipAsVector: Strategy[Rise] = {
    case e @ App(DepApp(NatKind, padEmpty(), p: Nat), App(
      Lambda(x, App(App(zip(),
        App(asV @ DepApp(NatKind, _, v: Nat), App(fst(), x2))),
        App(asV2, App(snd(), x3)))),
      in
    )) if x =~= x2 && x =~= x3 && isAsVector(asV) && asV =~= asV2 =>
      Success((
        preserveType(in) |> mapFst(padEmpty(p*v)) |> mapSnd(padEmpty(p*v)) |>
        // FIXME: aligning although we have no alignment information
        fun(p => zip(asVectorAligned(v)(fst(p)))(asVectorAligned(v)(snd(p))))
      ) !: e.t)
  }

  def isAsVector: Rise => Boolean = {
    case DepApp(NatKind, asVector(), _: Nat) => true
    case DepApp(NatKind, asVectorAligned(), _: Nat) => true
    case _ => false
  }

  private def isScalarFun: ExprType => Boolean = {
    case FunType(i, o) => isScalarTuple(i) &&
      (isScalarTuple(o) || isScalarFun(o))
    case _ => false
  }

  private def isScalarTuple: ExprType => Boolean = {
    case _: ScalarType => true
    case PairType(a, b) => isScalarTuple(a) && isScalarTuple(b)
    case _ => false
  }

  private def makeAsVector(asV: Rise): ExprType => ToBeTyped[Rise] = {
    case ArrayType(_, _: ScalarType) => eraseType(asV)
    case ArrayType(n, PairType(a, b)) =>
      unzip >> fun(p => zip(
        makeAsVector(asV)(ArrayType(n, a))(fst(p)))(
        makeAsVector(asV)(ArrayType(n, b))(snd(p))))
    case t => throw new Exception(s"did not expect $t")
  }

  private def makeShuffle(s: Rise): ExprType => ToBeTyped[Rise] = {
    case ArrayType(_, _: VectorType) => eraseType(s)
    case ArrayType(n, PairType(a, b)) =>
      unzip >> fun(p => zip(
        makeShuffle(s)(ArrayType(n, a))(fst(p)))(
        makeShuffle(s)(ArrayType(n, b))(snd(p))))
    case t => throw new Exception(s"did not expect $t")
  }

  // FIXME: assuming every scalar function vectorizes like this
  private def vectorizeScalarFun(f: Expr, vEnv: Set[Identifier])
  : ToBeTyped[Expr] = f match {
    case i: Identifier if vEnv(i) => eraseType(i)
    case Lambda(x, b) => lambda(eraseType(x), vectorizeScalarFun(b, vEnv + x))
    case App(f, e) => vectorizeScalarFun(f, vEnv)(vectorizeScalarFun(e, vEnv))
    case p: Primitive => eraseType(p)
    case s if (s.t match {
      case _: ScalarType => true
      case _ => false
    }) => vectorFromScalar(s)
    case _ => throw new Exception(s"did not expect $f")
  }
}
