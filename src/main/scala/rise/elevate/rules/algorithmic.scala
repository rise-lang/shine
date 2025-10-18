package rise.elevate.rules

import arithexpr.arithmetic.{ArithExpr, Cst}
import elevate.core._
import elevate.core.strategies.Traversable
import elevate.core.strategies.predicate._
import elevate.core.strategies.traversal.tryAll
import elevate.macros.RuleMacro.rule
import rise.core.DSL._
import rise.core._
import rise.core.primitives._
import rise.core.types.DataType._
import rise.core.types._
import rise.elevate._
import rise.elevate.strategies.normalForm.DFNF
import rise.elevate.strategies.predicate._
import rise.GAP8.primitives._

// noinspection MutatorLikeMethodIsParameterless
object algorithmic {

  // - Notation -
  // x >> y: piping operator, x then y
  // *f: map(f)
  // T: transpose
  // S: slide/split
  // J: join

  // divide & conquer

  def  splitJoin(n: Nat): Strategy[Rise] = `*f -> S >> **f >> J`(n: Nat)
  @rule def `*f -> S >> **f >> J`(n: Nat): Strategy[Rise] = {
    case e @ App(map(), f) => Success((split(n) >> map(map(f)) >> join) !: e.t)
  }

  @rule def splitJoin2(n: Nat): Strategy[Rise] = e => e.t match {
    case ArrayType(_,_) => Success( (toBeTyped(e) |> split(n) |> join) !: e.t )
    case _ => Failure(splitJoin2(n))
  }

  // fusion / fission

  def mapFusion: Strategy[Rise] = `*g >> *f -> *(g >> f)`
  @rule def `*g >> *f -> *(g >> f)`: Strategy[Rise] = {
    case e @ App(App(map(), f), App(App(map(), g), arg)) =>
      Success(map(preserveType(g) >> f)(arg) !: e.t)
  }

    // mapFst g >> mapFst f -> mapFst (g >> f)
  @rule def mapFstFusion: Strategy[Rise] = {
    case e @ App(App(mapFst(), f), App(App(mapFst(), g), in)) =>
      Success(mapFst(preserveType(g) >> f)(in) !: e.t)
  }

  // mapSnd g >> mapSnd f -> mapSnd (g >> f)
  @rule def mapSndFusion: Strategy[Rise] = {
    case e @ App(App(mapSnd(), f), App(App(mapSnd(), g), in)) =>
      Success(mapSnd(preserveType(g) >> f)(in) !: e.t)
  }

  // padEmpty n >> padEmpty m -> padEmpty n + m
  @rule def padEmptyFusion: Strategy[Rise] = {
    case e @ App(DepApp(NatKind, padEmpty(), m: Nat), App(DepApp(NatKind, padEmpty(), n: Nat), in)) =>
      Success(padEmpty(n+m)(in) !: e.t)
  }

  def `map >> reduce -> reduce`: Strategy[Rise] = reduceMapFusion
  // *g >> reduce f init -> reduce (acc, x => f acc (g x)) init
  @rule def reduceMapFusion: Strategy[Rise] = {
    case e @ App(App(App(r @ ReduceX(), f), init), App(App(map(), g), in)) =>
      val red = (r, g.t) match {
        case (reduce(), FunType(i, o)) if i =~= o => reduce
        case _ => reduceSeq
      }
      Success(red(fun(acc => fun(x =>
        preserveType(f)(acc)(preserveType(g)(x)))))(init)(in) !: e.t)
  }

  def fuseReduceMap: Strategy[Rise] = reduceMapFusion

  @rule def reduceMapFission()(implicit ev: Traversable[Rise]): Strategy[Rise] = {
    case e @ App(App(ReduceX(), Lambda(acc, Lambda(y,
        App(App(op, acc2), f@App(_, y2))))), init)
      if acc =~= acc2 && contains[Rise](y).apply(y2) =>
      Success((reduce(op)(init) o
        map(lambda(ToBeTyped[Identifier](y), preserveType(f)))) !: e.t
      )
  }


  // fission of the last function to be applied inside a map
  // *(g >> .. >> f) -> *(g >> ..) >> *f
  @rule def mapLastFission()(implicit ev: Traversable[Rise]): Strategy[Rise] = {
    // this is an example where we don't want to fission if gx == Identifier:
    // (map λe4. (((((zip: (K.float -> (K.float -> K.(float, float))))
    // (e3: K.float)): (K.float -> K.(float, float)))
    // (e4: K.float)): K.(float, float)))
    // gx == (e4: K.float)
    // in this case we would return some form of map(id):
    // ((map λe4. (e4: K.float)) e743))
    case e @ App(map(), Lambda(x, App(f, gx)))
      if !contains[Rise](x).apply(f) && !isIdentifier(gx) =>
      gx.t match {
        case _: DataType =>
          Success((app(map, lambda(eraseType(x), gx)) >> map(f)) !: e.t)
        case _ => Failure(mapLastFission())
      }
  }

  // identities
  @rule def idAfter: Strategy[Rise] = e => Success((preserveType(e) |> id) !: e.t)

  @rule def idToCopy: Strategy[Rise] = {
    case App(id() ::: FunType(in: ScalarType, out: ScalarType), arg ::: (argT: ScalarType))
      if in =~= out && in =~= argT =>
      Success(fun(x => x) $ arg)
  }

  @rule def liftId()(implicit ev: Traversable[Rise]): Strategy[Rise] = {
    case App(id() ::: FunType(ArrayType(_, _), _), arg) => Success(DFNF()(ev)((map(id) $ arg)).get)
  }

  @rule def createTransposePair: Strategy[Rise] = {
    case e @ App(id(), arg) => Success(app(transpose >> transpose, arg) !: e.t)
  }

  // _-> T >> T
  def transposePairAfter: Strategy[Rise] = idAfter `;` createTransposePair

  @rule def removeTransposePair: Strategy[Rise] = {
    case e @ App(transpose(), App(transpose(), x)) =>
      Success(x !: e.t)
  }

  // overlapped tiling

  // constraint: n - m = u - v
  // v = u + m - n
  @rule def slideOverlap(u: Nat): Strategy[Rise] = {
    case e @ DepApp(NatKind, DepApp(NatKind, slide(), n: Nat), m: Nat) =>
      val v = u + m - n
      Success((slide(u)(v) >> map(slide(n)(m)) >> join) !: e.t)
  }

  // slide widening

  // slide n 1 >> drop l -> slide (n+l) 1 >> map(drop l)
  @rule def dropInSlide: Strategy[Rise] = {
    case e@App(DepApp(NatKind, drop(), l: Nat), App(DepApp(NatKind, DepApp(NatKind, slide(), n: Nat), Cst(1)), in)) =>
      Success(app(map(drop(l)), app(slide(n + l)(1), preserveType(in))) !: e.t)
  }
  // slide n 1 >> take (N - r) -> slide (n+r) 1 >> map(take (n - r))
  @rule def takeInSlide: Strategy[Rise] = {
    case e@App(t@DepApp(NatKind, take(), rem: Nat), App(DepApp(NatKind, DepApp(NatKind, slide(), n: Nat), Cst(1)), in)) =>
      t.t match {
        case FunType(ArrayType(size, _), _) =>
          val r = size - rem
          Success(app(map(take(n)), app(slide(n + r)(1), preserveType(in))) !: e.t)
        case _ => throw new Exception("this should not happen")
      }
  }

  @rule def dropNothing: Strategy[Rise] = {
    case expr @ DepApp(NatKind, drop(), Cst(0)) => Success(fun(x => x) !: expr.t)
  }

  @rule def takeAll: Strategy[Rise] = {
    case expr @ DepApp(NatKind, take(), n: Nat) => expr.t match {
      case FunType(ArrayType(m, _), _) if n == m => Success(fun(x => x) !: expr.t)
      case _ => Failure(takeAll)
    }
  }

  @rule def padEmptyNothing: Strategy[Rise] = {
    case e @ DepApp(NatKind, padEmpty(), Cst(0)) => Success(fun(x => x) !: e.t)
  }

  @rule def mapIdentity: Strategy[Rise] = {
    case expr @ App(map(), Lambda(x1, x2)) if x1 =~= x2 => Success(fun(x => x) !: expr.t)
  }

  // x -> join (slide 1 1 x)
  @rule def slideAfter: Strategy[Rise] = e => Success(join(slide(1: Nat)(1: Nat)(e)) !: e.t)

  @rule def slideAfter2: Strategy[Rise] = e => Success(map(fun(x => x `@` lidx(0, 1)))(slide(1: Nat)(1: Nat)(e)) !: e.t)

  // s -> map snd (zip f s)
  @rule def zipFstAfter(f: Rise): Strategy[Rise] = s => (f.t, s.t) match {
    case (ArrayType(n, _), ArrayType(m, _)) if n == m =>
      Success(map(snd)(zip(f)(s)) !: s.t)
    case _ => Failure(zipFstAfter(f))
  }

  // f -> map fst (zip f s)
  @rule def zipSndAfter(s: Rise): Strategy[Rise] = f => (f.t, s.t) match {
    case (ArrayType(n, _), ArrayType(m, _)) if n == m =>
      Success(map(fst)(zip(f)(s)) !: f.t)
    case _ => Failure(zipSndAfter(s))
  }

  // J >> drop d -> drop (d / m) >> J >> drop (d % m)
  @rule def dropBeforeJoin: Strategy[Rise] = {
    case e @ App(DepApp(NatKind, drop(), d: Nat), App(join(), in)) => in.t match {
      case ArrayType(_, ArrayType(m, _)) =>
        Success(app(drop(d % m), join(drop(d / m)(in))) !: e.t)
      case _ => throw new Exception("this should not happen")
    }
  }

  // J >> take (n*m - d)
  // -> dropLast (d / m) >> J >> dropLast (d % m)
  // -> take (n - d / m) >> J >> take ((n - d / m)*m - d % m)
  @rule def takeBeforeJoin: Strategy[Rise] = {
    case e @ App(DepApp(NatKind, take(), nmd: Nat), App(join(), in)) => in.t match {
      case ArrayType(n, ArrayType(m, _)) =>
        val d = n*m - nmd
        val t1 = n - d / m
        val t2 = t1*m - d % m
        Success(app(take(t2), join(take(t1)(in))) !: e.t)
      case _ => throw new Exception("this should not happen")
    }
  }

  // take n >> padEmpty m -> padEmpty m'
  @rule def removeTakeBeforePadEmpty: Strategy[Rise] = {
    case e @ App(DepApp(NatKind, padEmpty(), m: Nat), App(DepApp(NatKind, take(), n: Nat), in)) =>
      in.t match {
        case ArrayType(size, _)
        if ArithExpr.isSmaller(size - n, m + 1).contains(true) =>
          val mPrime = m - (size - n)
          Success(padEmpty(mPrime)(in) !: e.t)
        case _ =>
          Failure(removeTakeBeforePadEmpty)
      }
  }

  // makeArray(n)(map f1 e)..(map fn e)
  // -> e |> map(x => makeArray(n)(f1 x)..(fn x)) |> transpose
  @rule def mapOutsideMakeArray: Strategy[Rise] = expr => {
    def matchExpectedMakeArray(mka: Rise): Option[Rise] = mka match {
      case App(makeArray(_), App(App(map(), _), e)) => Some(e)
      case App(f, App(App(map(), _), e2)) =>
        matchExpectedMakeArray(f).flatMap(e =>
          if (e =~= e2) { Some(e) } else { None })
      case _ => None
    }

    def transformMakeArray(mka: Rise, x: ToBeTyped[Rise]): ToBeTyped[Rise] = mka match {
      case makeArray(n) => makeArray(n)
      case App(mka, App(App(map(), f), _)) =>
        app(transformMakeArray(mka, x), app(f, x))
      case _ => throw new Exception("this should not happen")
    }

    matchExpectedMakeArray(expr) match {
      case Some(e) => Success(
        app(transpose, map(fun(x => transformMakeArray(expr, x)))(e))
          !: expr.t)
      case None => Failure(mapOutsideMakeArray)
    }
  }

  // generate (i => select t (map f e) (map g e))
  // -> e |> map (x => generate (i => select t (f x) (g x))) |> transpose
  @rule def mapOutsideGenerateSelect()(implicit ev: Traversable[Rise]): Strategy[Rise] = {
    case expr @ App(generate(), Lambda(i, App(App(App(select(), t),
      App(App(map(), f), e1)),
      App(App(map(), g), e2))))
    if e1 =~= e2 && !contains[Rise](i).apply(e1) =>
      Success(transpose(map(
        fun(x => generate(lambda(eraseType(i), select(t)(app(f, x), app(g, x))))))(
        e1
      )) !: expr.t)
  }

  // select t (f a) (f b) -> f (select t a b)
  @rule def fOutsideSelect: Strategy[Rise] = {
    case expr @ App(App(App(select(), t), App(f1, a)), App(f2, b)) if f1 =~= f2 =>
      f1.t match {
        case FunType(_: DataType, _: DataType) => Success(app(f1, select(t)(a)(b)) !: expr.t)
        case _ => Failure(fOutsideSelect)
      }
  }

  // makeArray (f e1) .. (f en) -> map f (makeArray e1 .. en)
  @rule def fOutsideMakeArray: Strategy[Rise] = expr => {
    def matchExpectedMakeArray(mka: Rise): Option[(Int, Rise)] = mka match {
      case App(makeArray(n), App(f, _)) =>
        f.t match {
          case FunType(_: DataType, _: DataType) => Some((n - 1, f))
          case _ => None
        }
      case App(mka, App(f2, _)) =>
        matchExpectedMakeArray(mka).flatMap { case (n, f) =>
          if (f =~= f2) { Some((n - 1, f)) } else { None }
        }
      case _ => None
    }

    def transformMakeArray(mka: Rise): ToBeTyped[Rise] = mka match {
      case makeArray(n) => makeArray(n)
      case App(mka, App(_, e)) => app(transformMakeArray(mka), e)
      case _ => throw new Exception("this should not happen")
    }

    matchExpectedMakeArray(expr) match {
      case Some((0, f)) =>
        Success(app(map(f), transformMakeArray(expr)) !: expr.t)
      case _ => Failure(fOutsideMakeArray)
    }
  }

  // zip (map fa a) (map fb b) -> zip a b >> map (p => pair (fa (fst p)) (fb (snd p)))
  @rule def mapOutsideZip: Strategy[Rise] = {
    case expr @ App(App(zip(), App(App(map(), fa), a)), App(App(map(), fb), b)) =>
      Success(map(fun(p => makePair(app(fa, fst(p)))(app(fb, snd(p)))))(zip(a)(b)) !: expr.t)
  }

  // pair (map fa a) (map fb b)
  // -> zip a b >> map (p => pair (fa (fst p)) (fb (snd p))) >> unzip
  @rule def mapOutsidePair: Strategy[Rise] = {
    case expr @ App(App(makePair(), App(App(map(), fa), a)), App(App(map(), fb), b)) =>
      Success(unzip(map(fun(p => makePair(app(fa, fst(p)))(app(fb, snd(p)))))(zip(a)(b))) !: expr.t)
  }

  // zip a a -> map (x => pair(x, x)) a
  @rule def zipSame: Strategy[Rise] = {
    case expr @ App(App(zip(), a), a2) if a =~= a2 =>
      Success(map(fun(x => makePair(x)(x)))(a) !: expr.t)
  }

  // zip(a, b) -> map (x => pair(snd(x), fst(x))) zip(b, a)
  @rule def zipSwap: Strategy[Rise] = {
    case expr @ App(App(zip(), a), b) =>
      Success(map(fun(x => makePair(snd(x))(fst(x))))(zip(b)(a)) !: expr.t)
  }

  // zip(a, zip(b, c)) -> map (x => pair(.., pair(..))) zip(zip(a, b), c)
  @rule def zipRotateLeft: Strategy[Rise] = {
    case expr @ App(App(zip(), a), App(App(zip(), b), c)) => Success(map(
      fun(x => makePair(fst(fst(x)))(makePair(snd(fst(x)))(snd(x)))))(
      zip(zip(a)(b))(c)
    ) !: expr.t)
  }

  // zip(zip(a, b), c) -> map (x => pair(pair(..), ..)) zip(a, zip(b, c))
  @rule def zipRotateRight: Strategy[Rise] = {
    case expr @ App(App(zip(), App(App(zip(), a), b)), c) => Success(map(
      fun(x => makePair(makePair(fst(x))(fst(snd(x))))(snd(snd(x)))))(
      zip(a)(zip(b)(c))
    ) !: expr.t)
  }

  def zipRotate: Strategy[Rise] = zipRotateLeft <+ zipRotateRight

  // e -> map (x => x) e
  @rule def mapIdentityAfter: Strategy[Rise] = expr => expr.t match {
    case ArrayType(_, _) => Success(map(fun(x => x))(expr) !: expr.t)
    case _ => Failure(mapIdentityAfter)
  }

  // fst (pair a b) -> a
  @rule def fstReduction: Strategy[Rise] = {
    case expr @ App(fst(), App(App(makePair(), a), _)) => Success(a !: expr.t)
  }

  // snd (pair a b) -> b
  @rule def sndReduction: Strategy[Rise] = {
    case expr @ App(snd(), App(App(makePair(), _), b)) => Success(b !: expr.t)
  }

  // zip (slide n m a) (slide n m b) -> map unzip (slide n m (zip a b))
  @rule def slideOutsideZip: Strategy[Rise] = {
    case expr @ App(App(zip(),
      App(DepApp(NatKind, DepApp(NatKind, slide(), n: Nat), m: Nat), a)),
      App(DepApp(NatKind, DepApp(NatKind, slide(), n2: Nat), m2: Nat), b)
    ) if n == n2 && m == m2 =>
      Success(map(unzip)(slide(n)(m)(zip(a)(b))) !: expr.t)
  }

  // slide n m (zip a b) -> map zip (zip (slide n m a) (slide n m b))
  @rule def slideInsideZip: Strategy[Rise] = {
    case expr @ App(DepApp(NatKind, DepApp(NatKind, slide(), n: Nat), m: Nat),
      App(App(zip(), a), b)
    ) =>
      Success(map(fun(p => zip(fst(p))(snd(p))))(
        zip(slide(n)(m)(a))(slide(n)(m)(b))) !: expr.t)
  }

  // TODO?
  // map (x => g (f (fst x))) (zip a b) -> map (x => g (fst x)) (zip (map f a) b)
  // def fBeforeZipMapFst: Strategy[Rise] =
  // map (x => g (f (snd x))) (zip a b) -> map (x => g (snd x)) (zip a (map f b))
  // def fBeforeZipMapSnd: Strategy[Rise] =
  @rule def fBeforeZipMap: Strategy[Rise] = {
    case expr @ App(
      App(map(), Lambda(x, App(App(zip(),
        App(f, App(fst(), x2))),
        App(g, App(snd(), x3))))),
      App(App(zip(), a), b)
    ) if x =~= x2 && x =~= x3 =>
      Success(app(
        map(fun(x => zip(fst(x))(snd(x)))),
        zip(map(f)(a))(map(g)(b))
      ) !: expr.t)
  }

  // a |> map (zip b) |> transpose
  // -> transpose a |> zip2D (map (x => generate (_ => x) b)
  @rule def transposeBeforeMapZip: Strategy[Rise] = {
    case e @ App(transpose(), App(App(map(), App(zip(), b)), a)) =>
      Success(map(fun(p => zip(fst(p))(snd(p))))(
        zip(map(fun(x => generate(fun(_ => x))))(b))(transpose(a))) !: e.t)
  }

  // unzip (zip a b) -> pair a b
  @rule def unzipZipIsPair: Strategy[Rise] = {
    case e @ App(unzip(), App(App(zip(), a), b)) =>
      Success(makePair(a)(b) !: e.t)
  }

  // FIXME: fighting against beta-reduction
  // unzip ((p => zip (fst p) (snd p)) in) -> in
  @rule def unzipZipIdentity: Strategy[Rise] = {
    case e @ App(unzip(), App(Lambda(p,
      App(App(zip(), App(fst(), p2)), App(snd(), p3))), in))
    if p =~= p2 && p =~= p3 =>
      Success(in !: e.t)
  }

  // FIXME: this is very specific
  // zip (fst/snd unzip e) (fst/snd unzip e)
  // -> map (p => pair (fst/snd p) (fst/snd p)) e
  //
  // equivalent sequence to replace this with:
  // fst unzip e = map fst e (fstUnzipAsMapFst)
  // snd unzip e = map snd e (sndUnzipAsMapSnd)
  // mapOutsideZip ; zipSame ; mapFusion ; fstReduction ; sndReduction
  @rule def zipUnzipAccessSimplification: Strategy[Rise] = {
    case e @ App(App(zip(),
      App(a1 @ (fst() | snd()), App(unzip(), e1))),
      App(a2 @ (fst() | snd()), App(unzip(), e2))
    ) if e1 =~= e2 =>
      Success(map(fun(p => makePair(eraseType(a1)(p))(eraseType(a2)(p))))(e1) !: e.t)
  }

  // FIXME: this is very specific
  @rule def zipAsVectorUnzipSimplification: Strategy[Rise] = {
    case e @ App(
      Lambda(x, App(App(zip(),
        App(DepApp(NatKind, asVector(), v: Nat), App(fst(), x2))),
        App(DepApp(NatKind, asVector(), v2: Nat), App(snd(), x3)))),
      App(unzip(), in)
    ) if x =~= x2 && x =~= x3 && v == v2 =>
      println(in.t)
      println(e.t)
      val r = preserveType(in) |>
        mapFst(asVectorAligned(v)) |> mapSnd(asVectorAligned(v)) |>
        fun(p => zip(fst(p))(snd(p)))
      Success(r !: e.t)
  }

  // FIXME: this is very specific
  // map (p => g (fst p) (snd p)) (zip (fst/snd e) (fst/snd e))
  // -> map (p => g (fst/snd p) (fst/snd p)) (zip (fst e) (snd e))
  @rule def mapProjZipUnification()(implicit ev: Traversable[Rise]): Strategy[Rise] = {
    case e @ App(App(map(),
      Lambda(p, App(App(g, App(fst(), p1)), App(snd(), p2)))),
      App(App(zip(),
        App(a1 @ (fst() | snd()), e1)),
        App(a2 @ (fst() | snd()), e2)))
    if e1 =~= e2 && p =~= p1 && p =~= p2 &&
      a1 =~= a2 && !contains[Rise](p).apply(g)
    =>
      Success(map(fun(p => preserveType(g)(eraseType(a1)(p), eraseType(a2)(p))))(
        zip(fst(e1))(snd(e1))) !: e.t)
  }

  // TODO: should not be in this file?
  // broadly speaking, f(x) -> x |> fun(y => f(y))
  case class subexpressionElimination(find: Strategy[Rise])(implicit ev: Traversable[Rise]) extends Strategy[Rise] {
    import elevate.core.strategies.traversal._
    def apply(e: Rise): RewriteResult[Rise] = {
      var typedX: Rise = null // Hack to get the typed version of X
      topDown(find `;` { xt =>
        typedX = xt
        Success(xt)
      }).apply(e).mapSuccess(_ => {
        app(fun(typedX.t)(y =>
          substitute.exprInExpr(y, `for` = typedX, e)
        ), typedX) !: e.t
      })
    }
  }

  // the inner strategies shouldn't be accessible from the outside
  // because they might change the semantics of a program
  @rule def freshLambdaIdentifier()(implicit ev: Traversable[Rise]): Strategy[Rise] = e => {
    @rule def freshIdentifier: Strategy[Rise] = {
      case Identifier(name) ::: t =>
        Success(Identifier(freshName("fresh_"+ name))(t))
    }

    @rule def replaceIdentifier(curr: Identifier, newId: Identifier): Strategy[Rise] = {
      case x: Identifier if curr =~= x => Success(newId)
    }

    e match {
      case Lambda(x,e) ::: t if contains[Rise](x).apply(e) =>
        val newX = freshIdentifier(x).get.asInstanceOf[Identifier]
        val newE = tryAll(replaceIdentifier(x, newX)).apply(e).get
        Success(Lambda(newX, newE)(t))
      case _ => Failure(freshLambdaIdentifier())
    }
  }

  // different name for ICFP'20
  def splitStrategy(n: Nat)(implicit ev: Traversable[Rise]): Strategy[Rise] = blockedReduce(n)
  @rule def blockedReduce(n: Nat)(implicit ev: Traversable[Rise]): Strategy[Rise] = {
    case App(App(App(reduce(), op ::: FunType(yT, FunType(initT, outT))),
      init), arg) if yT =~= outT =>
      // avoid having two lambdas using the same identifiers
      val freshOp = tryAll(freshLambdaIdentifier()).apply(op).get
      DFNF()(ev)(
        (reduceSeq(fun((acc, y) =>
          preserveType(op)(acc, reduce(freshOp)(init)(y))))(init) o
          split(n)) $ arg
      )
  }

  

  private val mulT: ToBeTyped[Rise] = fun(x => fst(x) * snd(x))
  private val sum: ToBeTyped[Rise] = reduce(add)(lf32(0.0f))
  private val dot: ToBeTyped[Rise] = fun(a => fun(b =>
    zip(a)(b) |> map(mulT) |> sum
  ))
  // TODO: check separability property?
  @rule def separateDotHV(weights2d: Expr, wH: Expr, wV: Expr): Strategy[Rise] = {
    case e @ App(App(App(reduce(), rf), init), App(App(map(), mf),
      App(App(zip(), App(join(), weights)), App(join(), nbh))
    )) if rf =~= ((add !: rf.t): Expr) &&
      init =~= (lf32(0.0f): Expr) &&
      mf =~= ((mulT !: mf.t): Expr) &&
      weights =~= weights2d
      =>
      Success((preserveType(nbh) |> map(dot(wH)) |> dot(wV)) !: e.t)
  }

  @rule def separateDotVH(weights2d: Expr, wV: Expr, wH: Expr): Strategy[Rise] = {
    case e @ App(App(App(reduce(), rf), init), App(App(map(), mf),
    App(App(zip(), App(join(), weights)), App(join(), nbh))
    )) if rf =~= ((add !: rf.t): Expr) &&
      init =~= (lf32(0.0f): Expr) &&
      mf =~= ((mulT !: mf.t): Expr) &&
      weights =~= weights2d
    =>
      Success((preserveType(nbh) |> transpose |> map(dot(wV)) |> dot(wH)) !: e.t)
  }

  @rule def separateSumHV: Strategy[Rise] = {
    case e @ App(sum2, App(join(), in)) if sum2 =~= ((sum !: sum2.t): Expr) =>
      Success((preserveType(in) |> map(sum) |> sum) !: e.t)
  }

  @rule def separateSumVH: Strategy[Rise] = {
    case e @ App(sum2, App(join(), in)) if sum2 =~= ((sum !: sum2.t): Expr) =>
      Success((preserveType(in) |> transpose |> map(sum) |> sum) !: e.t)
  }

  /*@rule def colapseOpenMPfor: Strategy[Rise] = {
    ???
  }*/

  /**
    * TODO: A couple of things to think about:
    *   1.This is extremely specific. Is there a way to make it more general in any way?
    *   2. Concrete HWCE primitive depends not only on the size of the sliding window,
    *     but on the appropirate dimensions of the input parameters as well. Patch that in
    *   3. slide2D has to be deconstructed fully to ensure that the underlying pattern
    *     fully conforms with the slide2D
    *   4. Generalize mapSeq() and reduceSeq()
    *   5. This should apply only if wrapped somewhere within gap8run (HWCE is in cluster)
    * */
  @rule def gap8hwConvMerge: Strategy[Rise] = {
    case e @
      App(
        App(mapSeq(),
          App(mapSeq(), Lambda(_,
            App(
              App(
                App(reduceSeq(), add()),
                  App(cast(), _)),
                    App(
                      App(map(), Lambda(_,
                        App(App(mul(), App(fst(), _)), App(snd(), _))
                        )
                      ),
                      App(App(zip(), App(join(), _)), App(join(), filter))
                    )
              )
            )
          )
        ),
        App(Lambda(_,
          App(_,
            App(Lambda(_,
              App(
                DepApp(NatKind, DepApp(NatKind, slide(), size), step), _
              )
            ), _)
          )
        ), in)
      ) =>
      (size, step) match {
        case (Cst(iSize), Cst(iStep)) =>
          if(1 == iStep && 3 == iSize) Success(gap8hwConv3x3(0)(in)(filter) !: e.t)
          else if(1 == iStep && 5 == iSize) Success(gap8hwConv5x5(0)(in)(filter) !: e.t)
          else if(1 == iStep && 7 == iSize) Success(gap8hwConv7x7(0)(in)(filter) !: e.t)
          else Failure(gap8hwConvMerge)
        case _ => Failure(gap8hwConvMerge)
      }
  }
}
