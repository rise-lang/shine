package rise.elevate.rules

import elevate.core.strategies.Traversable
import elevate.core.strategies.predicate._
import elevate.core._
import elevate.macros.RuleMacro.rule
import rise.elevate._
import rise.core._
import rise.core.types._
import rise.core.primitives._
import rise.core.TypedDSL._
import rise.core.TypeLevelDSL._
import rise.core.types.{ArrayType, DataType, FunType, IndexType, Nat, PairType}

// Describing possible movements between pairs of rise primitives
// (potentially nested in maps)

// todo: remove inspection prevention

//noinspection ScalaStyle
// todo: should all rules expect LCNF-normalized expressions as input?
object movement {

  // - Notation -
  // x >> y: piping operator, x then y
  // *f: map(f)
  // T: transpose
  // S: slide/split
  // J: join

  // transpose

  def mapMapFBeforeTranspose()(implicit ev: Traversable[Rise]): Strategy[Rise] = `**f >> T -> T >> **f`()(ev)
  @rule def `**f >> T -> T >> **f`()(implicit ev: Traversable[Rise]): Strategy[Rise] = {
    case e@App(
      Transpose(),
      App(App(Map(), App(Map(), f)), y)) =>
        Success((typed(y) |> transpose |> map(map(f))) :: e.t)
    // LCNF
    case e@App(Transpose(),
    App(
      App(Map(), lamA @ Lambda(_, App(
              App(Map(), lamB @ Lambda(_, App(f, _))), _))),
      arg)
    ) if etaReduction()(ev)(lamA) && etaReduction()(ev)(lamB) =>
      // Success((typed(arg) |> transpose |> map(map(f))) :: e.t)
      Success((typed(arg) |> transpose |> map(fun(a => map(fun(b => typed(f)(b)))(a)))) :: e.t)
  }

  def transposeBeforeMapMapF: Strategy[Rise] = `T >> **f -> **f >> T`
  @rule def `T >> **f -> **f >> T`: Strategy[Rise] = {
    case e@App(App(Map(), App(Map(), f)), App(Transpose(), y)) =>
      Success((typed(y) |> map(map(f)) |> transpose) :: e.t)
  }


  // split/slide

  def isSplitOrSlide(s: Expr): Boolean = s match {
    case DepApp(DepApp(Slide(), _: Nat), _: Nat) => true
    case DepApp(Split(), _: Nat)                 => true
    case _                                       => false
  }

  def slideBeforeMapMapF: Strategy[Rise] = `S >> **f -> *f >> S`
  @rule def `S >> **f -> *f >> S`: Strategy[Rise] = {
    case e@App(App(Map(), App(Map(), f)), App(s, y)) if isSplitOrSlide(s) =>
      Success((typed(y) |> map(f) |> untyped(s)) :: e.t)
  }

  def slideBeforeMap: Strategy[Rise] = `*f >> S -> S >> **f`
  @rule def `*f >> S -> S >> **f`: Strategy[Rise] = {
    case e@App(s @ DepApp(DepApp(Slide(), _: Nat), _: Nat), App(App(Map(), f), y)) =>
      Success((typed(y) |> untyped(s) |> map(map(f))) :: e.t)
  }

  // *f >> S -> S >> **f
  @rule def splitBeforeMap: Strategy[Rise] = {
    case e@App(s @ DepApp(Split(), _: Nat), App(App(Map(), f), y)) =>
      Success((typed(y) |> untyped(s) |> map(map(f))) :: e.t)
  }

  // join

  def joinBeforeMapF: Strategy[Rise] = `J >> *f -> **f >> J`
  @rule def `J >> *f -> **f >> J`: Strategy[Rise] = {
    case e@App(App(Map(), f),App(Join(), y)) =>
      Success((typed(y) |> map(map(f)) >> join) :: e.t)
  }

  def mapMapFBeforeJoin: Strategy[Rise] = `**f >> J -> J >> *f`
  @rule def `**f >> J -> J >> *f`: Strategy[Rise] = {
    case e@App(Join(), App(App(Map(), App(Map(), f)), y)) =>
      Success((typed(y) |> join |> map(f)) :: e.t)
  }

  // drop and take

  def dropBeforeMap: Strategy[Rise] = `*f >> drop n -> drop n >> *f`
  @rule def `*f >> drop n -> drop n >> *f`: Strategy[Rise] = {
    case expr @ App(DepApp(Drop(), n: Nat), App(App(Map(), f), in)) =>
      Success(app(map(f), app(drop(n), typed(in))) :: expr.t)
  }

  def takeBeforeMap: Strategy[Rise] = `*f >> take n -> take n >> *f`
  @rule def `*f >> take n -> take n >> *f`: Strategy[Rise] = {
    case expr @ App(DepApp(Take(), n: Nat), App(App(Map(), f), in)) =>
      Success(app(map(f), app(take(n), typed(in))) :: expr.t)
  }

  // take n >> *f -> *f >> take n
  @rule def takeAfterMap: Strategy[Rise] = {
    case e @ App(App(Map(), f), App(DepApp(Take(), n: Nat), in)) =>
      Success(take(n)(map(f, in)) :: e.t)
  }

  def takeInZip: Strategy[Rise] = `take n (zip a b) -> zip (take n a) (take n b)`
  @rule def `take n (zip a b) -> zip (take n a) (take n b)`: Strategy[Rise] = {
    case expr @ App(DepApp(Take(), n), App(App(Zip(), a), b)) =>
      Success(zip(depApp(take, n)(a), depApp(take, n)(b)) :: expr.t)
  }

  // zip (take n a) (take n b) -> take n (zip a b)
  @rule def takeOutisdeZip: Strategy[Rise] = {
    case e @ App(App(Zip(),
      App(DepApp(Take(), n1: Nat), a)), App(DepApp(Take(), n2: Nat), b)
    ) if n1 == n2 =>
      Success(take(n1)(zip(a, b)) :: e.t)
  }

  // pair (take n a) (take m b) -> pair a b >> mapFst take n >> mapSnd take m
  // TODO: can get any function out, see asScalarOutsidePair
  @rule def takeOutsidePair: Strategy[Rise] = {
    case e @ App(App(Pair(),
      App(DepApp(Take(), n: Nat), a)), App(DepApp(Take(), m: Nat), b)
    ) =>
      Success((pair(a, b) |> mapFst(take(n)) |> mapSnd(take(m))) :: e.t)
  }

  def dropInZip: Strategy[Rise] = `drop n (zip a b) -> zip (drop n a) (drop n b)`
  @rule def `drop n (zip a b) -> zip (drop n a) (drop n b)`: Strategy[Rise] = {
    case expr @ App(DepApp(Drop(), n), App(App(Zip(), a), b)) =>
      Success(zip(depApp(drop, n)(a), depApp(drop, n)(b)) :: expr.t)
  }

  def takeInSelect: Strategy[Rise] = `take n (select t a b) -> select t (take n a) (take n b)`
  @rule def `take n (select t a b) -> select t (take n a) (take n b)`: Strategy[Rise] = {
    case expr @ App(DepApp(Take(), n), App(App(App(Select(), t), a), b)) =>
      Success(select(t, depApp(take, n)(a), depApp(take, n)(b)) :: expr.t)
  }

  def dropInSelect: Strategy[Rise] = `drop n (select t a b) -> select t (drop n a) (drop n b)`
  @rule def `drop n (select t a b) -> select t (drop n a) (drop n b)`: Strategy[Rise] = {
    case expr @ App(DepApp(Drop(), n), App(App(App(Select(), t), a), b)) =>
      Success(select(t, depApp(drop, n)(a), depApp(drop, n)(b)) :: expr.t)
  }

  def dropBeforeTake: Strategy[Rise] = `take (n+m) >> drop m -> drop m >> take n`
  @rule def `take (n+m) >> drop m -> drop m >> take n`: Strategy[Rise] = {
    case expr @ App(DepApp(Drop(), m: Nat), App(DepApp(Take(), nm: Nat), in)) =>
      Success(app(take(nm - m), app(drop(m), typed(in))) :: expr.t)
  }

  def takeBeforeDrop: Strategy[Rise] = `drop m >> take n -> take (n+m) >> drop m`
  @rule def `drop m >> take n -> take (n+m) >> drop m`: Strategy[Rise] = {
    case expr @ App(DepApp(Take(), n: Nat), App(DepApp(Drop(), m: Nat), in)) =>
      Success(app(drop(m), app(take(n+m), typed(in))) :: expr.t)
  }

  def takeBeforeSlide: Strategy[Rise] = `slide n m >> take t -> take (m * (t - 1) + n) >> slide n m`
  @rule def `slide n m >> take t -> take (m * (t - 1) + n) >> slide n m`: Strategy[Rise] = {
    case expr @ App(DepApp(Take(), t: Nat), App(DepApp(DepApp(Slide(), n: Nat), m: Nat), in)) =>
      Success(app(slide(n)(m), take(m * (t - 1) + n)(in)) :: expr.t)
  }

  def dropBeforeSlide: Strategy[Rise] = `slide n m >> drop d -> drop (d * m) >> slide n m`
  @rule def `slide n m >> drop d -> drop (d * m) >> slide n m`: Strategy[Rise] = {
    case expr @ App(DepApp(Drop(), d: Nat), App(DepApp(DepApp(Slide(), n: Nat), m: Nat), in)) =>
      Success(app(slide(n)(m), drop(d * m)(in)) :: expr.t)
  }

  // slide n m >> padEmpty p -> padEmpty (p * m) >> slide n m
  @rule def padEmptyBeforeSlide: Strategy[Rise] = {
    case e @ App(DepApp(PadEmpty(), p: Nat),
      App(DepApp(DepApp(Slide(), n: Nat), m: Nat), in)
    ) =>
      Success(slide(n)(m)(padEmpty(p * m)(in)) :: e.t)
  }

  // map f >> padEmpty n -> padEmpty n >> map f
  @rule def padEmptyBeforeMap: Strategy[Rise] = {
    case e @ App(DepApp(PadEmpty(), n: Nat), App(App(Map(), f), in)) =>
      Success(map(f, padEmpty(n)(in)) :: e.t)
  }

  // transpose >> padEmpty n -> map (padEmpty n) >> transpose
  @rule def padEmptyBeforeTranspose: Strategy[Rise] = {
    case e @ App(DepApp(PadEmpty(), n: Nat), App(Transpose(), in)) =>
      Success(transpose(map(padEmpty(n), in)) :: e.t)
  }

  // padEmpty n (zip a b) -> zip (padEmpty n a) (padEmpty n b)
  @rule def padEmptyInsideZip: Strategy[Rise] = {
    case e @ App(DepApp(PadEmpty(), n: Nat), App(App(Zip(), a), b)) =>
      Success(zip(padEmpty(n)(a), padEmpty(n)(b)) :: e.t)
  }

  // FIXME: this is very specific
  // zip (fst e) (snd e) |> padEmpty n ->
  // (mapFst padEmpty n) (mapSnd padEmpty n) |> fun(p => zip (fst p) (snd(p))
  @rule def padEmptyBeforeZip: Strategy[Rise] = {
    case e @ App(DepApp(PadEmpty(), n: Nat),
      App(App(Zip(), App(Fst(), e1)), App(Snd(), e2)))
    if e1 == e2 =>
      Success((typed(e1) |>
        mapFst(padEmpty(n)) |> mapSnd(padEmpty(n)) |>
        fun(p => zip(fst(p), snd(p)))) :: e.t)
  }

  // special-cases
  // slide + transpose

  def transposeBeforeSlide: Strategy[Rise] = `T >> S -> *S >> T >> *T`
  @rule def `T >> S -> *S >> T >> *T`: Strategy[Rise] = {
    case e@App(s, App(Transpose(), y)) if isSplitOrSlide(s) =>
      Success((typed(y) |> map(untyped(s)) |> transpose >> map(transpose)) :: e.t)
  }

  def transposeBeforeMapSlide: Strategy[Rise] = `T >> *S -> S >> *T >> T`
  @rule def `T >> *S -> S >> *T >> T`: Strategy[Rise] = {
    case e@App(App(Map(), s), App(Transpose(), y)) if isSplitOrSlide(s) =>
      Success((typed(y) |> untyped(s) |> map(transpose) |> transpose) :: e.t)
  }

  def mapSlideBeforeTranspose: Strategy[Rise] = `*S >> T -> T >> S >> *T`
  @rule def `*S >> T -> T >> S >> *T`: Strategy[Rise] = {
    case e@App(Transpose(), App(App(Map(), s), y)) if isSplitOrSlide(s) =>
      Success((typed(y) |> transpose >> untyped(s) >> map(transpose)) :: e.t)
  }

  // transpose + join

  def joinBeforeTranspose: Strategy[Rise] = `J >> T -> *T >> T >> *J`
  @rule def `J >> T -> *T >> T >> *J`: Strategy[Rise] = {
    case e@App(Transpose(), App(Join(), y)) =>
      Success((typed(y) |> map(transpose) |> transpose |> map(join)) :: e.t)
  }

  def transposeBeforeMapJoin: Strategy[Rise] = `T >> *J -> *T >> J >> T`
  @rule def `T >> *J -> *T >> J >> T`: Strategy[Rise] = {
    case e@App(App(Map(), Join()), App(Transpose(), y)) =>
      Success((typed(y) |> map(transpose) |> join |> transpose) :: e.t)
  }

  def mapTransposeBeforeJoin: Strategy[Rise] = `*T >> J -> T >> *J >> T`
  @rule def `*T >> J -> T >> *J >> T`: Strategy[Rise] = {
    case e@App(Join(), App(App(Map(), Transpose()), y)) =>
      Success((typed(y) |> transpose |> map(join) |> transpose) :: e.t)
  }

  def mapJoinBeforeTranspose: Strategy[Rise] = `*J >> T -> T >> *T >> J`
  @rule def `*J >> T -> T >> *T >> J`: Strategy[Rise] = {
    case e@App(Transpose(), App(App(Map(), Join()), y)) =>
      Success((typed(y) |> transpose |> map(transpose) |> join) :: e.t)
  }

  // join + join

  def joinBeforeJoin: Strategy[Rise] = `J >> J -> *J >> J`
  @rule def `J >> J -> *J >> J`: Strategy[Rise] = {
    case e@App(Join(), App(Join(), y)) =>
      Success((typed(y) |> map(join) >> join) :: e.t)
  }

  def mapJoinBeforeJoin: Strategy[Rise] = `*J >> J -> J >> J`
  @rule def `*J >> J -> J >> J`: Strategy[Rise] = {
    case e@App(Join(), App(App(Map(), Join()), y)) =>
      Success((typed(y) |> join |> join) :: e.t)
  }

  // split + slide

  def slideBeforeSplit: Strategy[Rise] = `slide(n)(s) >> split(k) -> slide(k+n-s)(k) >> map(slide(n)(s))`
  @rule def `slide(n)(s) >> split(k) -> slide(k+n-s)(k) >> map(slide(n)(s))`: Strategy[Rise] = {
    case e@App(DepApp(Split(), k: Nat), App(DepApp(DepApp(Slide(), n: Nat), s: Nat), y)) =>
      Success((typed(y) |> slide(k + n - s)(k) |> map(slide(n)(s))) :: e.t)
  }

  // TODO: what if s != 1?
  // slide(n)(s=1) >> slide(m)(k) -> slide(m+n-1)(k) >> map(slide(n)(1))
  @rule def slideBeforeSlide: Strategy[Rise] = {
    case e@App(DepApp(DepApp(Slide(), m: Nat), k: Nat),
            App(DepApp(DepApp(Slide(), n: Nat), s: Nat), in)
         ) if s == (1: Nat) =>
      Success((typed(in) |> slide(m+n-s)(k) |> map(slide(n)(s))) :: e.t)
  }

  // nested map + reduce

  // different variants for rewriting map(reduce) to reduce(map)
  // todo what makes them different? can we decompose them into simpler rules?
  @rule def liftReduce: Strategy[Rise] = {

     // 2D array of pairs ----------------------------------------------------
    case e@App(Map(), Lambda(_,
    App(App(App(ReduceX(), op),
    _), _)
    // PairType is new here
    )) ::: FunType(ArrayType(_, ArrayType(_,PairType(_,_))), resultT) =>

      val result: TDSL[Rise] = (fun(x =>
        (reduceSeq(fun((acc, y) =>
          map(fun(a => typed(op)(a._1)(a._2))) $ zip(acc,y)
        ))(x._1 :: resultT) o // x._1 :: 2D array
          // transpose2D
          transpose o map(transpose) $ x._2)) o
        // unzip2D
        unzip o map(unzip)) :: e.t

      Success(result)

      // yet another case ----------------------------------------------------
    /*
  documentation for the concrete case I had:
  32.(float, 4.(float,float)) -> 32.float (via map(f o reduce))
  (32.float, 32.4.(float,float)) [unzip]
  (32.float, 4.32.(float,float)) [transpose $ x._2]
  now reducing (map(+)) x._2, using x_.1 as init,
   */
    case e@App(Map(), Lambda(_,
    App(_, // <- this function contained add, do I need this? ...
    // or can we get rid of this somehow?
    App(App(App(ReduceX(), op), _), _))
    // PairType  -> I need to be able to unzip
    // ArrayType -> I need to be able to transpose x._2
    ) ::: FunType(PairType(_,ArrayType(_,_)), _) // lambda.t
    ) ::: FunType(ArrayType(_,_), resultT) =>    // outermost app.t

      val result: TDSL[Rise] =
        (fun(x =>
          reduceSeq(
            fun((acc, y) => // acc::32.float, y::32.(float,float)
              map(fun(a => typed(op)(a._1)(a._2))) $ zip(acc,y)
            )
          )(x._1 :: resultT) o
            transpose $ x._2) o unzip) :: e.t

      Success(result)

      // "usual" case below --------------------------------------------------
      // this case already works for multiple dimensions (taken from old repo)
    case e@App(Map(), Lambda(mapVar,
         App(App(App(rx@(Reduce() | ReduceSeq()), op),
         init ::: (dt: DataType)), reduceArg)
    )) ::: FunType(inputT@ArrayType(size, ArrayType(_,_)), _) =>

    def reduceMap(zippedMapArg : (TDSL[Rise], TDSL[Rise]) => TDSL[Rise],
                  reduceArgFun: TDSL[Rise]): RewriteResult[Rise] = {
        Success((
          untyped(rx)(fun((acc, y) =>
            map(fun(x => app(app(op, fst(x)), snd(x)))) $ zippedMapArg(acc, y)
          ))(generate(fun(IndexType(size) ->: dt)(_ => init))) o reduceArgFun
        ) :: e.t)
      }

      reduceArg match {
        // simple case (see test "lift reduce")
        case x if x == mapVar =>
          reduceMap(
            (acc, y) => zip(acc, y),
            transpose
          )
        // zipped input (see test "MM to MM-LoopMKN")
        case App(App(Zip(), u), v) =>
          val notToBeTransposed = if (mapVar == u) v else u
          reduceMap(
            zippedMapArg = (acc, y) =>
              zip(acc, map(fun(bs => pair(bs, fst(y)))) $ snd(y)),
            reduceArgFun = zip(notToBeTransposed) o transpose
          )

        // expression is not in RNF!
        case a@App(_,_) => ???

        case _ =>
          // todo implement recursively
          val reduceArgTransposed = inputT match {
            case ArrayType(_, ArrayType(_, ArrayType(_,ArrayType(_,_)))) =>
              transpose o map(transpose) o map(map(transpose))
            case ArrayType(_, ArrayType(_, ArrayType(_,_))) =>
              transpose o map(transpose)
            case ArrayType(_, ArrayType(_,_)) => transpose
            case _ => ???
          }

          val result = reduceMap(
            (acc, y) => zip(acc, y),
            reduceArgTransposed
          )
          result
      }
  }

  // mapSnd f >> mapFst g -> mapFst g >> mapSnd f
  @rule def mapFstBeforeMapSnd: Strategy[Rise] = {
    case e @ App(App(MapFst(), g), App(App(MapSnd(), f), in)) =>
      Success(mapSnd(f)(mapFst(g, in)) :: e.t)
  }
}
