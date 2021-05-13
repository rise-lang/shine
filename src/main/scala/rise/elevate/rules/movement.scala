package rise.elevate.rules

import elevate.core.strategies.Traversable
import elevate.core.strategies.predicate._
import elevate.core._
import elevate.macros.RuleMacro.rule
import rise.elevate._
import rise.core._
import rise.core.types._
import rise.core.primitives._
import rise.core.DSL._
import rise.core.DSL.Type._
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
      transpose(),
      App(App(map(), App(map(), f)), y)) =>
        Success((preserveType(y) |> transpose |> map(map(f))) !: e)
    // LCNF
    case e@App(transpose(),
    App(
      App(map(), lamA @ Lambda(_, App(
              App(map(), lamB @ Lambda(_, App(f, _))), _))),
      arg)
    ) if etaReduction()(ev)(lamA) && etaReduction()(ev)(lamB) =>
      // Success((typed(arg) |> transpose |> map(map(f))) :: e.t)
      Success((preserveType(arg) |> transpose |> map(fun(a => map(fun(b => preserveType(f)(b)))(a)))) !: e)
  }

  def transposeBeforeMapMapF: Strategy[Rise] = `T >> **f -> **f >> T`
  @rule def `T >> **f -> **f >> T`: Strategy[Rise] = {
    case e@App(App(map(), App(map(), f)), App(transpose(), y)) =>
      Success((preserveType(y) |> map(map(f)) |> transpose) !: e)
  }


  // split/slide

  def isSplitOrSlide(s: Expr): Boolean = s match {
    case DepApp(DepApp(slide(), _: Nat), _: Nat) => true
    case DepApp(split(), _: Nat)                 => true
    case _                                       => false
  }

  def slideBeforeMapMapF: Strategy[Rise] = `S >> **f -> *f >> S`
  @rule def `S >> **f -> *f >> S`: Strategy[Rise] = {
    case e@App(App(map(), App(map(), f)), App(s, y)) if isSplitOrSlide(s) =>
      Success((preserveType(y) |> map(f) |> eraseType(s)) !: e)
  }

  def slideBeforeMap: Strategy[Rise] = `*f >> S -> S >> **f`
  @rule def `*f >> S -> S >> **f`: Strategy[Rise] = {
    case e@App(s @ DepApp(DepApp(slide(), _: Nat), _: Nat), App(App(map(), f), y)) =>
      Success((preserveType(y) |> eraseType(s) |> map(map(f))) !: e)
  }

  // *f >> S -> S >> **f
  @rule def splitBeforeMap: Strategy[Rise] = {
    case e@App(s @ DepApp(split(), _: Nat), App(App(map(), f), y)) =>
      Success((preserveType(y) |> eraseType(s) |> map(map(f))) !: e)
  }

  // join

  def joinBeforeMapF: Strategy[Rise] = `J >> *f -> **f >> J`
  @rule def `J >> *f -> **f >> J`: Strategy[Rise] = {
    case e@App(App(map(), f),App(join(), y)) =>
      Success((preserveType(y) |> map(map(f)) >> join) !: e)
  }

  def mapMapFBeforeJoin: Strategy[Rise] = `**f >> J -> J >> *f`
  @rule def `**f >> J -> J >> *f`: Strategy[Rise] = {
    case e@App(join(), App(App(map(), App(map(), f)), y)) =>
      Success((preserveType(y) |> join |> map(f)) !: e)
  }

  // drop and take

  def dropBeforeMap: Strategy[Rise] = `*f >> drop n -> drop n >> *f`
  @rule def `*f >> drop n -> drop n >> *f`: Strategy[Rise] = {
    case expr @ App(DepApp(drop(), n: Nat), App(App(map(), f), in)) =>
      Success(app(map(f), app(drop(n), preserveType(in))) !: expr)
  }

  def takeBeforeMap: Strategy[Rise] = `*f >> take n -> take n >> *f`
  @rule def `*f >> take n -> take n >> *f`: Strategy[Rise] = {
    case expr @ App(DepApp(take(), n: Nat), App(App(map(), f), in)) =>
      Success(app(map(f), app(take(n), preserveType(in))) !: expr)
  }

  // take n >> *f -> *f >> take n
  @rule def takeAfterMap: Strategy[Rise] = {
    case e @ App(App(map(), f), App(DepApp(take(), n: Nat), in)) =>
      Success(take(n)(map(f)(in)) !: e)
  }

  def takeInZip: Strategy[Rise] = `take n (zip a b) -> zip (take n a) (take n b)`
  @rule def `take n (zip a b) -> zip (take n a) (take n b)`: Strategy[Rise] = {
    case expr @ App(DepApp(take(), n), App(App(zip(), a), b)) =>
      Success(zip(depApp(take, n)(a))(depApp(take, n)(b)) !: expr)
  }

  // zip (take n a) (take n b) -> take n (zip a b)
  @rule def takeOutisdeZip: Strategy[Rise] = {
    case e @ App(App(zip(),
      App(DepApp(take(), n1: Nat), a)), App(DepApp(take(), n2: Nat), b)
    ) if n1 == n2 =>
      Success(take(n1)(zip(a)(b)) !: e)
  }

  // pair (take n a) (take m b) -> pair a b >> mapFst take n >> mapSnd take m
  // TODO: can get any function out, see asScalarOutsidePair
  @rule def takeOutsidePair: Strategy[Rise] = {
    case e @ App(App(makePair(),
      App(DepApp(take(), n: Nat), a)), App(DepApp(take(), m: Nat), b)
    ) =>
      Success((makePair(a)(b) |> mapFst(take(n)) |> mapSnd(take(m))) !: e)
  }

  def dropInZip: Strategy[Rise] = `drop n (zip a b) -> zip (drop n a) (drop n b)`
  @rule def `drop n (zip a b) -> zip (drop n a) (drop n b)`: Strategy[Rise] = {
    case expr @ App(DepApp(drop(), n), App(App(zip(), a), b)) =>
      Success(zip(depApp(drop, n)(a))(depApp(drop, n)(b)) !: expr)
  }

  def takeInSelect: Strategy[Rise] = `take n (select t a b) -> select t (take n a) (take n b)`
  @rule def `take n (select t a b) -> select t (take n a) (take n b)`: Strategy[Rise] = {
    case expr @ App(DepApp(take(), n), App(App(App(select(), t), a), b)) =>
      Success(select(t)(depApp(take, n)(a), depApp(take, n)(b)) !: expr)
  }

  def dropInSelect: Strategy[Rise] = `drop n (select t a b) -> select t (drop n a) (drop n b)`
  @rule def `drop n (select t a b) -> select t (drop n a) (drop n b)`: Strategy[Rise] = {
    case expr @ App(DepApp(drop(), n), App(App(App(select(), t), a), b)) =>
      Success(select(t)(depApp(drop, n)(a), depApp(drop, n)(b)) !: expr)
  }

  def dropBeforeTake: Strategy[Rise] = `take (n+m) >> drop m -> drop m >> take n`
  @rule def `take (n+m) >> drop m -> drop m >> take n`: Strategy[Rise] = {
    case expr @ App(DepApp(drop(), m: Nat), App(DepApp(take(), nm: Nat), in)) =>
      Success(app(take(nm - m), app(drop(m), preserveType(in))) !: expr)
  }

  def takeBeforeDrop: Strategy[Rise] = `drop m >> take n -> take (n+m) >> drop m`
  @rule def `drop m >> take n -> take (n+m) >> drop m`: Strategy[Rise] = {
    case expr @ App(DepApp(take(), n: Nat), App(DepApp(drop(), m: Nat), in)) =>
      Success(app(drop(m), app(take(n+m), preserveType(in))) !: expr)
  }

  def takeBeforeSlide: Strategy[Rise] = `slide n m >> take t -> take (m * (t - 1) + n) >> slide n m`
  @rule def `slide n m >> take t -> take (m * (t - 1) + n) >> slide n m`: Strategy[Rise] = {
    case expr @ App(DepApp(take(), t: Nat), App(DepApp(DepApp(slide(), n: Nat), m: Nat), in)) =>
      Success(app(slide(n)(m), take(m * (t - 1) + n)(in)) !: expr)
  }

  def dropBeforeSlide: Strategy[Rise] = `slide n m >> drop d -> drop (d * m) >> slide n m`
  @rule def `slide n m >> drop d -> drop (d * m) >> slide n m`: Strategy[Rise] = {
    case expr @ App(DepApp(drop(), d: Nat), App(DepApp(DepApp(slide(), n: Nat), m: Nat), in)) =>
      Success(app(slide(n)(m), drop(d * m)(in)) !: expr)
  }

  // slide n m >> padEmpty p -> padEmpty (p * m) >> slide n m
  @rule def padEmptyBeforeSlide: Strategy[Rise] = {
    case e @ App(DepApp(padEmpty(), p: Nat),
      App(DepApp(DepApp(slide(), n: Nat), m: Nat), in)
    ) =>
      Success(slide(n)(m)(padEmpty(p * m)(in)) !: e)
  }

  // map f >> padEmpty n -> padEmpty n >> map f
  @rule def padEmptyBeforeMap: Strategy[Rise] = {
    case e @ App(DepApp(padEmpty(), n: Nat), App(App(map(), f), in)) =>
      Success(map(f)(padEmpty(n)(in)) !: e)
  }

  // transpose >> padEmpty n -> map (padEmpty n) >> transpose
  @rule def padEmptyBeforeTranspose: Strategy[Rise] = {
    case e @ App(DepApp(padEmpty(), n: Nat), App(transpose(), in)) =>
      Success(transpose(map(padEmpty(n))(in)) !: e)
  }

  // padEmpty n (zip a b) -> zip (padEmpty n a) (padEmpty n b)
  @rule def padEmptyInsideZip: Strategy[Rise] = {
    case e @ App(DepApp(padEmpty(), n: Nat), App(App(zip(), a), b)) =>
      Success(zip(padEmpty(n)(a))(padEmpty(n)(b)) !: e)
  }

  // FIXME: this is very specific
  // zip (fst e) (snd e) |> padEmpty n ->
  // (mapFst padEmpty n) (mapSnd padEmpty n) |> fun(p => zip (fst p) (snd(p))
  @rule def padEmptyBeforeZip: Strategy[Rise] = {
    case e @ App(DepApp(padEmpty(), n: Nat),
      App(App(zip(), App(fst(), e1)), App(snd(), e2)))
    if e1 =~= e2 =>
      Success((preserveType(e1) |>
        mapFst(padEmpty(n)) |> mapSnd(padEmpty(n)) |>
        fun(p => zip(fst(p))(snd(p)))) !: e)
  }

  // special-cases
  // slide + transpose

  def transposeBeforeSlide: Strategy[Rise] = `T >> S -> *S >> T >> *T`
  @rule def `T >> S -> *S >> T >> *T`: Strategy[Rise] = {
    case e@App(s, App(transpose(), y)) if isSplitOrSlide(s) =>
      Success((preserveType(y) |> map(eraseType(s)) |> transpose.apply |> map(transpose)) !: e)
  }

  def transposeBeforeMapSlide: Strategy[Rise] = `T >> *S -> S >> *T >> T`
  @rule def `T >> *S -> S >> *T >> T`: Strategy[Rise] = {
    case e@App(App(map(), s), App(transpose(), y)) if isSplitOrSlide(s) =>
      Success((preserveType(y) |> eraseType(s) |> map(transpose) |> transpose) !: e)
  }

  def mapSlideBeforeTranspose: Strategy[Rise] = `*S >> T -> T >> S >> *T`
  @rule def `*S >> T -> T >> S >> *T`: Strategy[Rise] = {
    case e@App(transpose(), App(App(map(), s), y)) if isSplitOrSlide(s) =>
      Success((preserveType(y) |> transpose.apply |> eraseType(s) |> map(transpose)) !: e)
  }

  // transpose + join

  def joinBeforeTranspose: Strategy[Rise] = `J >> T -> *T >> T >> *J`
  @rule def `J >> T -> *T >> T >> *J`: Strategy[Rise] = {
    case e@App(transpose(), App(join(), y)) =>
      Success((preserveType(y) |> map(transpose) |> transpose |> map(join)) !: e)
  }

  def transposeBeforeMapJoin: Strategy[Rise] = `T >> *J -> *T >> J >> T`
  @rule def `T >> *J -> *T >> J >> T`: Strategy[Rise] = {
    case e@App(App(map(), join()), App(transpose(), y)) =>
      Success((preserveType(y) |> map(transpose) |> join |> transpose) !: e)
  }

  def mapTransposeBeforeJoin: Strategy[Rise] = `*T >> J -> T >> *J >> T`
  @rule def `*T >> J -> T >> *J >> T`: Strategy[Rise] = {
    case e@App(join(), App(App(map(), transpose()), y)) =>
      Success((preserveType(y) |> transpose |> map(join) |> transpose) !: e)
  }

  def mapJoinBeforeTranspose: Strategy[Rise] = `*J >> T -> T >> *T >> J`
  @rule def `*J >> T -> T >> *T >> J`: Strategy[Rise] = {
    case e@App(transpose(), App(App(map(), join()), y)) =>
      Success((preserveType(y) |> transpose |> map(transpose) |> join) !: e)
  }

  // join + join

  def joinBeforeJoin: Strategy[Rise] = `J >> J -> *J >> J`
  @rule def `J >> J -> *J >> J`: Strategy[Rise] = {
    case e@App(join(), App(join(), y)) =>
      Success((preserveType(y) |> map(join) >> join) !: e)
  }

  def mapJoinBeforeJoin: Strategy[Rise] = `*J >> J -> J >> J`
  @rule def `*J >> J -> J >> J`: Strategy[Rise] = {
    case e@App(join(), App(App(map(), join()), y)) =>
      Success((preserveType(y) |> join |> join) !: e)
  }

  // split + slide

  def slideBeforeSplit: Strategy[Rise] = `slide(n)(s) >> split(k) -> slide(k+n-s)(k) >> map(slide(n)(s))`
  @rule def `slide(n)(s) >> split(k) -> slide(k+n-s)(k) >> map(slide(n)(s))`: Strategy[Rise] = {
    case e@App(DepApp(split(), k: Nat), App(DepApp(DepApp(slide(), n: Nat), s: Nat), y)) =>
      Success((preserveType(y) |> slide(k + n - s)(k) |> map(slide(n)(s))) !: e)
  }

  // TODO: what if s != 1?
  // slide(n)(s=1) >> slide(m)(k) -> slide(m+n-1)(k) >> map(slide(n)(1))
  @rule def slideBeforeSlide: Strategy[Rise] = {
    case e@App(DepApp(DepApp(slide(), m: Nat), k: Nat),
            App(DepApp(DepApp(slide(), n: Nat), s: Nat), in)
         ) if s == (1: Nat) =>
      Success((preserveType(in) |> slide(m+n-s)(k) |> map(slide(n)(s))) !: e)
  }

  // nested map + reduce

  // different variants for rewriting map(reduce) to reduce(map)
  // todo what makes them different? can we decompose them into simpler rules?
  @rule def liftReduce: Strategy[Rise] = {

     // 2D array of pairs ----------------------------------------------------
    case e@App(map(), Lambda(_,
    App(App(App(ReduceX(), op),
    _), _)
    // PairType is new here
    )) ::: FunType(ArrayType(_, ArrayType(_,PairType(_,_))), resultT) =>

      val result: ToBeTyped[Rise] = (fun(x =>
        (reduceSeq(fun((acc, y) =>
          map(fun(a => preserveType(op)(a._1)(a._2))) $ zip(acc)(y)
        ))(x._1 :: resultT) o // x._1 :: 2D array
          // transpose2D
          transpose o map(transpose) $ x._2)) o
        // unzip2D
        unzip o map(unzip)) !: e

      Success(result)

      // yet another case ----------------------------------------------------
    /*
  documentation for the concrete case I had:
  32.(float, 4.(float,float)) -> 32.float (via map(f o reduce))
  (32.float, 32.4.(float,float)) [unzip]
  (32.float, 4.32.(float,float)) [transpose $ x._2]
  now reducing (map(+)) x._2, using x_.1 as init,
   */
    case e@App(map(), Lambda(_,
    App(_, // <- this function contained add, do I need this? ...
    // or can we get rid of this somehow?
    App(App(App(ReduceX(), op), _), _))
    // PairType  -> I need to be able to unzip
    // ArrayType -> I need to be able to transpose x._2
    ) ::: FunType(PairType(_,ArrayType(_,_)), _) // lambda.t
    ) ::: FunType(ArrayType(_,_), resultT) =>    // outermost app.t

      val result: ToBeTyped[Rise] =
        (fun(x =>
          reduceSeq(
            fun((acc, y) => // acc::32.float, y::32.(float,float)
              map(fun(a => preserveType(op)(a._1)(a._2))) $ zip(acc)(y)
            )
          )(x._1 :: resultT) o
            transpose $ x._2) o unzip) !: e

      Success(result)

      // "usual" case below --------------------------------------------------
      // this case already works for multiple dimensions (taken from old repo)
    case e@App(map(), Lambda(mapVar,
         App(App(App(rx@(reduce() | reduceSeq()), op),
         init ::: (dt: DataType)), reduceArg)
    )) ::: FunType(inputT@ArrayType(size, ArrayType(_,_)), _) =>

    def reduceMap(zippedMapArg : (ToBeTyped[Rise], ToBeTyped[Rise]) => ToBeTyped[Rise],
                  reduceArgFun: ToBeTyped[Rise]): RewriteResult[Rise] = {
        Success((
          eraseType(rx)(fun((acc, y) =>
            map(fun(x => app(app(op, fst(x)), snd(x)))) $ zippedMapArg(acc, y)
          ))(generate(fun(IndexType(size) ->: dt)(_ => init))) o reduceArgFun
        ) !: e)
      }

      reduceArg match {
        // simple case (see test "lift reduce")
        case x if x =~= mapVar =>
          reduceMap(
            (acc, y) => zip(acc)(y),
            transpose
          )
        // zipped input (see test "MM to MM-LoopMKN")
        case App(App(zip(), u), v) =>
          val notToBeTransposed = if (mapVar =~= u) v else u
          reduceMap(
            zippedMapArg = (acc, y) =>
              zip(acc)(map(fun(bs => makePair(bs)(fst(y)))) $ snd(y)),
            reduceArgFun = zip(notToBeTransposed) o transpose
          )

        // expression is not in RNF!
        case a@App(_,_) => ???

        case _ =>
          // todo implement recursively
          val reduceArgTransposed: ToBeTyped[Rise] = inputT match {
            case ArrayType(_, ArrayType(_, ArrayType(_,ArrayType(_,_)))) =>
              transpose o map(transpose) o map(map(transpose))
            case ArrayType(_, ArrayType(_, ArrayType(_,_))) =>
              transpose o map(transpose)
            case ArrayType(_, ArrayType(_,_)) => transpose
            case _ => ???
          }

          val result = reduceMap(
            (acc, y) => zip(acc)(y),
            reduceArgTransposed
          )
          result
      }
  }

  // mapSnd f >> mapFst g -> mapFst g >> mapSnd f
  @rule def mapFstBeforeMapSnd: Strategy[Rise] = {
    case e @ App(App(mapFst(), g), App(App(mapSnd(), f), in)) =>
      Success(mapSnd(f)(mapFst(g)(in)) !: e)
  }
}
