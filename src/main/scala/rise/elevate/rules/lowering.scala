package rise.elevate.rules

import arithexpr.arithmetic.Cst
import elevate.core.strategies.basic._
import elevate.core.strategies.predicate._
import elevate.core.strategies.traversal._
import elevate.core.strategies.{Traversable, predicate}
import elevate.core.{Failure, Strategy, Success}
import elevate.macros.RuleMacro.rule
import elevate.macros.StrategyMacro.strategy
import rise.core.DSL._
import rise.core.primitives.{not => _, _}
import rise.core.types.DataType._
import rise.core.types._
import rise.core.{primitives => p, _}
import rise.elevate._
import rise.elevate.rules.traversal._
import rise.elevate.strategies.normalForm.DFNF
import rise.elevate.strategies.predicate.{isVectorArray, _}
import rise.elevate.strategies.traversal._
import rise.openMP.{primitives => omp}

object lowering {

  // Straight-forward Lowering

  def typeHasTrivialCopy(t: ExprType): Boolean = t match {
    case _: ScalarType => true
    case NatType => true
    case _: IndexType => true
    case _: VectorType => true
    case _ => false
  }

  def `map -> mapSeq`: Strategy[Rise] = mapSeq
  @rule def mapSeq: Strategy[Rise] = {
    case m@map() => Success(p.mapSeq !: m.t)
  }

  def `map -> mapPar`: Strategy[Rise] = mapPar
  @rule def mapPar: Strategy[Rise] = {
    case m@map() => Success(omp.mapPar !: m.t)
  }

  def `map -> mapStream`: Strategy[Rise] = mapStream
  @rule def mapStream: Strategy[Rise] = {
    case m@map() => Success(p.mapStream !: m.t)
  }

  def `map -> iterateStream`: Strategy[Rise] = iterateStream
  @rule def iterateStream: Strategy[Rise] = {
    case m@map() => Success(p.iterateStream !: m.t)
  }

  def `map -> mapSeqUnroll`: Strategy[Rise] = mapSeqUnroll
  @rule def mapSeqUnroll: Strategy[Rise] = {
    case m@map() => Success(p.mapSeqUnroll !: m.t)
  }

  def `map -> mapGlobal`(dim: Int = 0): Strategy[Rise] = mapGlobal(dim)
  @rule def mapGlobal(dim: Int = 0): Strategy[Rise] = {
    case m@map() => Success(rise.openCL.DSL.mapGlobal(dim) !: m.t)
  }

  def `reduce -> reduceSeq`: Strategy[Rise] = reduceSeq
  @rule def reduceSeq: Strategy[Rise] = {
    case e@reduce() => Success(p.reduceSeq !: e.t)
  }

  def `reduce -> reduceSeqUnroll`: Strategy[Rise] = reduceSeqUnroll
  @rule def reduceSeqUnroll: Strategy[Rise] = {
    case e@reduce() => Success(p.reduceSeqUnroll !: e.t)
  }

  // Specialized Lowering

  @rule def mapSeqCompute()(implicit ev: Traversable[Rise]): Strategy[Rise] = {
    case e@App(map(), f) if containsComputation()(ev)(f) && predicate.not(isMappingZip)(f) =>
      Success(p.mapSeq(f) !: e.t)
  }

  @rule def isMappingZip: Strategy[Rise] = {
    case l@Lambda(_, App(App(zip(), a), b)) => Success(l)
    case m@Lambda(_, App(App(map(), f), arg)) => isMappingZip(f)
  }

  // TODO: load identity instead, then change with other rules?
  @rule def circularBuffer(load: Expr): Strategy[Rise] = {
    case e@DepApp(NatKind, DepApp(NatKind, slide(), sz: Nat), Cst(1)) => Success(
      p.circularBuffer(sz)(sz)(eraseType(load)) !: e.t)
  }

  @rule def rotateValues(write: Expr): Strategy[Rise] = {
    case e@DepApp(NatKind, DepApp(NatKind, slide(), sz: Nat), Cst(1)) => Success(
      p.rotateValues(sz)(eraseType(write)) !: e.t)
  }

  @rule def containsComputation()(implicit ev: Traversable[Rise]): Strategy[Rise] =
    topDown(isComputation())(ev)

  // requires type information!
  @rule def isComputation()(implicit ev: Traversable[Rise]): Strategy[Rise] = e => {
    def isPairOrBasicType(t: ExprType): Boolean = t match {
      case _ if typeHasTrivialCopy(t) => true
      case PairType(a, b) => isPairOrBasicType(a) && isPairOrBasicType(b)
      case _ => false
    }

    e match {
      // unary function (map)
      case l@Lambda(_, _) if isId(l) => Success(l)
      case l@Lambda(_, _) =>
        l.t match {
          // unary function
          case FunType(in, out) if
          isPairOrBasicType(in) && isPairOrBasicType(out) => Success(l)
          // binary function
          case FunType(in, FunType(in2, out)) if
          isPairOrBasicType(in) && isPairOrBasicType(in2) &&
            isPairOrBasicType(out) => Success(l)
          case _ => Failure(containsComputation())
        }
      case f@foreignFunction(_, _) => Success(f)
      case _ => Failure(containsComputation())
    }
  }


//  case class slideSeq(rot: SlideSeq.Rotate, write_dt1: Expr) extends Strategy[Rise] {
//    def apply(e: Rise): RewriteResult[Rise] = e match {
//      case slide() => Success(nFun(sz => nFun(sp =>
//        TypedDSL.slideSeq(rot)(sz)(sp)(untyped(write_dt))
//      )) :: e.t)
//      case _ => Failure(slideSeq(rot, write_dt))
//    }
//    override def toString = s"slideSeq($rot, $write_dt)"
//  }

  // writing to memory

  // TODO: think about more complex cases
  @rule def mapSeqUnrollWrite: Strategy[Rise] = e => e.t match {
    case ArrayType(_, t) if typeHasTrivialCopy(t) =>
      Success(app(p.mapSeqUnroll(fun(x => x)), preserveType(e)) !: e.t)
    case _ =>
      Failure(mapSeqUnrollWrite)
  }

  @rule def toMemAfterMapSeq: Strategy[Rise] = {
    case a@App(App(p.mapSeq(), _), _) =>
      Success((preserveType(a) |> p.toMem) !: a.t)
  }

  // Lowerings used in PLDI submission

  // adds copy after every generate
  def materializeGenerate()(implicit ev: Traversable[Rise]): Strategy[Rise] =
    normalize(ev)(
      argument(function(isGenerate)) `;`
        not(isCopy) `;`
        argument(copyAfterGenerate)
    )

  // adds explicit copies for every init value in reductions
  def materializeInitOfReduce()(implicit ev: Traversable[Rise]): Strategy[Rise] =
    normalize(ev)(
      function(function(isReduceX)) `;`
        argument(not(isCopy) `;` insertCopyAfter)
    )

  @rule def insertCopyAfter: Strategy[Rise] = e => {
    def constructCopy(t: ExprType): ToBeTyped[Rise] = t match {
      case ArrayType(_, dt) => p.mapSeq(fun(x => constructCopy(dt) $ x))
      case _ if typeHasTrivialCopy(t) => fun(x => x)
      case _ => ??? // shouldn't happen?
    }

    Success(constructCopy(e.t) $ e)
  }

  // todo currently only works for mapSeq
  @rule def isCopy: Strategy[Rise] = {
    case c@App(p.let(), id) if isId(id) => Success(c)
    case c@App(App(p.mapSeq(), id), etaInput) if isId(id) => Success(c)
    case App(App(p.mapSeq(), Lambda(_, f)), etaInput) => isCopy(f)
    case c@App(id, _) if isId(id) => Success(c)
  }

  @rule def isId: Strategy[Rise] = {
    case l@Lambda(x1, x2) if x1 =~= x2 => Success(l)
  }

  // requires expr to be in LCNF
  def specializeSeq()(implicit ev: Traversable[Rise]): Strategy[Rise] =
    normalize(ev)(lowering.mapSeqCompute() <+ lowering.reduceSeq)

  def addRequiredCopies()(implicit ev: Traversable[Rise]): Strategy[Rise] =
    // `try`(oncetd(copyAfterReduce)) `;` LCNF `;` materializeInitOfReduce
    tryAll(copyAfterReduce) `;` DFNF() `;` materializeInitOfReduce()

  // todo gotta use a normalform for introducing copies! e.g., if we have two reduce primitives
  def lowerToC(implicit ev: Traversable[Rise]): Strategy[Rise] =
    addRequiredCopies() `;` specializeSeq()


  // todo currently only works for mapSeq
  @rule def copyAfterReduce: Strategy[Rise] = e => {
    def constructCopy(t: ExprType): ToBeTyped[Rise] = t match {
      case _ if typeHasTrivialCopy(t) => letf(fun(x => x))
      case ArrayType(_, b) if typeHasTrivialCopy(b) => p.mapSeq(fun(x => x))
      case ArrayType(_, a: ArrayType) => p.mapSeq(fun(x => constructCopy(a) $ x))
      case _ => ??? // shouldn't happen?
    }

    e match {
      case reduceResult@App(App(App(ReduceX(), _), _), _) =>
        Success((preserveType(e) |> constructCopy(reduceResult.t) ) !: e.t)
      case _ => Failure(copyAfterReduce)
    }
  }

  @rule def copyAfterReduceInit: Strategy[Rise] = e => {
    def constructCopy(t: ExprType): ToBeTyped[Rise] = t match {
      case _ if typeHasTrivialCopy(t) => letf(fun(x => x))
      case ArrayType(_, b) if typeHasTrivialCopy(b) => p.mapSeq(fun(x => x))
      case ArrayType(_, a: ArrayType) => p.mapSeq(fun(x => constructCopy(a) $ x))
      case x => println(x) ; ??? // shouldn't happen?
    }

    e match {
      case App(a@App(ReduceX(), _), init) =>
        Success((preserveType(init) |> constructCopy(init.t) |> a) !: e.t)
      case _ => Failure(copyAfterReduceInit)
    }
  }

  // todo currently only works for mapSeq
  @rule def copyAfterGenerate: Strategy[Rise] = e => {
    def constructCopy(t: ExprType): ToBeTyped[Rise] = t match {
      case ArrayType(_, dt) => p.mapSeq(fun(x => constructCopy(dt) $ x))
      case _ if typeHasTrivialCopy(t) => fun(x => x)
      case _ => ??? // shouldn't happen?
    }

    e match {
      case a@App(generate(), _) =>
        Success((preserveType(a) |> constructCopy(a.t)) !: e.t)
      case _ => Failure(copyAfterGenerate)
    }
  }

  @rule def toMemAfterAsScalar: Strategy[Rise] = {
    case a@App(asScalar(), _) => Success((preserveType(a) |> p.toMem) !: a.t)
  }

  @rule def toMemAfter: Strategy[Rise] =
    e => Success((preserveType(e) |> p.toMem) !: e.t)

  @rule def toMemBefore: Strategy[Rise] = {
    case a@App(f, e) => Success((p.toMem(e) |> preserveType(f)) !: a.t)
  }

  @strategy
  def storeTempsAsScalars: Strategy[Rise] =
    innermost(isApplied(isPrimitive(asScalar)))(toMemAfter)

  @strategy
  def storeTempAsVectors: Strategy[Rise] =
    innermost(isApplied(isPrimitive(asScalar)))(toMemBefore)

  def `map(f) -> asVector >> map(f_vec) >> asScalar`(n: Nat): Strategy[Rise] =
    vectorize(n)(default.RiseTraversable)

  @rule def vectorize(n: Nat)(implicit ev: Traversable[Rise]): Strategy[Rise] = {
    case a@App(App(map(), f), input) if
      isComputation()(ev)(f) && !isVectorArray(a.t) =>

      def vectorizeArrayBasedOnType(t: ExprType): ToBeTyped[Rise] = {
        def generateUnZips(dt: ExprType): ToBeTyped[Rise] = {
          dt match {
            case _ if typeHasTrivialCopy(dt) => asVectorAligned(n)
            case PairType(aT, bT) => fun(x =>
              zip(generateUnZips(aT) $ x.`1`)(generateUnZips(bT) $ x.`2`)) o unzip
            case x => println(x) ; ???
          }
        }

        t match {
          case ArrayType(_, dt) => generateUnZips(dt) // remove first array layer
          case _ => ??? // shouldnt happen
        }
      }

      val newF = eraseType(f)
      Success(
        toBeTyped(input) |> vectorizeArrayBasedOnType(input.t) |> (map(newF) >> asScalar)
      )
    case _ => Failure(vectorize(n))
  }

  @rule def untype: Strategy[Rise] = p => Success(p.setType(TypePlaceholder))

  def parallel()(implicit ev: Traversable[Rise]): Strategy[Rise] = mapParCompute()
  @rule def mapParCompute()(implicit ev: Traversable[Rise]): Strategy[Rise] = {
    case e@App(map(), f) if containsComputation()(ev)(f) => Success(omp.mapPar(f) !: e.t)
  }

  @rule def unroll: Strategy[Rise] = {
    case e@p.reduceSeq() => Success(p.reduceSeqUnroll !: e.t)
  }

  object ocl {
    import rise.core.types.AddressSpace
    import rise.openCL.primitives._

    // TODO shall we allow lowering from an already lowered reduceSeq?
    @rule def reduceSeqUnroll(a: AddressSpace): Strategy[Rise] = {
      case e@reduce() => Success(oclReduceSeqUnroll(a) !: e.t)
      case e@p.reduceSeq() => Success(oclReduceSeqUnroll(a) !: e.t)
    }

    @rule def circularBuffer(a: AddressSpace): Strategy[Rise] = {
      case e@DepApp(NatKind, DepApp(NatKind, slide(), n: Nat), Cst(1)) =>
        Success(
          oclCircularBuffer(a)(n)(n)(fun(x => x))
            !: e.t)
    }

    @rule def circularBufferLoadFusion: Strategy[Rise] = {
      case e@App(App(
        cb @ DepApp(NatKind, DepApp(NatKind, DepApp(AddressSpaceKind, oclCircularBuffer(), _), _), _),
        load), App(App(map(), f), in)
      ) =>
        Success(eraseType(cb)(preserveType(f) >> load, in) !: e.t)
    }

    @rule def rotateValues(a: AddressSpace, write: Expr): Strategy[Rise] = {
      case e@DepApp(NatKind, DepApp(NatKind, slide(), n: Nat), Cst(1)) =>
        Success(
          oclRotateValues(a)(n)(eraseType(write))
            !: e.t)
    }
  }
}
