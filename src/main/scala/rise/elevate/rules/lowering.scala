package rise.elevate.rules

import arithexpr.arithmetic.Cst
import elevate.core.strategies.basic._
import elevate.core.strategies.predicate._
import elevate.core.strategies.traversal._
import elevate.core.strategies.{Traversable, predicate}
import elevate.core._
import elevate.core.macros._
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
  def mapSeq: Strategy[Rise] = rule("mapSeq", {
    case m@map() => Success(p.mapSeq !: m.t)
  })

  def `map -> mapPar`: Strategy[Rise] = mapPar
  def mapPar: Strategy[Rise] = rule("mapPar", {
    case m@map() => Success(omp.mapPar !: m.t)
  })

  def `map -> mapStream`: Strategy[Rise] = mapStream
  def mapStream: Strategy[Rise] = rule("mapStream", {
    case m@map() => Success(p.mapStream !: m.t)
  })

  def `map -> iterateStream`: Strategy[Rise] = iterateStream
  def iterateStream: Strategy[Rise] = rule("iterateStream", {
    case m@map() => Success(p.iterateStream !: m.t)
  })

  def `map -> mapSeqUnroll`: Strategy[Rise] = mapSeqUnroll
  def mapSeqUnroll: Strategy[Rise] = rule("mapSeqUnroll", {
    case m@map() => Success(p.mapSeqUnroll !: m.t)
  })

  def `map -> mapGlobal`(dim: Int = 0): Strategy[Rise] = mapGlobal(dim)
  def mapGlobal(dim: Int = 0): Strategy[Rise] = rule("mapGlobal", {
    case m@map() => Success(rise.openCL.DSL.mapGlobal(dim) !: m.t)
  })

  def `reduce -> reduceSeq`: Strategy[Rise] = reduceSeq
  def reduceSeq: Strategy[Rise] = rule("reduceSeq", {
    case e@reduce() => Success(p.reduceSeq !: e.t)
  })

  def `reduce -> reduceSeqUnroll`: Strategy[Rise] = reduceSeqUnroll
  def reduceSeqUnroll: Strategy[Rise] = rule("reduceSeqUnroll", {
    case e@reduce() => Success(p.reduceSeqUnroll !: e.t)
  })

  // Specialized Lowering

  def mapSeqCompute()(implicit ev: Traversable[Rise]): Strategy[Rise] = rule("mapSeqCompute", {
    case e@App(map(), f) if containsComputation()(ev)(f) && predicate.not(isMappingZip)(f) =>
      Success(p.mapSeq(f) !: e.t)
  })

  def isMappingZip: Strategy[Rise] = rule("isMappingZip", {
    case l@Lambda(_, App(App(zip(), a), b)) => Success(l)
    case m@Lambda(_, App(App(map(), f), arg)) => isMappingZip(f)
  })

  // TODO: load identity instead, then change with other rules?
  def circularBuffer(load: Expr): Strategy[Rise] = rule("circularBuffer", {
    case e@DepApp(NatKind, DepApp(NatKind, slide(), sz: Nat), Cst(1)) => Success(
      p.circularBuffer(sz)(sz)(eraseType(load)) !: e.t)
  })

  def rotateValues(write: Expr): Strategy[Rise] = rule("rotateValues", {
    case e@DepApp(NatKind, DepApp(NatKind, slide(), sz: Nat), Cst(1)) => Success(
      p.rotateValues(sz)(eraseType(write)) !: e.t)
  })

  def containsComputation()(implicit ev: Traversable[Rise]): Strategy[Rise] =
    rule("containsComputation", topDown(isComputation())(ev))

  // requires type information!
  def isComputation()(implicit ev: Traversable[Rise]): Strategy[Rise] = rule("isComputation", e => {
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
  })


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
  def mapSeqUnrollWrite: Strategy[Rise] = rule("mapSeqUnrollWrite", e => e.t match {
    case ArrayType(_, t) if typeHasTrivialCopy(t) =>
      Success(app(p.mapSeqUnroll(fun(x => x)), preserveType(e)) !: e.t)
    case _ =>
      Failure(mapSeqUnrollWrite)
  })

  def toMemAfterMapSeq: Strategy[Rise] = rule("toMemAfterMapSeq", {
    case a@App(App(p.mapSeq(), _), _) =>
      Success((preserveType(a) |> p.toMem) !: a.t)
  })

  // Lowerings used in PLDI submission

  // adds copy after every generate
  def materializeGenerate()(using ev: Traversable[Rise]): Strategy[Rise] =
    normalize(
      argument(function(isGenerate)) `;`
        not(isCopy) `;`
        argument(copyAfterGenerate)
    )

  // adds explicit copies for every init value in reductions
  def materializeInitOfReduce()(using ev: Traversable[Rise]): Strategy[Rise] =
    normalize(
      function(function(isReduceX)) `;`
        argument(not(isCopy) `;` insertCopyAfter)
    )

  def insertCopyAfter: Strategy[Rise] = rule("insertCopyAfter", e => {
    def constructCopy(t: ExprType): ToBeTyped[Rise] = t match {
      case ArrayType(_, dt) => p.mapSeq(fun(x => constructCopy(dt) $ x))
      case _ if typeHasTrivialCopy(t) => fun(x => x)
      case _ => ??? // shouldn't happen?
    }

    Success(constructCopy(e.t) $ e)
  })

  // todo currently only works for mapSeq
  def isCopy: Strategy[Rise] = rule("isCopy", {
    case c@App(p.let(), id) if isId(id) => Success(c)
    case c@App(App(p.mapSeq(), id), etaInput) if isId(id) => Success(c)
    case App(App(p.mapSeq(), Lambda(_, f)), etaInput) => isCopy(f)
    case c@App(id, _) if isId(id) => Success(c)
  })

  def isId: Strategy[Rise] = rule("isId", {
    case l@Lambda(x1, x2) if x1 =~= x2 => Success(l)
  })

  // requires expr to be in LCNF
  def specializeSeq()(using ev: Traversable[Rise]): Strategy[Rise] =
    normalize(lowering.mapSeqCompute() <+ lowering.reduceSeq)

  def addRequiredCopies()(using ev: Traversable[Rise]): Strategy[Rise] =
    // `try`(oncetd(copyAfterReduce)) `;` LCNF `;` materializeInitOfReduce
    tryAll(copyAfterReduce) `;` DFNF() `;` materializeInitOfReduce()

  // todo gotta use a normalform for introducing copies! e.g., if we have two reduce primitives
  def lowerToC(using ev: Traversable[Rise]): Strategy[Rise] =
    addRequiredCopies() `;` specializeSeq()


  // todo currently only works for mapSeq
  def copyAfterReduce: Strategy[Rise] = rule("copyAfterReduce", e => {
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
  })

  def copyAfterReduceInit: Strategy[Rise] = rule("copyAfterReduceInit", e => {
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
  })

  // todo currently only works for mapSeq
  def copyAfterGenerate: Strategy[Rise] = rule("copyAfterGenerate", e => {
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
  })

  def toMemAfterAsScalar: Strategy[Rise] = rule("toMemAfterAsScalar", {
    case a@App(asScalar(), _) => Success((preserveType(a) |> p.toMem) !: a.t)
  })

  def toMemAfter: Strategy[Rise] = rule("toMemAfter",
    e => Success((preserveType(e) |> p.toMem) !: e.t))

  def toMemBefore: Strategy[Rise] = rule("toMemBefore", {
    case a@App(f, e) => Success((p.toMem(e) |> preserveType(f)) !: a.t)
  })

  def storeTempsAsScalars: Strategy[Rise] =
    strategy("storeTempsAsScalars", innermost(isApplied(isPrimitive(asScalar)))(toMemAfter))

  def storeTempAsVectors: Strategy[Rise] =
    strategy("storeTempAsVectors", innermost(isApplied(isPrimitive(asScalar)))(toMemBefore))

  def `map(f) -> asVector >> map(f_vec) >> asScalar`(n: Nat): Strategy[Rise] =
    vectorize(n)(default.RiseTraversable)

  def vectorize(n: Nat)(implicit ev: Traversable[Rise]): Strategy[Rise] = rule("vectorize", {
    case a@App(App(map(), f), input) if
      isComputation()(ev)(f) && !isVectorArray(a.t) =>

      def vectorizeArrayBasedOnType(t: ExprType): ToBeTyped[Rise] = {
        def generateUnZips(dt: ExprType): ToBeTyped[Rise] = {
          dt match {
            case _ if typeHasTrivialCopy(dt) => asVectorAligned(n)
            case PairType(aT, bT) => fun(x =>
              zip(generateUnZips(aT) $ x._1)(generateUnZips(bT) $ x._2)) o unzip
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
  })

  def untype: Strategy[Rise] = rule("untype", p => Success(p.setType(TypePlaceholder)))

  def parallel()(implicit ev: Traversable[Rise]): Strategy[Rise] = mapParCompute()
  def mapParCompute()(implicit ev: Traversable[Rise]): Strategy[Rise] = rule("mapParCompute", {
    case e@App(map(), f) if containsComputation()(ev)(f) => Success(omp.mapPar(f) !: e.t)
  })

  def unroll: Strategy[Rise] = rule("unroll", {
    case e@p.reduceSeq() => Success(p.reduceSeqUnroll !: e.t)
  })

  object ocl {
    import rise.core.types.AddressSpace
    import rise.openCL.primitives._

    // TODO shall we allow lowering from an already lowered reduceSeq?
    def reduceSeqUnroll(a: AddressSpace): Strategy[Rise] = rule("reduceSeqUnroll", {
      case e@reduce() => Success(oclReduceSeqUnroll(a) !: e.t)
      case e@p.reduceSeq() => Success(oclReduceSeqUnroll(a) !: e.t)
    })

    def circularBuffer(a: AddressSpace): Strategy[Rise] = rule("circularBuffer", {
      case e@DepApp(NatKind, DepApp(NatKind, slide(), n: Nat), Cst(1)) =>
        Success(
          oclCircularBuffer(a)(n)(n)(fun(x => x))
            !: e.t)
    })

    def circularBufferLoadFusion: Strategy[Rise] = rule("circularBufferLoadFusion", {
      case e@App(App(
        cb @ DepApp(NatKind, DepApp(NatKind, DepApp(AddressSpaceKind, oclCircularBuffer(), _), _), _),
        load), App(App(map(), f), in)
      ) =>
        Success(eraseType(cb)(preserveType(f) >> load, in) !: e.t)
    })

    def rotateValues(a: AddressSpace, write: Expr): Strategy[Rise] = rule("rotateValues", {
      case e@DepApp(NatKind, DepApp(NatKind, slide(), n: Nat), Cst(1)) =>
        Success(
          oclRotateValues(a)(n)(eraseType(write))
            !: e.t)
    })
  }
}
