package lift.core

import lift.core.types._
import lift.core.types.infer._
import lift.core.semantics._
import lift.core.primitives._
import lift.core.traversal.{Continue, Result, Stop}
import lift.core.TypeLevelDSL._

import scala.collection.mutable

import scala.language.implicitConversions

object TypedDSL {

  final case class Opaque(e: Expr) extends Primitive {
    import TypedDSL.Opaque._
    override val t: Type = freeze(e.t)
    override def typeScheme: Type = e.t
    override def setType(t: Type): Opaque = this
    override def name: String = s"Opaque Expr: $t"
  }

  object Opaque {
    def freeze(t: Type): Type = traversal.types.DepthFirstLocalResult(t, new traversal.Visitor {
      override def visitNat(ae: Nat): Result[Nat] = ae match {
        case i: NatIdentifier => Stop(i.asExplicit)
        case _ => Continue(ae, this)
      }
      override def visitAddressSpace(a: AddressSpace): Result[AddressSpace] = a match {
        case i: AddressSpaceIdentifier => Stop(i.asExplicit)
        case _ => Continue(a, this)
      }
      override def visitN2N(n2n: NatToNat): Result[NatToNat] = n2n match {
        case i: NatToNatIdentifier => Stop(i.asExplicit)
        case _ => Continue(n2n, this)
      }
      override def visitN2D(n2d: NatToData): Result[NatToData] = n2d match {
        case i: NatToDataIdentifier => Stop(i.asExplicit)
        case _ => Continue(n2d, this)
      }
      override def visitType[T <: Type](t: T): Result[T] = t match {
        case i: DataTypeIdentifier => Stop(i.asExplicit.asInstanceOf[T])
        case _: TypeIdentifier => throw TypeException("Cannot make an Expr with TypeIdentifier opaque")
        case _ => Continue(t, this)
      }
    })
  }

  final case class TopLevel(e: Expr, inst: Solution = Solution())(override val t: Type = TypePlaceholder) extends Primitive {
    import TypedDSL.TopLevel._
    override def typeScheme: Type = e.t
    override def setType(t: Type): TopLevel = {
      val subs = instantiate(t)
      this.copy(inst = subs)(subs(t))
    }
    override def name: String = s"TopLevel Expr: $t"
  }

  object TopLevel {
    def instantiate(t: Type): Solution = {
      import scala.collection.immutable.Map
      val emptySubs: (Map[Type, Type],
          Map[NatIdentifier, Nat],
          Map[AddressSpaceIdentifier, AddressSpace],
          Map[NatToDataIdentifier, NatToData]) = (Map(), Map(), Map(), Map())
      val ftvs = getFTVs(t)
      val subs = ftvs.foldLeft(emptySubs)((subs, ftv) => subs match {
        case (ts, ns, as, n2ds) => ftv match {
          case i: TypeIdentifier => (ts ++ Map(i -> implT(identity)), ns, as, n2ds)
          case i: NatIdentifier => (ts, ns ++ Map(i -> implN(identity)), as, n2ds)
          case i: AddressSpaceIdentifier => (ts, ns, as ++ Map(i -> implA(identity)), n2ds)
          case i: NatToDataIdentifier => (ts, ns, as, n2ds ++ Map(i -> implN2DT(identity)))
          case _ => throw TypeException("Not supported yet")
        }
      })
      new Solution(subs._1, subs._2, subs._3, subs._4)
    }

    def getFTVs(t: Type): Seq[Kind.Identifier] = {
      val ftvs = mutable.Set[Kind.Identifier]()
      traversal.types.DepthFirstLocalResult(t, new traversal.Visitor {
        override def visitType[T <: Type](t: T): Result[T] = {
          t match {
            case i: Kind.Identifier with Kind.Explicitness if ! i.isExplicit => ftvs += i
            case i: TypeIdentifier => ftvs += i
            case _ =>
          }
          Continue(t, this)
        }
      })
      ftvs.toSeq
    }

    case class Visitor(ftvSubs: Solution, sol: Solution) extends traversal.Visitor {
      override def visitExpr(e: Expr): Result[Expr] = Continue(e, this)
      override def visitNat(ae: Nat): Result[Nat] = Stop(cascadedApply(ftvSubs, sol, ae))
      override def visitType[T <: Type](t: T): Result[T] = Stop(cascadedApply(ftvSubs, sol, t).asInstanceOf[T])
      override def visitAddressSpace(a: AddressSpace): Result[AddressSpace] = Stop(cascadedApply(ftvSubs, sol, a))
      override def visitN2D(n2d: NatToData): Result[NatToData] = Stop(cascadedApply(ftvSubs, sol, n2d))
    }

    private def cascadedApply(ftvSubs: Solution, sol: Solution, t: Type): Type = {
      traversal.types.DepthFirstLocalResult(t, new Visitor(ftvSubs, sol) {
        override def visitType[T <: Type](t: T): Result[T] = t match {
          case i: TypeIdentifier => ftvSubs.ts.get(i) match {
            case Some(i) => sol.ts.get(i.asInstanceOf[TypeIdentifier]) match {
              case Some(x) => Stop(x.asInstanceOf[T])
              case None => Continue(t, this)
            }
            case None => Continue(t, this)
          }
          case _ => Continue(t, this)
        }
      })
    }

    private def cascadedApply(ftvSubs: Solution, sol: Solution, n: Nat): Nat = {
      n.visitAndRebuild {
        case i: NatIdentifier => ftvSubs.ns.get(i) match {
          case Some(n) => sol.ns.get(n.asInstanceOf[NatIdentifier]) match {
            case Some(x) => x
            case None => n
          }
          case None => i
        }
        case n => n
      }
    }

    private def cascadedApply(ftvSubs: Solution, sol: Solution, a: AddressSpace): AddressSpace = {
      a match {
        case i: AddressSpaceIdentifier => ftvSubs.as.get(i) match {
          case Some(a) => sol.as.get(a.asInstanceOf[AddressSpaceIdentifier]) match {
            case Some(x) => x
            case None => a
          }
          case None => i
        }
        case a => a
      }
    }

    private def cascadedApply(ftvSubs: Solution, sol: Solution, n2d: NatToData): NatToData = {
      n2d match {
        case i: NatToDataIdentifier => ftvSubs.n2ds.get(i) match {
          case Some(n2d) => sol.n2ds.get(n2d.asInstanceOf[NatToDataIdentifier]) match {
            case Some(x) => x
            case None => n2d
          }
          case None => i
        }
        case n2d => n2d
      }
    }
  }

  final case class TDSL[+T <: Expr](private val e: T) {
    def toExpr: Expr = TDSL.infer(e)
    def >>=[X <: Expr](f: T => TDSL[X]): TDSL[X] = f(e)
  }

  implicit def typed[T <: Expr](e: T): TDSL[Opaque] = TDSL(Opaque(e))

  def toTDSL[T <: Expr](e: T): TDSL[T] = TDSL(e)

  implicit def toExpr(d: TDSL[Expr]): Expr = d.toExpr

  def topLevel(e: Expr): Expr = TopLevel(e)()

  def erase(e: Expr): Expr = traversal.DepthFirstLocalResult(e, new traversal.Visitor {
    override def visitExpr(e: Expr): Result[Expr] = e match {
      case _ => Continue(e.setType(TypePlaceholder), this)
    }
  })

  object TDSL {
    case class Visitor(sol: Solution) extends traversal.Visitor {
      override def visitExpr(e: Expr): Result[Expr] = e match {
        case Opaque(x) => Stop(x)
        case TopLevel(x, inst) => Stop(traversal.DepthFirstLocalResult(x, TopLevel.Visitor(inst, sol)))
        case _ => Continue(e, this)
      }
      override def visitNat(ae: Nat): Result[Nat] = Stop(sol(ae))
      override def visitType[T <: Type](t: T): Result[T] = Stop(sol(t).asInstanceOf[T])
      override def visitAddressSpace(a: AddressSpace): Result[AddressSpace] = Stop(sol(a))
      override def visitN2D(n2d: NatToData): Result[NatToData] = Stop(sol(n2d))
    }

    def infer(e: Expr): Expr = {
      val constraints = mutable.ArrayBuffer[Constraint]()
      val typed_e = constrainTypes(e, constraints, mutable.Map())
      val solution = solve(constraints)
      val r = traversal.DepthFirstLocalResult(typed_e, Visitor(solution))
      r
    }
  }

  def identifier(name: String): TDSL[Identifier] = toTDSL(Identifier(name)())
  def lambda(x: TDSL[Identifier], e: TDSL[Expr]): TDSL[Lambda] =
    x >>= (x => e >>= (e => toTDSL(Lambda(x, e)())))
  def app(f: TDSL[Expr], e: TDSL[Expr]): TDSL[App] = f >>= (f => e >>= (e => toTDSL(App(f, e)())))
  def depLambda[K <: Kind : KindName](x: K#I with Kind.Explicitness, e: TDSL[Expr]): TDSL[DepLambda[K]] =
    e >>= (e => toTDSL(DepLambda[K](x, e)()))
  def depApp[K <: Kind](f: TDSL[Expr], x: K#T): TDSL[DepApp[K]] = f >>= (f => toTDSL(DepApp[K](f, x)()))
  def literal(d: semantics.Data): TDSL[Literal] = toTDSL(Literal(d))

  def array(n: Int): TDSL[MakeArray] = toTDSL(primitives.MakeArray(n)())
  def cast: TDSL[Cast] = toTDSL(primitives.Cast()())
  def depJoin: TDSL[DepJoin] = toTDSL(primitives.DepJoin()())
  def depMapSeq: TDSL[DepMapSeq] = toTDSL(primitives.DepMapSeq()())
  def depZip: TDSL[DepZip] = toTDSL(primitives.DepZip()())
  def drop: TDSL[Drop] = toTDSL(primitives.Drop()())
  def fst: TDSL[Fst] = toTDSL(primitives.Fst()())
  def gather: TDSL[Gather] = toTDSL(primitives.Gather()())
  def generate: TDSL[Generate] = toTDSL(primitives.Generate()())
  def idx: TDSL[Idx] = toTDSL(primitives.Idx()())
  def id: TDSL[Id] = toTDSL(primitives.Id()())
  def indexAsNat: TDSL[IndexAsNat] = toTDSL(primitives.IndexAsNat()())
  def iterate: TDSL[Iterate] = toTDSL(primitives.Iterate()())
  def join: TDSL[Join] = toTDSL(primitives.Join()())
  def let: TDSL[Let] = toTDSL(primitives.Let()())
  def map: TDSL[Map] = toTDSL(primitives.Map()())
  def mapFst: TDSL[MapFst] = toTDSL(primitives.MapFst()())
  def mapSnd: TDSL[MapSnd] = toTDSL(primitives.MapSnd()())
  def mapSeq: TDSL[MapSeq] = toTDSL(primitives.MapSeq()())
  def mapSeqUnroll: TDSL[MapSeqUnroll] = toTDSL(primitives.MapSeqUnroll()())
  def natAsIndex: TDSL[NatAsIndex] = toTDSL(primitives.NatAsIndex()())
  def padCst: TDSL[PadCst] = toTDSL(primitives.PadCst()())
  def padClamp: TDSL[PadClamp] = toTDSL(primitives.PadClamp()())
  def partition: TDSL[Partition] = toTDSL(primitives.Partition()())
  def pair: TDSL[Pair] = toTDSL(primitives.Pair()())
  def reduce: TDSL[Reduce] = toTDSL(primitives.Reduce()())
  def reduceSeq: TDSL[ReduceSeq] = toTDSL(primitives.ReduceSeq()())
  def reduceSeqUnroll: TDSL[ReduceSeqUnroll] = toTDSL(primitives.ReduceSeqUnroll()())
  def reorder: TDSL[Reorder] = toTDSL(primitives.Reorder()())
  def scanSeq: TDSL[ScanSeq] = toTDSL(primitives.ScanSeq()())
  def slide: TDSL[Slide] = toTDSL(primitives.Slide()())
  def slideSeq(roprimT: SlideSeq.Rotate): TDSL[SlideSeq] = toTDSL(primitives.SlideSeq(roprimT)())
  def snd: TDSL[Snd] = toTDSL(primitives.Snd()())
  def split: TDSL[Split] = toTDSL(primitives.Split()())
  def take: TDSL[Take] = toTDSL(primitives.Take()())
  def transpose: TDSL[Transpose] = toTDSL(primitives.Transpose()())
  def select: TDSL[Select] = toTDSL(primitives.Select()())
  def unzip: TDSL[Unzip] = toTDSL(primitives.Unzip()())
  def zip: TDSL[Zip] = toTDSL(primitives.Zip()())

  def neg: TDSL[Neg] = toTDSL(primitives.Neg()())
  def add: TDSL[Add] = toTDSL(primitives.Add()())
  def sub: TDSL[Sub] = toTDSL(primitives.Sub()())
  def mul: TDSL[Mul] = toTDSL(primitives.Mul()())
  def div: TDSL[Div] = toTDSL(primitives.Div()())
  def mod: TDSL[Mod] = toTDSL(primitives.Mod()())
  def gt: TDSL[Gt] = toTDSL(primitives.Gt()())
  def lt: TDSL[Lt] = toTDSL(primitives.Lt()())
  def equal: TDSL[Equal] = toTDSL(primitives.Equal()())

  def asVector: TDSL[AsVector] = toTDSL(primitives.AsVector()())
  def asVectorAligned: TDSL[AsVectorAligned] = toTDSL(primitives.AsVectorAligned()())
  def asScalar: TDSL[AsScalar] = toTDSL(primitives.AsScalar()())
  def vectorFromScalar: TDSL[VectorFromScalar] = toTDSL(primitives.VectorFromScalar()())

  def printType(msg: String): TDSL[PrintType] = toTDSL(PrintType(msg)())
  def typeHole(msg: String): TDSL[TypeHole] = toTDSL(TypeHole(msg)())

  implicit class Ops(lhs: TDSL[Expr]) {

    // binary
    def +(rhs: TDSL[Expr]): TDSL[App] = add(lhs)(rhs)
    def -(rhs: TDSL[Expr]): TDSL[App] = sub(lhs)(rhs)
    def *(rhs: TDSL[Expr]): TDSL[App] = mul(lhs)(rhs)
    def /(rhs: TDSL[Expr]): TDSL[App] = div(lhs)(rhs)
    def %(rhs: TDSL[Expr]): TDSL[App] = mod(lhs)(rhs)
    def >(rhs: TDSL[Expr]): TDSL[App] = gt(lhs)(rhs)
    def <(rhs: TDSL[Expr]): TDSL[App] = lt(lhs)(rhs)
    def =:=(rhs: TDSL[Expr]): TDSL[App] = equal(lhs)(rhs)

    // unary
    def unary_- : TDSL[App] = neg(lhs)

    // pair accesses
    def _1: TDSL[App] = fst(lhs)
    def _2: TDSL[App] = snd(lhs)
  }

  implicit class Indexing(e: TDSL[Expr]) {
    def `@`(i: TDSL[Expr]): TDSL[App] = idx(i)(e)
  }

  /*
  implicit class TypeAnnotation(t: Type) {
    def ::[T <: Expr](e: TDSL[T]): TDSL[T] = e >>= (e =>
      if (e.t == TypePlaceholder) tdsl(e.setType(t).asInstanceOf[T])
      else if (e.t == t) tdsl(e) else
        throw TypeException(s"tried to replace ${e.t} with ${t}, but type annotation can only replace a TypePlaceholder"))
    def `:`[T <: Expr](e: TDSL[T]): TDSL[T] = e :: t
  }
   */
  implicit class TypeAnnotation(t: Type) {
    def ::[T <: Expr](e: TDSL[T]): TDSL[Expr] = e >>= (e => toTDSL(Annotation(e, t)))
    def `:`[T <: Expr](e: TDSL[T]): TDSL[Expr] = e :: t
  }

  implicit class FunCall(f: TDSL[Expr]) {

    def apply(e: TDSL[Expr]): TDSL[App] = app(f, e)
    def apply(n: Nat): TDSL[DepApp[NatKind]] = depApp[NatKind](f, n)
    def apply(dt: DataType): TDSL[DepApp[DataKind]] = depApp[DataKind](f, dt)
    def apply(a: AddressSpace): TDSL[DepApp[AddressSpaceKind]] = depApp[AddressSpaceKind](f, a)

    def apply(n2n: NatToNat): TDSL[DepApp[NatToNatKind]] = depApp[NatToNatKind](f, n2n)

    def apply(e1: TDSL[Expr], e2: TDSL[Expr]): TDSL[App] = {
      f(e1)(e2)
    }

    def apply(e1: TDSL[Expr], e2: TDSL[Expr], e3: TDSL[Expr]): TDSL[App] = {
      f(e1)(e2)(e3)
    }

    def apply(e1: TDSL[Expr], e2: TDSL[Expr], e3: TDSL[Expr], e4: TDSL[Expr]): TDSL[App] = {
      f(e1)(e2)(e3)(e4)
    }

    def apply(e1: TDSL[Expr], e2: TDSL[Expr], e3: TDSL[Expr], e4: TDSL[Expr], e5: TDSL[Expr]): TDSL[App] = {
      f(e1)(e2)(e3)(e4)(e5)
    }
  }

  implicit class FunPipe(e: TDSL[Expr]) {
    def |>(f: TDSL[Expr]): TDSL[App] = f.apply(e)
  }

  implicit class FunPipeReverse(f: TDSL[Expr]) {
    def $(e: TDSL[Expr]): TDSL[App] = f.apply(e)
  }

  implicit class FunComp(f: TDSL[Expr]) {
    def >>(g: TDSL[Expr]): TDSL[Lambda] = fun(x => g(f(x)))
  }

  implicit class FunCompReverse(f: TDSL[Expr]) {
    def o(g: TDSL[Expr]): TDSL[Lambda] = fun(x => f(g(x)))
  }

  // function values
  object fun {
    def apply(t: Type)(f: TDSL[Identifier] => TDSL[Expr]): TDSL[Lambda] = {
      val x = identifier(freshName("e")) >>= (i => toTDSL(i.setType(t)))
      lambda(x, f(x))
    }

    def apply(f: TDSL[Identifier] => TDSL[Expr]): TDSL[Lambda] = untyped(f)
    def apply(f: (TDSL[Identifier], TDSL[Identifier]) => TDSL[Expr]): TDSL[Lambda] = untyped(f)
    def apply(f: (TDSL[Identifier], TDSL[Identifier], TDSL[Identifier]) => TDSL[Expr]): TDSL[Lambda] = untyped(f)
    def apply(f: (TDSL[Identifier], TDSL[Identifier], TDSL[Identifier], TDSL[Identifier]) => TDSL[Expr]): TDSL[Lambda] = untyped(f)
    def apply(f: (TDSL[Identifier], TDSL[Identifier], TDSL[Identifier], TDSL[Identifier], TDSL[Identifier]) => TDSL[Expr]): TDSL[Lambda] = untyped(f)
    def apply(f: (TDSL[Identifier], TDSL[Identifier], TDSL[Identifier], TDSL[Identifier], TDSL[Identifier], TDSL[Identifier]) => TDSL[Expr]): TDSL[Lambda] = untyped(f)

    private def untyped(f: TDSL[Identifier] => TDSL[Expr]): TDSL[Lambda] = {
      val e = identifier(freshName("e"))
      lambda(e, f(e))
    }

    private def untyped(f: (TDSL[Identifier], TDSL[Identifier]) => TDSL[Expr]): TDSL[Lambda] = {
      val e = identifier(freshName("e"))
      lambda(e, untyped(e1 => f(e, e1)))
    }

    private def untyped(f: (TDSL[Identifier], TDSL[Identifier], TDSL[Identifier]) => TDSL[Expr]): TDSL[Lambda] = {
      val e = identifier(freshName("e"))
      lambda(e, untyped((e1, e2) => f(e, e1, e2)))
    }

    private def untyped(f: (TDSL[Identifier], TDSL[Identifier], TDSL[Identifier], TDSL[Identifier]) => TDSL[Expr]): TDSL[Lambda] = {
      val e = identifier(freshName("e"))
      lambda(e, untyped((e1, e2, e3) => f(e, e1, e2, e3)))
    }

    private def untyped(f: (TDSL[Identifier], TDSL[Identifier], TDSL[Identifier], TDSL[Identifier], TDSL[Identifier]) => TDSL[Expr]): TDSL[Lambda] = {
      val e = identifier(freshName("e"))
      lambda(e, untyped((e1, e2, e3, e4) => f(e, e1, e2, e3, e4)))
    }

    private def untyped(f: (TDSL[Identifier], TDSL[Identifier], TDSL[Identifier], TDSL[Identifier], TDSL[Identifier], TDSL[Identifier]) => TDSL[Expr]): TDSL[Lambda] = {
      val e = identifier(freshName("e"))
      lambda(e, untyped((e1, e2, e3, e4, e5) => f(e, e1, e2, e3, e4, e5)))
    }

    //noinspection TypeAnnotation
    def apply(ft: FunType[Type, Type]) = new {
      def apply(f: TDSL[Identifier] => TDSL[Expr]): TDSL[Expr] = untyped(f) :: ft
      def apply(f: (TDSL[Identifier], TDSL[Identifier]) => TDSL[Expr]): TDSL[Expr] = untyped(f) :: ft
      def apply(f: (TDSL[Identifier], TDSL[Identifier], TDSL[Identifier]) => TDSL[Expr]): TDSL[Expr] = untyped(f) :: ft
      def apply(f: (TDSL[Identifier], TDSL[Identifier], TDSL[Identifier], TDSL[Identifier]) => TDSL[Expr]): TDSL[Expr] = untyped(f) :: ft
      def apply(f: (TDSL[Identifier], TDSL[Identifier], TDSL[Identifier], TDSL[Identifier], TDSL[Identifier]) => TDSL[Expr]): TDSL[Expr] = untyped(f) :: ft
      def apply(f: (TDSL[Identifier], TDSL[Identifier], TDSL[Identifier], TDSL[Identifier], TDSL[Identifier], TDSL[Identifier]) => TDSL[Expr]): TDSL[Expr] = untyped(f) :: ft
    }
  }

  object nFun {
    def apply(r: lift.arithmetic.Range, f: NatIdentifier => TDSL[Expr]): TDSL[DepLambda[NatKind]] = {
      val x = NatIdentifier(freshName("n"), r, isExplicit = true)
      depLambda[NatKind](x, f(x))
    }

    def apply(f: NatIdentifier => TDSL[Expr]): TDSL[DepLambda[NatKind]] = {
      nFun(lift.arithmetic.RangeAdd(0, lift.arithmetic.PosInf, 1), f)
    }

    def apply(f: (NatIdentifier, NatIdentifier) => TDSL[Expr]): TDSL[DepLambda[NatKind]] = {
      val r = lift.arithmetic.RangeAdd(0, lift.arithmetic.PosInf, 1)
      val n = NatIdentifier(freshName("n"), r, isExplicit = true)
      depLambda[NatKind](n, nFun(f(n, _)))
    }

    def apply(f: (NatIdentifier, NatIdentifier, NatIdentifier) => TDSL[Expr]): TDSL[DepLambda[NatKind]] = {
      val r = lift.arithmetic.RangeAdd(0, lift.arithmetic.PosInf, 1)
      val n = NatIdentifier(freshName("n"), r, isExplicit = true)
      depLambda[NatKind](n, nFun((n1, n2) => f(n, n1, n2)))
    }

    def apply(f: (NatIdentifier, NatIdentifier, NatIdentifier, NatIdentifier) => TDSL[Expr]): TDSL[DepLambda[NatKind]] = {
      val r = lift.arithmetic.RangeAdd(0, lift.arithmetic.PosInf, 1)
      val n = NatIdentifier(freshName("n"), r, isExplicit = true)
      depLambda[NatKind](n, nFun((n1, n2, n3) => f(n, n1, n2, n3)))
    }

    def apply(f: (NatIdentifier, NatIdentifier, NatIdentifier, NatIdentifier, NatIdentifier) => TDSL[Expr]): TDSL[DepLambda[NatKind]] = {
      val r = lift.arithmetic.RangeAdd(0, lift.arithmetic.PosInf, 1)
      val n = NatIdentifier(freshName("n"), r, isExplicit = true)
      depLambda[NatKind](n, nFun((n1, n2, n3, n4) => f(n, n1, n2, n3, n4)))
    }
  }

  object dtFun {
    def apply(f: DataTypeIdentifier => TDSL[Expr]): TDSL[DepLambda[DataKind]] = {
      val x = DataTypeIdentifier(freshName("dt"), isExplicit = true)
      depLambda[DataKind](x, f(x))
    }
  }

  implicit def wrapInNatExpr(n: Nat): TDSL[Literal] = literal(NatData(n))

  def l(i: Int): TDSL[Literal] = literal(IntData(i))
  def l(f: Float): TDSL[Literal] = literal(FloatData(f))
  def l(d: Double): TDSL[Literal] = literal(DoubleData(d))
  def lidx(i: Nat, n: Nat): TDSL[Literal] = literal(IndexData(i, n))
  def lvec(v: Seq[ScalarData]): TDSL[Literal] = literal(VectorData(v))
  def larr(a: Seq[Data]): TDSL[Literal] = literal(ArrayData(a))

  object foreignFun {
    def apply(name: String, t: Type): TDSL[ForeignFunction] = {
      toTDSL(ForeignFunction(ForeignFunction.Decl(name, None))(t))
    }

    def apply(name: String, params: Seq[String], body: String, t: Type): TDSL[ForeignFunction] = {
      toTDSL(ForeignFunction(ForeignFunction.Decl(name,
        Some(ForeignFunction.Def(params, body))))(t))
    }
  }
}
