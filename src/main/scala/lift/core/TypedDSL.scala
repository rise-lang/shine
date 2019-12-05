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
    override val t: Type = e.t
    override def typeScheme: Type = e.t
    override def setType(t: Type): Opaque = this
    override def toString: String = "opaque"
  }

  final case class TDSL[+T <: Expr](private val e: T) {
    def matches(t: Type): Expr = TDSL.infer(e, t)
    def >>=[X <: Expr](f: T => TDSL[X]): TDSL[X] = f(e)
  }

  implicit def typed[T <: Expr](e: T): TDSL[Opaque] = TDSL(Opaque(e))

  def tdsl[T <: Expr](e: T): TDSL[T] = TDSL(e)

  implicit def trivial(d: TDSL[Expr]): Expr = d.matches(freshTypeIdentifier)

  object TDSL {
    case class Visitor(sol: Solution) extends traversal.Visitor {
      override def visitExpr(e: Expr): Result[Expr] = e match {
          case Opaque(x) => Stop(x)
          case _ => Continue(e, this)
        }
      override def visitNat(ae: Nat): Result[Nat] = Stop(sol(ae))
      override def visitType[T <: Type](t: T): Result[T] = Stop(sol(t).asInstanceOf[T])
      override def visitAddressSpace(a: AddressSpace): Result[AddressSpace] = Stop(sol(a))
      override def visitN2D(n2d: NatToData): Result[NatToData] = Stop(sol(n2d))
    }

    def infer(e: Expr, expected: Type): Expr = {
      val constraints = mutable.ArrayBuffer[Constraint]()
      val typed_e = constrainTypes(e, constraints, mutable.Map())
      constraints += TypeConstraint(typed_e.t, expected)
      val solution = solve(constraints)
      val r = traversal.DepthFirstLocalResult(typed_e, Visitor(solution))
      r
    }
  }

  def identifier(name: String): TDSL[Identifier] = tdsl(Identifier(name)())
  def lambda(x: TDSL[Identifier], e: TDSL[Expr]): TDSL[Lambda] =
    x >>= (x => e >>= (e => tdsl(Lambda(x, e)())))
  def app(f: TDSL[Expr], e: TDSL[Expr]): TDSL[App] = f >>= (f => e >>= (e => tdsl(App(f, e)())))
  def depLambda[K <: Kind : KindName](x: K#I with Kind.Explicitness, e: TDSL[Expr]): TDSL[DepLambda[K]] =
    e >>= (e => tdsl(DepLambda[K](x, e)()))
  def depApp[K <: Kind](f: TDSL[Expr], x: K#T): TDSL[DepApp[K]] = f >>= (f => tdsl(DepApp[K](f, x)()))
  def literal(d: semantics.Data): TDSL[Literal] = tdsl(Literal(d))

  def array(n: Int): TDSL[MakeArray] = tdsl(primitives.MakeArray(n)())
  def cast: TDSL[Cast] = tdsl(primitives.Cast()())
  def depJoin: TDSL[DepJoin] = tdsl(primitives.DepJoin()())
  def depMapSeq: TDSL[DepMapSeq] = tdsl(primitives.DepMapSeq()())
  def depZip: TDSL[DepZip] = tdsl(primitives.DepZip()())
  def drop: TDSL[Drop] = tdsl(primitives.Drop()())
  def fst: TDSL[Fst] = tdsl(primitives.Fst()())
  def gather: TDSL[Gather] = tdsl(primitives.Gather()())
  def generate: TDSL[Generate] = tdsl(primitives.Generate()())
  def idx: TDSL[Idx] = tdsl(primitives.Idx()())
  def id: TDSL[Id] = tdsl(primitives.Id()())
  def indexAsNat: TDSL[IndexAsNat] = tdsl(primitives.IndexAsNat()())
  def iterate: TDSL[Iterate] = tdsl(primitives.Iterate()())
  def join: TDSL[Join] = tdsl(primitives.Join()())
  def let: TDSL[Let] = tdsl(primitives.Let()())
  def map: TDSL[Map] = tdsl(primitives.Map()())
  def mapFst: TDSL[MapFst] = tdsl(primitives.MapFst()())
  def mapSnd: TDSL[MapSnd] = tdsl(primitives.MapSnd()())
  def mapSeq: TDSL[MapSeq] = tdsl(primitives.MapSeq()())
  def mapSeqUnroll: TDSL[MapSeqUnroll] = tdsl(primitives.MapSeqUnroll()())
  def natAsIndex: TDSL[NatAsIndex] = tdsl(primitives.NatAsIndex()())
  def padCst: TDSL[PadCst] = tdsl(primitives.PadCst()())
  def padClamp: TDSL[PadClamp] = tdsl(primitives.PadClamp()())
  def partition: TDSL[Partition] = tdsl(primitives.Partition()())
  def pair: TDSL[Pair] = tdsl(primitives.Pair()())
  def reduce: TDSL[Reduce] = tdsl(primitives.Reduce()())
  def reduceSeq: TDSL[ReduceSeq] = tdsl(primitives.ReduceSeq()())
  def reduceSeqUnroll: TDSL[ReduceSeqUnroll] = tdsl(primitives.ReduceSeqUnroll()())
  def reorder: TDSL[Reorder] = tdsl(primitives.Reorder()())
  def scanSeq: TDSL[ScanSeq] = tdsl(primitives.ScanSeq()())
  def slide: TDSL[Slide] = tdsl(primitives.Slide()())
  def slideSeq(roprimT: SlideSeq.Rotate): TDSL[SlideSeq] = tdsl(primitives.SlideSeq(roprimT)())
  def snd: TDSL[Snd] = tdsl(primitives.Snd()())
  def split: TDSL[Split] = tdsl(primitives.Split()())
  def take: TDSL[Take] = tdsl(primitives.Take()())
  def transpose: TDSL[Transpose] = tdsl(primitives.Transpose()())
  def select: TDSL[Select] = tdsl(primitives.Select()())
  def unzip: TDSL[Unzip] = tdsl(primitives.Unzip()())
  def zip: TDSL[Zip] = tdsl(primitives.Zip()())

  def neg: TDSL[Neg] = tdsl(primitives.Neg()())
  def add: TDSL[Add] = tdsl(primitives.Add()())
  def sub: TDSL[Sub] = tdsl(primitives.Sub()())
  def mul: TDSL[Mul] = tdsl(primitives.Mul()())
  def div: TDSL[Div] = tdsl(primitives.Div()())
  def mod: TDSL[Mod] = tdsl(primitives.Mod()())
  def gt: TDSL[Gt] = tdsl(primitives.Gt()())
  def lt: TDSL[Lt] = tdsl(primitives.Lt()())
  def equal: TDSL[Equal] = tdsl(primitives.Equal()())

  def asVector: TDSL[AsVector] = tdsl(primitives.AsVector()())
  def asVectorAligned: TDSL[AsVectorAligned] = tdsl(primitives.AsVectorAligned()())
  def asScalar: TDSL[AsScalar] = tdsl(primitives.AsScalar()())
  def vectorFromScalar: TDSL[VectorFromScalar] = tdsl(primitives.VectorFromScalar()())

  def printType(msg: String): TDSL[PrintType] = tdsl(PrintType(msg)())
  def typeHole(msg: String): TDSL[TypeHole] = tdsl(TypeHole(msg)())

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

  implicit class TypeAnnotation(t: Type) {
    def ::[T <: Expr](e: TDSL[T]): TDSL[T] = e >>= (e =>
      if (e.t == TypePlaceholder) tdsl(e.setType(t).asInstanceOf[T])
      else if (e.t == t) tdsl(e) else
        throw TypeException(s"tried to replace ${e.t} with ${t}, but type annotation can only replace a TypePlaceholder"))
    def `:`[T <: Expr](e: TDSL[T]): TDSL[T] = e :: t
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
      val x = identifier(freshName("e")) >>= (i => tdsl(i.setType(t)))
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
      def apply(f: TDSL[Identifier] => TDSL[Expr]): TDSL[Lambda] = untyped(f) :: ft
      def apply(f: (TDSL[Identifier], TDSL[Identifier]) => TDSL[Expr]): TDSL[Lambda] = untyped(f) :: ft
      def apply(f: (TDSL[Identifier], TDSL[Identifier], TDSL[Identifier]) => TDSL[Expr]): TDSL[Lambda] = untyped(f) :: ft
      def apply(f: (TDSL[Identifier], TDSL[Identifier], TDSL[Identifier], TDSL[Identifier]) => TDSL[Expr]): TDSL[Lambda] = untyped(f) :: ft
      def apply(f: (TDSL[Identifier], TDSL[Identifier], TDSL[Identifier], TDSL[Identifier], TDSL[Identifier]) => TDSL[Expr]): TDSL[Lambda] = untyped(f) :: ft
      def apply(f: (TDSL[Identifier], TDSL[Identifier], TDSL[Identifier], TDSL[Identifier], TDSL[Identifier], TDSL[Identifier]) => TDSL[Expr]): TDSL[Lambda] = untyped(f) :: ft
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
      tdsl(ForeignFunction(ForeignFunction.Decl(name, None))(t))
    }

    def apply(name: String, params: Seq[String], body: String, t: Type): TDSL[ForeignFunction] = {
      tdsl(ForeignFunction(ForeignFunction.Decl(name,
        Some(ForeignFunction.Def(params, body))))(t))
    }
  }
  /*
  final case class Chain(a: InferenceResult, f: Expr => InferenceResult) extends InferenceResult

  object InferenceResult {
    def genType(e: Expr): Type = if (e.t == TypePlaceholder) freshTypeIdentifier else e.t
    def genConstraints(x: InferenceResult, constraints: mutable.ArrayBuffer[Constraint]): InferenceResult = {
      def constrain(x: InferenceResult): InferenceResult = genConstraints(x, constraints)
      x match {
        case _: Success | _: Failure => x
        case Chain(a, f) =>
          val ca = constrain(a)
          ca match {
            case _: Failure => ca
            case Success(e, _) =>
              val ea = e match {
                case i @ (_: Identifier) if i.isBinder => i.setType(genType(i))
                case _ => e
              }

          }
      }
    }
  }
  */
}
