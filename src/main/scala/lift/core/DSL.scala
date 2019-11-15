package lift.core

import lift.arithmetic.{Cst, RangeAdd}
import lift.core.types._
import lift.core.semantics._
import lift.core.primitives._

import scala.language.implicitConversions

object DSL {

  def identifier(name: String): Identifier = Identifier(name)()
  def lambda(x: Identifier, e: Expr): Lambda = Lambda(x, e)()
  def app(f: Expr, e: Expr): App = App(f, e)()
  def depLambda[K <: Kind](x: K#I, e: Expr)(implicit kn: KindName[K]): DepLambda[K] = DepLambda[K](x, e)()(kn)
  def depApp[K <: Kind](f: Expr, x: K#T): DepApp[K] = DepApp[K](f, x)()
  def literal(d: semantics.Data): Literal = Literal(d)

  def array(n: Int): ArrayCons = primitives.ArrayCons(n)()
  def cast: Cast = primitives.Cast()()
  def depJoin: DepJoin = primitives.DepJoin()()
  def depMapSeq: DepMapSeq = primitives.DepMapSeq()()
  def depZip: DepZip = primitives.DepZip()()
  def drop: Drop = primitives.Drop()()
  def fst: Fst = primitives.Fst()()
  def gather: Gather = primitives.Gather()()
  def generate: Generate = primitives.Generate()()
  def idx: Idx = primitives.Idx()()
  def id: Id = primitives.Id()()
  def indexAsNat: IndexAsNat = primitives.IndexAsNat()()
  def iterate: Iterate = primitives.Iterate()()
  def join: Join = primitives.Join()()
  def let: Let = primitives.Let()()
  def map: Map = primitives.Map()()
  def mapFst: MapFst = primitives.MapFst()()
  def mapSnd: MapSnd = primitives.MapSnd()()
  def mapSeq: MapSeq = primitives.MapSeq()()
  def mapSeqUnroll: MapSeqUnroll = primitives.MapSeqUnroll()()
  def natAsIndex: NatAsIndex = primitives.NatAsIndex()()
  def padCst: PadCst = primitives.PadCst()()
  def padClamp: PadClamp = primitives.PadClamp()()
  def partition: Partition = primitives.Partition()()
  def pair: Pair = primitives.Pair()()
  def reduce: Reduce = primitives.Reduce()()
  def reduceSeq: ReduceSeq = primitives.ReduceSeq()()
  def reduceSeqUnroll: ReduceSeqUnroll = primitives.ReduceSeqUnroll()()
  def reorder: Reorder = primitives.Reorder()()
  def scanSeq: ScanSeq = primitives.ScanSeq()()
  def slide: Slide = primitives.Slide()()
  def slideSeq(roprimT: SlideSeq.Rotate): SlideSeq = primitives.SlideSeq(roprimT)()
  def snd: Snd = primitives.Snd()()
  def split: Split = primitives.Split()()
  def take: Take = primitives.Take()()
  def transpose: Transpose = primitives.Transpose()()
  def select: Select = primitives.Select()()
  def unzip: Unzip = primitives.Unzip()()
  def zip: Zip = primitives.Zip()()

  def neg: Neg = primitives.Neg()()
  def add: Add = primitives.Add()()
  def sub: Sub = primitives.Sub()()
  def mul: Mul = primitives.Mul()()
  def div: Div = primitives.Div()()
  def mod: Mod = primitives.Mod()()
  def gt: Gt = primitives.Gt()()
  def lt: Lt = primitives.Lt()()
  def equal: Equal = primitives.Equal()()

  def asVector: AsVector = primitives.AsVector()()
  def asVectorAligned: AsVectorAligned = primitives.AsVectorAligned()()
  def asScalar: AsScalar = primitives.AsScalar()()
  def vectorFromScalar: VectorFromScalar = primitives.VectorFromScalar()()

  def printType(msg: String): PrintType = PrintType(msg)()
  def typeHole(msg: String): TypeHole = TypeHole(msg)()

  implicit class Ops(lhs: Expr) {
    import lift.core.primitives._

    // binary
    def +(rhs: Expr): Expr = add(lhs)(rhs)
    def -(rhs: Expr): Expr = sub(lhs)(rhs)
    def *(rhs: Expr): Expr = mul(lhs)(rhs)
    def /(rhs: Expr): Expr = div(lhs)(rhs)
    def %(rhs: Expr): Expr = mod(lhs)(rhs)
    def >(rhs: Expr): Expr = gt(lhs)(rhs)
    def <(rhs: Expr): Expr = lt(lhs)(rhs)
    def =:=(rhs: Expr): Expr = equal(lhs)(rhs)

    // unary
    def unary_- : Expr = neg(lhs)

    // pair accesses
    def _1: Expr = fst(lhs)
    def _2: Expr = snd(lhs)
  }

  implicit class Indexing(e: Expr) {
    import lift.core.primitives._

    def `@`(i: Expr): Expr = idx(i)(e)
  }

  implicit class TypeAnnotation(t: Type) {
    def ::(e: Expr): Expr =
      if (e.t == TypePlaceholder) e.setType(t)
      else if (e.t == t) e else
        throw TypeException(s"tried to replace ${e.t} with ${t}, but type annotation can only replace a TypePlaceholder")
    def `:`(e: Expr): Expr = e :: t
  }

  implicit class TypeEqual(a: Type) {
    def =~=(b: Type): Boolean = (a, b) match {
      case (TypePlaceholder, _) => true
      case (_, TypePlaceholder) => true
      case _ => a == b
    }
  }

  implicit class FunCall(f: Expr) {

    def apply(e: Expr): Expr = app(f, e)
    def apply(n: Nat): Expr = depApp[NatKind](f, n)
    def apply(dt: DataType): Expr = depApp[DataKind](f, dt)
    def apply(a: AddressSpace): Expr = depApp[AddressSpaceKind](f, a)

    def apply(n2n: NatToNat): Expr = depApp[NatToNatKind](f, n2n)

    def apply(e1: Expr, e2: Expr): Expr = {
      f(e1)(e2)
    }

    def apply(e1: Expr, e2: Expr, e3: Expr): Expr = {
      f(e1)(e2)(e3)
    }

    def apply(e1: Expr, e2: Expr, e3: Expr, e4: Expr): Expr = {
      f(e1)(e2)(e3)(e4)
    }

    def apply(e1: Expr, e2: Expr, e3: Expr, e4: Expr, e5: Expr): Expr = {
      f(e1)(e2)(e3)(e4)(e5)
    }
  }

  implicit class FunPipe(e: Expr) {
    def |>(f: Expr): Expr = f.apply(e)
  }

  implicit class FunPipeReverse(f: Expr) {
    def $(e: Expr): Expr = f.apply(e)
  }

  implicit class FunComp(f: Expr) {
    def >>(g: Expr): Expr = fun(x => g(f(x)))
  }

  implicit class FunCompReverse(f: Expr) {
    def o(g: Expr): Expr = fun(x => f(g(x)))
  }

  // function values
  object fun {
    def apply(t: Type)(f: Expr => Expr): Expr = {
      val x = identifier(freshName("e")).setType(t)
      lambda(x, f(x))
    }

    def apply(f: Identifier => Expr): Expr = untyped(f)
    def apply(f: (Identifier, Identifier) => Expr): Expr = untyped(f)
    def apply(f: (Identifier, Identifier, Identifier) => Expr): Expr = untyped(f)
    def apply(f: (Identifier, Identifier, Identifier, Identifier) => Expr): Expr = untyped(f)
    def apply(f: (Identifier, Identifier, Identifier, Identifier, Identifier) => Expr): Expr = untyped(f)
    def apply(f: (Identifier, Identifier, Identifier, Identifier, Identifier, Identifier) => Expr): Expr = untyped(f)

    private def untyped(f: Identifier => Expr): Expr = {
      val e = identifier(freshName("e"))
      lambda(e, f(e))
    }

    private def untyped(f: (Identifier, Identifier) => Expr): Expr = {
      val e = identifier(freshName("e"))
      lambda(e, untyped(e1 => f(e, e1)))
    }

    private def untyped(f: (Identifier, Identifier, Identifier) => Expr): Expr = {
      val e = identifier(freshName("e"))
      lambda(e, untyped((e1, e2) => f(e, e1, e2)))
    }

    private def untyped(f: (Identifier, Identifier, Identifier, Identifier) => Expr): Expr = {
      val e = identifier(freshName("e"))
      lambda(e, untyped((e1, e2, e3) => f(e, e1, e2, e3)))
    }

    private def untyped(f: (Identifier, Identifier, Identifier, Identifier, Identifier) => Expr): Expr = {
      val e = identifier(freshName("e"))
      lambda(e, untyped((e1, e2, e3, e4) => f(e, e1, e2, e3, e4)))
    }

    private def untyped(f: (Identifier, Identifier, Identifier, Identifier, Identifier, Identifier) => Expr): Expr = {
      val e = identifier(freshName("e"))
      lambda(e, untyped((e1, e2, e3, e4, e5) => f(e, e1, e2, e3, e4, e5)))
    }

    //noinspection TypeAnnotation
    def apply(ft: FunType[Type, Type]) = new {
      def apply(f: Identifier => Expr): Expr = untyped(f) :: ft
      def apply(f: (Identifier, Identifier) => Expr): Expr = untyped(f) :: ft
      def apply(f: (Identifier, Identifier, Identifier) => Expr): Expr = untyped(f) :: ft
      def apply(f: (Identifier, Identifier, Identifier, Identifier) => Expr): Expr = untyped(f) :: ft
      def apply(f: (Identifier, Identifier, Identifier, Identifier, Identifier) => Expr): Expr = untyped(f) :: ft
      def apply(f: (Identifier, Identifier, Identifier, Identifier, Identifier, Identifier) => Expr): Expr = untyped(f) :: ft
    }
  }

  object nFun {
    def apply(r: lift.arithmetic.Range, f: NatIdentifier => Expr): DepLambda[NatKind] = {
      val x = NatIdentifier(freshName("n"), r)
      depLambda[NatKind](x, f(x))
    }

    def apply(f: NatIdentifier => Expr): DepLambda[NatKind] = {
      nFun(lift.arithmetic.RangeAdd(0, lift.arithmetic.PosInf, 1), f)
    }

    def apply(f: (NatIdentifier, NatIdentifier) => Expr): DepLambda[NatKind] = {
      val r = lift.arithmetic.RangeAdd(0, lift.arithmetic.PosInf, 1)
      val n = NatIdentifier(freshName("n"), r)
      depLambda[NatKind](n, nFun( f(n, _) ))
    }

    def apply(f: (NatIdentifier, NatIdentifier, NatIdentifier) => Expr): DepLambda[NatKind] = {
      val r = lift.arithmetic.RangeAdd(0, lift.arithmetic.PosInf, 1)
      val n = NatIdentifier(freshName("n"), r)
      depLambda[NatKind](n, nFun( (n1, n2) => f(n, n1, n2) ))
    }

    def apply(f: (NatIdentifier, NatIdentifier, NatIdentifier, NatIdentifier) => Expr): DepLambda[NatKind] = {
      val r = lift.arithmetic.RangeAdd(0, lift.arithmetic.PosInf, 1)
      val n = NatIdentifier(freshName("n"), r)
      depLambda[NatKind](n, nFun( (n1, n2, n3) => f(n, n1, n2, n3) ))
    }

    def apply(f: (NatIdentifier, NatIdentifier, NatIdentifier, NatIdentifier, NatIdentifier) => Expr): DepLambda[NatKind] = {
      val r = lift.arithmetic.RangeAdd(0, lift.arithmetic.PosInf, 1)
      val n = NatIdentifier(freshName("n"), r)
      depLambda[NatKind](n, nFun( (n1, n2, n3, n4) => f(n, n1, n2, n3, n4) ))
    }
  }

  object dtFun {
    def apply(f: DataTypeIdentifier => Expr): DepLambda[DataKind] = {
      val x = DataTypeIdentifier(freshName("dt"))
      depLambda[DataKind](x, f(x))
    }
  }

  // type level lambdas
  object n2dtFun {
    def apply(f: NatIdentifier => DataType): NatToDataLambda = {
      val x = NatIdentifier(freshName("n2dt"))
      NatToDataLambda(x, f(x))
    }

    def apply(r: lift.arithmetic.Range)(f: NatIdentifier => DataType): NatToDataLambda = {
      val x = NatIdentifier(freshName("n2dt"), r)
      NatToDataLambda(x, f(x))
    }

    def apply(upperBound: Nat)(f: NatIdentifier => DataType): NatToDataLambda = {
      apply(RangeAdd(0, upperBound, 1))(f)
    }
  }

  object n2nFun {
    def apply(f: NatIdentifier => Nat): NatToNatLambda = {
      val x = NatIdentifier(freshName("n2n"))
      NatToNatLambda(x, f(x))
    }

    def apply(r: lift.arithmetic.Range)(f: NatIdentifier => Nat): NatToNatLambda = {
      val x = NatIdentifier(freshName("n2n"), r)
      NatToNatLambda(x, f(x))
    }

    def apply(upperBound: Nat)(f: NatIdentifier => Nat): NatToNatLambda = {
      apply(RangeAdd(0, upperBound, 1))(f)
    }
  }

  // dependent function types
  object nFunT {
    def apply(f: NatIdentifier => Type): Type = {
      val x = NatIdentifier(freshName("n"))
      DepFunType[NatKind, Type](x, f(x))
    }
  }

  object dtFunT {
    def apply(f: DataTypeIdentifier => Type): Type = {
      val x = DataTypeIdentifier(freshName("dt"))
      DepFunType[DataKind, Type](x, f(x))
    }
  }

  object n2nFunT {
    def apply(f: NatToNat => Type): Type = {
      val x = NatToNatIdentifier(freshName("_n2n"))
      DepFunType[NatToNatKind, Type](x, f(x))
    }
  }

  object n2dtFunT {
    def apply(f: NatToData => Type): Type = {
      val x = NatToDataIdentifier(freshName("_n2dt"))
      DepFunType[NatToDataKind, Type](x, f(x))
    }
  }

  object aFunT {
    def apply(f: AddressSpaceIdentifier => Type): Type = {
      val x = AddressSpaceIdentifier(freshName("a"))
      DepFunType[AddressSpaceKind, Type](x, f(x))
    }
  }

  // types with implicit type parameters
  def implN[A](f: NatIdentifier => A): A = {
    f(NatIdentifier(freshName("_n")))
  }


  def implT[A](f: TypeIdentifier => A): A = {
    f(TypeIdentifier(freshName("_t")))
  }
  def implDT[A](f: DataTypeIdentifier => A): A = {
    f(DataTypeIdentifier(freshName("_dt")))
  }
  // TODO: BasicTypeIdentifier
  def implBT[A](f: DataTypeIdentifier => A): A = {
    f(DataTypeIdentifier(freshName("_dt")))
  }
  // TODO: ScalarTypeIdentifier
  def implST[A](f: DataTypeIdentifier => A): A = {
    f(DataTypeIdentifier(freshName("_dt")))
  }

  def implN2N[A](f: NatToNat => A): A = {
    f(NatToNatIdentifier(freshName("_n2n")))
  }

  def implN2DT[A](f: NatToData => A): A = {
    f(NatToDataIdentifier(freshName("_n2dt")))
  }

  def implA[A](f: AddressSpaceIdentifier => A): A = {
    f(AddressSpaceIdentifier(freshName("_w")))
  }

  def freshTypeIdentifier: Type = implT(identity)

  implicit def wrapInNatExpr(n: Nat): Literal = literal(NatData(n))

  def l(i: Int): Literal = literal(IntData(i))
  def l(f: Float): Literal = literal(FloatData(f))
  def l(d: Double): Literal = literal(DoubleData(d))
  def lidx(i: Nat, n: Nat): Literal = literal(IndexData(i, n))
  def lvec(v: Seq[ScalarData]): Literal = literal(VectorData(v))
  def larr(a: Seq[Data]): Literal = literal(ArrayData(a))

  implicit final class TypeConstructors(private val r: Type) extends AnyVal {
    @inline def ->:(t: Type): FunType[Type, Type] = FunType(t, r)
  }

  implicit final class TupleTypeConstructors(private val a: DataType) extends AnyVal {
    @inline def x(b: DataType): PairType = PairType(a, b)
  }

  final case class ArrayTypeConstructorHelper(ns: Seq[Nat]) {
    @inline def `.`(n: Nat): ArrayTypeConstructorHelper = ArrayTypeConstructorHelper(ns :+ n)
    @inline def `.`(dt: DataType): ArrayType = {
      val nsr = ns.reverse
      nsr.tail.foldLeft(ArrayType(nsr.head, dt))( (t, n) => ArrayType(n, t) )
    }
  }

  implicit final class ArrayTypeConstructors(private val n: Nat) extends AnyVal {
    @inline def `.`(m: Nat): ArrayTypeConstructorHelper = ArrayTypeConstructorHelper(Seq(n, m))
    @inline def `.`(dt: DataType): ArrayType = ArrayType(n, dt)
  }

  implicit final class ArrayTypeConstructorsFromInt(private val n: Int) extends AnyVal {
    @inline def `.`(m: Nat): ArrayTypeConstructorHelper = ArrayTypeConstructorHelper(Seq(Cst(n), m))
    @inline def `.`(dt: DataType): ArrayType = ArrayType(Cst(n), dt)
  }

  object foreignFun {
    def apply(name: String, t: Type): Expr = {
      ForeignFunction(ForeignFunction.Decl(name, None))(t)
    }

    def apply(name: String, params: Seq[String], body: String, t: Type): Expr = {
      ForeignFunction(ForeignFunction.Decl(name,
        Some(ForeignFunction.Def(params, body))))(t)
    }
  }
}
