package lift.core

import lift.arithmetic.{Cst, RangeAdd}
import lift.core.types._
import lift.core.semantics._

import scala.language.implicitConversions

object DSL {
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
    def _1: Expr = primitives.fst(lhs)
    def _2: Expr = primitives.snd(lhs)
  }

  implicit class Indexing(e: Expr) {
    import lift.core.primitives._

    def `@`(i: Expr): Expr = idx(i)(e)
  }

  implicit class TypeAnnotation(t: Type) {
    def ::(e: Expr): TypedExpr = TypedExpr(e, t)
    def `:`(e: Expr): TypedExpr = TypedExpr(e, t)
  }

  implicit class FunCall(f: Expr) {
    def apply(e: Expr): Expr = Apply(f, e)
    def apply(n: Nat): Expr = DepApply[NatKind](f, n)
    def apply(dt: DataType): Expr = DepApply[DataKind](f, dt)
    def apply(a: AddressSpace): Expr = DepApply[AddressSpaceKind](f, a)
    def apply(n2n: NatToNat): Expr = DepApply[NatToNatKind](f, n2n)

    def apply(e1: Expr, e2: Expr): Expr =
      f(e1)(e2)
    def apply(e1: Expr, e2: Expr, e3: Expr): Expr =
      f(e1)(e2)(e3)
    def apply(e1: Expr, e2: Expr, e3: Expr, e4: Expr): Expr =
      f(e1)(e2)(e3)(e4)
    def apply(e1: Expr, e2: Expr, e3: Expr, e4: Expr, e5: Expr): Expr =
      f(e1)(e2)(e3)(e4)(e5)
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
      val x = Identifier(freshName("e"))
      Lambda(x, f(TypedExpr(x, t)))
    }

    def apply(f: Identifier => Expr): Expr = untyped(f)
    def apply(f: (Identifier, Identifier) => Expr): Expr = untyped(f)
    def apply(f: (Identifier, Identifier, Identifier) => Expr): Expr = untyped(f)
    def apply(f: (Identifier, Identifier, Identifier, Identifier) => Expr): Expr = untyped(f)
    def apply(f: (Identifier, Identifier, Identifier, Identifier, Identifier) => Expr): Expr = untyped(f)
    def apply(f: (Identifier, Identifier, Identifier, Identifier, Identifier, Identifier) => Expr): Expr = untyped(f)

    private def untyped(f: Identifier => Expr): Expr = {
      val e = Identifier(freshName("e"))
      Lambda(e, f(e))
    }

    private def untyped(f: (Identifier, Identifier) => Expr): Expr = {
      val e = Identifier(freshName("e"))
      Lambda(e, untyped(e1 => f(e, e1)))
    }

    private def untyped(f: (Identifier, Identifier, Identifier) => Expr): Expr = {
      val e = Identifier(freshName("e"))
      Lambda(e, untyped((e1, e2) => f(e, e1, e2)))
    }

    private def untyped(f: (Identifier, Identifier, Identifier, Identifier) => Expr): Expr = {
      val e = Identifier(freshName("e"))
      Lambda(e, untyped((e1, e2, e3) => f(e, e1, e2, e3)))
    }

    private def untyped(f: (Identifier, Identifier, Identifier, Identifier, Identifier) => Expr): Expr = {
      val e = Identifier(freshName("e"))
      Lambda(e, untyped((e1, e2, e3, e4) => f(e, e1, e2, e3, e4)))
    }

    private def untyped(f: (Identifier, Identifier, Identifier, Identifier, Identifier, Identifier) => Expr): Expr = {
      val e = Identifier(freshName("e"))
      Lambda(e, untyped((e1, e2, e3, e4, e5) => f(e, e1, e2, e3, e4, e5)))
    }

    //noinspection TypeAnnotation
    def apply(ft: FunType[Type, Type]) = new {
      def apply(f: Identifier => Expr): TypedExpr = untyped(f) :: ft
      def apply(f: (Identifier, Identifier) => Expr): TypedExpr = untyped(f) :: ft
      def apply(f: (Identifier, Identifier, Identifier) => Expr): TypedExpr = untyped(f) :: ft
      def apply(f: (Identifier, Identifier, Identifier, Identifier) => Expr): TypedExpr = untyped(f) :: ft
      def apply(f: (Identifier, Identifier, Identifier, Identifier, Identifier) => Expr): TypedExpr = untyped(f) :: ft
      def apply(f: (Identifier, Identifier, Identifier, Identifier, Identifier, Identifier) => Expr): TypedExpr = untyped(f) :: ft
    }
  }

  object nFun {
    def apply(r: lift.arithmetic.Range, f: NatIdentifier => Expr): DepLambda[NatKind] = {
      val x = NatIdentifier(freshName("n"), r)
      DepLambda[NatKind](x, f(x))
    }

    def apply(f: NatIdentifier => Expr): DepLambda[NatKind] = {
      nFun(lift.arithmetic.RangeAdd(0, lift.arithmetic.PosInf, 1), f)
    }

    def apply(f: (NatIdentifier, NatIdentifier) => Expr): DepLambda[NatKind] = {
      val r = lift.arithmetic.RangeAdd(0, lift.arithmetic.PosInf, 1)
      val n = NatIdentifier(freshName("n"), r)
      DepLambda[NatKind](n, nFun( f(n, _) ))
    }

    def apply(f: (NatIdentifier, NatIdentifier, NatIdentifier) => Expr): DepLambda[NatKind] = {
      val r = lift.arithmetic.RangeAdd(0, lift.arithmetic.PosInf, 1)
      val n = NatIdentifier(freshName("n"), r)
      DepLambda[NatKind](n, nFun( (n1, n2) => f(n, n1, n2) ))
    }

    def apply(f: (NatIdentifier, NatIdentifier, NatIdentifier, NatIdentifier) => Expr): DepLambda[NatKind] = {
      val r = lift.arithmetic.RangeAdd(0, lift.arithmetic.PosInf, 1)
      val n = NatIdentifier(freshName("n"), r)
      DepLambda[NatKind](n, nFun( (n1, n2, n3) => f(n, n1, n2, n3) ))
    }

    def apply(f: (NatIdentifier, NatIdentifier, NatIdentifier, NatIdentifier, NatIdentifier) => Expr): DepLambda[NatKind] = {
      val r = lift.arithmetic.RangeAdd(0, lift.arithmetic.PosInf, 1)
      val n = NatIdentifier(freshName("n"), r)
      DepLambda[NatKind](n, nFun( (n1, n2, n3, n4) => f(n, n1, n2, n3, n4) ))
    }
  }

  object dtFun {
    def apply(f: DataTypeIdentifier => Expr): DepLambda[DataKind] = {
      val x = DataTypeIdentifier(freshName("dt"))
      DepLambda[DataKind](x, f(x))
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

  implicit def wrapInNatExpr(n: Nat): Literal = Literal(NatData(n))

  def l(i: Int): Literal = Literal(IntData(i))
  def l(f: Float): Literal = Literal(FloatData(f))
  def l(d: Double): Literal = Literal(DoubleData(d))
  def lidx(i: Nat, n: Nat): Literal = Literal(IndexData(i, n))
  def lvec(v: Seq[ScalarData]): Literal = Literal(VectorData(v))
  def larr(a: Seq[Data]): Literal = Literal(ArrayData(a))

  implicit final class TypeConstructors(private val r: Type) extends AnyVal {
    @inline def ->:(t: Type): FunType[Type, Type] = FunType(t, r)
  }

  implicit final class TupleTypeConstructors(private val a: DataType) extends AnyVal {
    @inline def x(b: DataType): TupleType = TupleType(a, b)
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
      ForeignFunction(ForeignFunction.Decl(name, None), t)
    }

    def apply(name: String, params: Seq[String], body: String, t: Type): Expr = {
      ForeignFunction(ForeignFunction.Decl(name,
        Some(ForeignFunction.Def(params, body))), t)
    }
  }
}
