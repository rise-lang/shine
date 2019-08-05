package lift.core

import lift.arithmetic.RangeAdd
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

  implicit class TypeAnnotation(t: Type) {
    def ::(e: Expr): TypedExpr = TypedExpr(e, t)
    def `:`(e: Expr): TypedExpr = TypedExpr(e, t)
  }

  implicit class FunCall(f: Expr) {
    import lift.core.lifting._

    def apply(e: Expr): Expr = liftFunExpr(f).value(e)
    def apply(n: Nat): Expr = liftDepFunExpr[NatKind](f).value(n)
    def apply(dt: DataType): Expr = liftDepFunExpr[DataKind](f).value(dt)
    def apply(n2n: NatToNat): Expr = liftDepFunExpr[NatToNatKind](f).value(n2n)

    def apply(e1: Expr, e2: Expr): Expr = {
      liftFunExpr(f).value(e1).apply(e2)
    }

    def apply(e1: Expr, e2: Expr, e3: Expr): Expr = {
      liftFunExpr(f).value(e1).apply(e2, e3)
    }

    def apply(e1: Expr, e2: Expr, e3: Expr, e4: Expr): Expr = {
      liftFunExpr(f).value(e1).apply(e2, e3, e4)
    }

    def apply(e1: Expr, e2: Expr, e3: Expr, e4: Expr, e5: Expr): Expr = {
      liftFunExpr(f).value(e1).apply(e2, e3, e4, e5)
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
    def apply(f: Identifier => Expr): Expr = {
      val x = Identifier(freshName("e"))
      Lambda(x, f(x))
    }

    def apply(dt: DataType)(f: TypedExpr => Expr): Expr = {
      val x = Identifier(freshName("e"))
      Lambda(x, f(TypedExpr(x, dt)))
    }

    def apply(f: (Identifier, Identifier) => Expr): Expr = untyped(f)

    def apply(f: (Identifier, Identifier, Identifier) => Expr): Expr = untyped(f)

    def apply(f: (Identifier, Identifier, Identifier, Identifier) => Expr): Expr = untyped(f)

    def apply(f: (Identifier, Identifier, Identifier, Identifier, Identifier) => Expr): Expr = untyped(f)

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

    //noinspection TypeAnnotation
    def apply(ft: FunType[Type, Type]) = new {
      def apply(f: Identifier => Expr): TypedExpr = untyped(f) :: ft

      def apply(f: (Identifier, Identifier) => Expr): TypedExpr = untyped(f) :: ft

      def apply(f: (Identifier, Identifier, Identifier) => Expr): TypedExpr = untyped(f) :: ft

      def apply(f: (Identifier, Identifier, Identifier, Identifier) => Expr): TypedExpr = untyped(f) :: ft

      def apply(f: (Identifier, Identifier, Identifier, Identifier, Identifier) => Expr): TypedExpr = untyped(f) :: ft
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

  // type level functions
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

  // types with implicit type parameters
  def implN[A](f: NatIdentifier => A): A = {
    f(NatIdentifier(freshName("_n")))
  }

  def implDT[A](f: DataTypeIdentifier => A): A = {
    f(DataTypeIdentifier(freshName("_dt")))
  }

  def implN2N[A](f: NatToNat => A): A = {
    f(NatToNatIdentifier(freshName("_n2n")))
  }

  def implN2DT[A](f: NatToData => A): A = {
    f(NatToDataIdentifier(freshName("_n2dt")))
  }

  implicit def wrapInNatExpr(n: Nat): Literal = Literal(NatData(n))

  def mapNatExpr(n: Expr, f: Nat => Nat): Literal = {
    Literal(NatData(f(Internal.natFromNatExpr(n))))
  }

  private object Internal {
    @scala.annotation.tailrec
    def natFromNatExpr(e: Expr): Nat = {
      e match {
        case Literal(NatData(n)) => n

        case _: Identifier      => ??? // NamedVar(i.name, StartFromRange(0))
        case Apply(fun, arg)    => fun match {
          case l: Lambda => natFromNatExpr(lifting.liftFunExpr(l).value(arg))
          case p: Primitive       => p match {
            case _: primitives.indexAsNat.type => natFromNatExpr(arg)
            case _ => ???
          }
          case _ => ???
        }
        case DepApply(fun, arg) => natFromNatExpr(lifting.liftDepFunExpr(fun).value(arg))
        case Literal(_)         => throw new Exception("This should never happen")
        case TypedExpr(te, _)   => natFromNatExpr(te)
        case pt => throw new Exception(s"Expected exp[nat] but found $pt.")
      }
    }
  }

  def l(i: Int): Literal = Literal(IntData(i))
  def l(f: Float): Literal = Literal(FloatData(f))
  def l(d: Double): Literal = Literal(DoubleData(d))
  def l(v: VectorData): Literal = Literal(v)
  def l(a: ArrayData): Literal = Literal(a)

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

  object foreignFun {
    def apply(name: String, t: Type): Expr = {
      primitives.ForeignFunction(primitives.ForeignFunction.Decl(name, None), t)
    }

    def apply(name: String, params: Seq[String], body: String, t: Type): Expr = {
      primitives.ForeignFunction(primitives.ForeignFunction.Decl(name,
        Some(primitives.ForeignFunction.Def(params, body))), t)
    }
  }
}
