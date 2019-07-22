package lift.core

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

  implicit class DataTypeAnnotation(dt: DataType) {
    def ::(e: Expr): TypedExpr = TypedExpr(e, dt._R)
    def `:`(e: Expr): TypedExpr = TypedExpr(e, dt._R)
  }

  implicit class FunCall(f: Expr) {
    import lift.core.lifting._

    def apply(n: Nat): Expr = liftDependentFunctionExpr[NatKind](f).value(n)
    def apply(dt: DataType): Expr = liftDependentFunctionExpr[DataKind](f).value(dt)
    def apply(a: AddressSpace): Expr = liftDependentFunctionExpr[AddressSpaceKind](f).value(a)

    def apply(e: Expr): Expr = liftFunctionExpr(f).value(e)

    def apply(e1: Expr, e2: Expr): Expr = {
      val g = liftFunctionExpr(f).value(e1)
      g.apply(e2)
    }

    def apply(e1: Expr, e2: Expr, e3: Expr): Expr = {
      val g = liftFunctionExpr(f).value(e1)
      g.apply(e2, e3)
    }

    def apply(e1: Expr, e2: Expr, e3: Expr, e4: Expr): Expr = {
      val g = liftFunctionExpr(f).value(e1)
      g.apply(e2, e3, e4)
    }

    def apply(e1: Expr, e2: Expr, e3: Expr, e4: Expr, e5: Expr): Expr = {
      val g = liftFunctionExpr(f).value(e1)
      g.apply(e2, e3, e4, e5)
    }
  }

  implicit class FunPipe(e: Expr) {
    def |>(f: Expr): Expr = f.apply(e)
  }

  implicit class FunComp(f: Expr) {
    def >>(g: Expr): Expr = fun(x => g(f(x)))
  }

  // function values
  object fun {
    def apply(dt: DataType)(f: TypedExpr => Expr): Expr = {
      val x = Identifier(freshName("e"))
      Lambda(x, f(TypedExpr(x, dt)))
    }

    def apply(f: Identifier => Expr): Expr = {
      val x = Identifier(freshName("e"))
      Lambda(x, f(x))
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
    def apply(ft: FunctionType[Type, Type]) = new {
      def apply(f: Identifier => Expr): TypedExpr = fun.untyped(f) :: ft

      def apply(f: (Identifier, Identifier) => Expr): TypedExpr = fun.untyped(f) :: ft

      def apply(f: (Identifier, Identifier, Identifier) => Expr): TypedExpr = fun.untyped(f) :: ft

      def apply(f: (Identifier, Identifier, Identifier, Identifier) => Expr): TypedExpr = fun.untyped(f) :: ft

      def apply(f: (Identifier, Identifier, Identifier, Identifier, Identifier) => Expr): TypedExpr = fun.untyped(f) :: ft
    }

    def apply(dt: DataType)(f: Expr => Expr): Expr = apply(dt._R)(f)
  }

  object nFun {
    def apply(r: lift.arithmetic.Range, f: NatIdentifier => Expr): NatDepLambda = {
      val x = NatIdentifier(freshName("n"), r)
      NatDepLambda(x, f(x))
    }

    def apply(f: NatIdentifier => Expr): NatDepLambda = {
      nFun(lift.arithmetic.RangeAdd(0, lift.arithmetic.PosInf, 1), f)
    }

    def apply(f: (NatIdentifier, NatIdentifier) => Expr): NatDepLambda = {
      val r = lift.arithmetic.RangeAdd(0, lift.arithmetic.PosInf, 1)
      val n = NatIdentifier(freshName("n"), r)
      NatDepLambda(n, nFun( f(n, _) ))
    }

    def apply(f: (NatIdentifier, NatIdentifier, NatIdentifier) => Expr): NatDepLambda = {
      val r = lift.arithmetic.RangeAdd(0, lift.arithmetic.PosInf, 1)
      val n = NatIdentifier(freshName("n"), r)
      NatDepLambda(n, nFun( (n1, n2) => f(n, n1, n2) ))
    }

    def apply(f: (NatIdentifier, NatIdentifier, NatIdentifier, NatIdentifier) => Expr): NatDepLambda = {
      val r = lift.arithmetic.RangeAdd(0, lift.arithmetic.PosInf, 1)
      val n = NatIdentifier(freshName("n"), r)
      NatDepLambda(n, nFun( (n1, n2, n3) => f(n, n1, n2, n3) ))
    }

    def apply(f: (NatIdentifier, NatIdentifier, NatIdentifier, NatIdentifier, NatIdentifier) => Expr): NatDepLambda = {
      val r = lift.arithmetic.RangeAdd(0, lift.arithmetic.PosInf, 1)
      val n = NatIdentifier(freshName("n"), r)
      NatDepLambda(n, nFun( (n1, n2, n3, n4) => f(n, n1, n2, n3, n4) ))
    }
  }

  object tFun {
    def apply(f: DataTypeIdentifier => Expr): TypeDepLambda = {
      val x = DataTypeIdentifier(freshName("dt"))
      TypeDepLambda(x, f(x))
    }
  }

  // dependent function types
  object nFunT {
    def apply(f: NatIdentifier => Type): Type = {
      val x = NatIdentifier(freshName("n"))
      NatDependentFunctionType(x, f(x))
    }
  }

  object tFunT {
    def apply(f: DataTypeIdentifier => Type): Type = {
      val x = DataTypeIdentifier(freshName("dt"))
      TypeDependentFunctionType(x, f(x))
    }
  }

  object nFunA {
    def apply(f: AddressSpaceIdentifier => Type): Type = {
      val x = AddressSpaceIdentifier(freshName("a"))
      AddressSpaceDependentFunctionType(x, f(x))
    }
  }

  // type level functions
  object natTypeFun {
    def apply(f: NatIdentifier => DataType): NatDataTypeLambda = {
      val x = NatIdentifier(freshName("n"))
      NatDataTypeLambda(x, f(x))
    }
  }

  object natNatFun {
    def apply(f: NatIdentifier => Nat): NatNatLambda = {
      val x = NatIdentifier(freshName("n"))
      NatNatLambda(x, f(x))
    }
  }

  // types with implicit type parameters
  def implN[A](f: NatIdentifier => A): A = {
    f(NatIdentifier(freshName("_n")))
  }

  def implDT[A](f: DataTypeIdentifier => A): A = {
    f(DataTypeIdentifier(freshName("_dt")))
  }

  def implNNF[A](f: NatNatFunction => A): A = {
    f(NatNatFunctionIdentifier(freshName("_nnf")))
  }

  def implNDF[A](f: NatDataTypeFunction => A): A = {
    f(NatDataTypeFunctionIdentifier(freshName("_ndf")))
  }

  def implW[A](f: AccessTypeIdentifier => A): A = {
    f(AccessTypeIdentifier(freshName("_w")))
  }

  def implA[A](f: AddressSpaceIdentifier => A): A = {
    f(AddressSpaceIdentifier(freshName("_w")))
  }

  implicit def natToExpr(n: Nat): NatExpr = NatExpr(n)

  def mapNatExpr(n: Expr, f: Nat => Nat): NatExpr = {
    NatExpr(f(Internal.natFromNatExpr(n)))
  }

  private object Internal {
    def natFromNatExpr(e: Expr): Nat = {
      e match {
        case NatExpr(n) => n

        case _: Identifier      => ??? // NamedVar(i.name, StartFromRange(0))
        case Apply(fun, arg)    => fun match {
          case l: Lambda => natFromNatExpr(lifting.liftFunctionExpr(l).value(arg))
          case p: Primitive       => p match {
            case _: primitives.indexAsNat.type => natFromNatExpr(arg)
            case _ => ???
          }
          case _ => ???
        }
        case DepApply(fun, arg) => natFromNatExpr(lifting.liftDependentFunctionExpr(fun).value(arg))
        case Literal(_)         => throw new Exception("This should never happen")
        case Index(_, _)        => ???
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

  implicit final class TypeConstructors(private val a: Type) extends AnyVal {
    @inline def ->:(b: Type): FunctionType[Type, Type] = FunctionType(b, a)
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
