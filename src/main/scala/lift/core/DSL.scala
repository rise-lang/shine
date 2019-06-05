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

  implicit class FunCall(f: Expr) {
    import lift.core.lifting._

    def apply(e: Expr): Expr = liftFunExpr(f).value(e)
    def apply(n: Nat): Expr = liftDepFunExpr[NatKind](f).value(n)
    def apply(dt: DataType): Expr = liftDepFunExpr[DataKind](f).value(dt)
    def apply(n2n: NatToNat): Expr = liftDepFunExpr[NatToNatKind](f).value(n2n)
  }

  implicit class FunPipe(e: Expr) {
    def |>(f: Expr): Expr = f(e)
  }

  implicit class FunComp(f: Expr) {
    def >>(g: Expr): Expr = fun(x => g(f(x)))
  }

  // function values
  object fun {
    def apply(f: Identifier => Expr): Expr = {
      val x = Identifier(freshName("e"))
      Lambda(x, f(x))
    }

    def apply(dt: DataType)(f: Expr => Expr): Expr = {
      val x = Identifier(freshName("e"))
      Lambda(x, f(TypedExpr(x, dt)))
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
  }

  object n2nFun {
    def apply(f: NatIdentifier => Nat): NatToNatLambda = {
      val x = NatIdentifier(freshName("n2n"))
      NatToNatLambda(x, f(x))
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

  implicit def wrapInNatExpr(n: Nat): NatExpr = NatExpr(n)

  def mapNatExpr(n: Expr, f: Nat => Nat): NatExpr = {
    NatExpr(f(Internal.natFromNatExpr(n)))
  }

  private object Internal {
    def natFromNatExpr(e: Expr): Nat = {
      e match {
        case NatExpr(n) => n

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

  implicit final class FunTypeHelper(private val a: Type) extends AnyVal {
    @inline def ->(b: Type): Type = FunType(a, b)
  }

  implicit final class ArrayTypeHelper(private val n: Nat) extends AnyVal {
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
