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

  implicit class FunCall(f: Expr) {
    import lift.core.lifting._

    def apply(e: Expr): Expr = liftFunctionExpr(f).value(e)
    def apply(n: Nat): Expr = liftDependentFunctionExpr[NatKind](f).value(n)
    def apply(dt: DataType): Expr = liftDependentFunctionExpr[DataKind](f).value(dt)
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

    //TODO use TypedExpr one line earlier already?
    def apply(dt: DataType)(f: Expr => Expr): Expr = {
      val x = Identifier(freshName("e"))
      Lambda(x, f(TypedExpr(x, dt)))
    }
  }

  object nFun {
    def apply(r: lift.arithmetic.Range, f: NatIdentifier => Expr): NatDepLambda = {
      val x = NatIdentifier(freshName("n"), r)
      NatDepLambda(x, f(x))
    }

    def apply(f: NatIdentifier => Expr): NatDepLambda = {
      nFun(lift.arithmetic.RangeAdd(0, lift.arithmetic.PosInf, 1), f)
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

  def implT[A](f: DataTypeIdentifier => A): A = {
    f(DataTypeIdentifier(freshName("_dt")))
  }

  def implNNF[A](f: NatNatFunction => A): A = {
    f(NatNatFunctionIdentifier(freshName("_nnf")))
  }

  def implNDF[A](f: NatDataTypeFunction => A): A = {
    f(NatDataTypeFunctionIdentifier(freshName("_ndf")))
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

  implicit final class To(private val a: Type) extends AnyVal {
    @inline def ->(b: Type): Type = FunctionType(a, b)
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
