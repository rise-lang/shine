package lift.core

import lift.core.types.{DataType, DataTypeIdentifier}
import lift.core.semantics._

object DSL {

  implicit class BinOps(lhs: Expr) {
    import lift.core.primitives.{BinOp, UnaryOp}

    def +(rhs: Expr) = BinOp(Operators.Binary.ADD)(lhs)(rhs)
    def -(rhs: Expr) = BinOp(Operators.Binary.SUB)(lhs)(rhs)
    def *(rhs: Expr) = BinOp(Operators.Binary.MUL)(lhs)(rhs)
    def /(rhs: Expr) = BinOp(Operators.Binary.DIV)(lhs)(rhs)
    def %(rhs: Expr) = BinOp(Operators.Binary.MOD)(lhs)(rhs)
    def >(rhs: Expr) = BinOp(Operators.Binary.GT)(lhs)(rhs)
    def <(rhs: Expr) = BinOp(Operators.Binary.LT)(lhs)(rhs)
    def =:=(rhs: Expr) = BinOp(Operators.Binary.EQ)(lhs)(rhs)

    def unary_- = UnaryOp(Operators.Unary.NEG)(lhs)
  }

  implicit class FunCall(f: Expr) {
    import lift.core.lifting._

    def apply(e: Expr): Expr = liftFunctionExpr(f).value(e)
    def apply(n: Nat): Expr = liftNatDependentFunctionExpr(f).value(n)
    def apply(dt: DataType): Expr = liftTypeDependentFunctionExpr(f).value(dt)
  }

  implicit class FunPipe(e: Expr) {
    def |>(f: Expr): Expr = f(e)
  }

  implicit class FunComp(f: Expr) {
    def \>(g: Expr): Expr = fun(x => g(f(x)))
  }

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
    def apply(f: NatIdentifier => Expr): NatLambda = {
      val x = lift.arithmetic.NamedVar(freshName("n"))
      NatLambda(x, f(x))
    }
  }

  object tFun {
    def apply(f: DataTypeIdentifier => Expr): TypeLambda = {
      val x = DataTypeIdentifier(freshName("dt"))
      TypeLambda(x, f(x))
    }
  }
  
  def l(i: Int): Literal = Literal(IntData(i))
  def l(f: Float): Literal = Literal(FloatData(f))
  def l(d: Double): Literal = Literal(DoubleData(d))
  def l(v: VectorData): Literal = Literal(v)
}