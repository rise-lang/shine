package idealised.SurfaceLanguage

import idealised.SurfaceLanguage.DSL.{DataExpr, unsafeAsIndex}
import idealised.SurfaceLanguage.Primitives.{AsNat, UnsafeAsIndex}
import idealised.SurfaceLanguage.Semantics.IndexData
import idealised.SurfaceLanguage.Types._
import lift.arithmetic.{NamedVar, RangeAdd, StartFromRange}

import scala.language.postfixOps
import scala.language.reflectiveCalls

object Lifting {

  def liftFunctionExpr[T <: Type](p: Expr[DataType -> T]): DataExpr => Expr[T] = {
    p match {
      case l: LambdaExpr[T] =>
        (arg: DataExpr) =>  Expr.substitute(arg, `for` = l.param, in = l.body)
      case app: ApplyExpr[DataType -> T] =>
        val fun = liftFunctionExpr(app.fun)
        liftFunctionExpr(fun(app.arg))
      case app: NatDependentApplyExpr[DataType -> T] =>
        val fun = liftNatDependentFunctionExpr(app.fun)
        liftFunctionExpr(fun(app.arg))
      case app: TypeDependentApplyExpr[DataType -> T] =>
        val fun = liftTypeDependentFunctionExpr(app.fun)
        liftFunctionExpr(fun(app.arg))
      case IfThenElseExpr(_, _, _) =>
        throw new Exception("This should never happen")
    }
  }

  def liftNatDependentFunctionExpr[T <: Type](p: Expr[`(nat)->`[T]]): Nat => Expr[T] = {
    p match {
      case l: NatDependentLambdaExpr[T] =>
        (arg: Nat) => Type.substitute(arg, `for` = l.x, in = l.body)
      case app: ApplyExpr[`(nat)->`[T]] =>
        val fun = liftFunctionExpr(app.fun)
        liftNatDependentFunctionExpr(fun(app.arg))
      case app: NatDependentApplyExpr[`(nat)->`[T]] =>
        val fun = liftNatDependentFunctionExpr(app.fun)
        liftNatDependentFunctionExpr(fun(app.arg))
      case app: TypeDependentApplyExpr[`(nat)->`[T]] =>
        val fun = liftTypeDependentFunctionExpr(app.fun)
        liftNatDependentFunctionExpr(fun(app.arg))
      case IfThenElseExpr(_, _, _) =>
        throw new Exception("This should never happen")
    }
  }

  def liftTypeDependentFunctionExpr[T <: Type](p: Expr[`(dt)->`[T]]): DataType => Expr[T] = {
    p match {
      case l: TypeDependentLambdaExpr[T] =>
        (arg: DataType) => Type.substitute(arg, `for` = l.x, in = l.body)
      case app: ApplyExpr[`(dt)->`[T]] =>
        val fun = liftFunctionExpr(app.fun)
        liftTypeDependentFunctionExpr(fun(app.arg))
      case app: NatDependentApplyExpr[`(dt)->`[T]] =>
        val fun = liftNatDependentFunctionExpr(app.fun)
        liftTypeDependentFunctionExpr(fun(app.arg))
      case app: TypeDependentApplyExpr[`(dt)->`[T]] =>
        val fun = liftTypeDependentFunctionExpr(app.fun)
        liftTypeDependentFunctionExpr(fun(app.arg))
      case IfThenElseExpr(_, _, _) =>
        throw new Exception("This should never happen")
    }
  }

  def liftIndexExpr(p: Expr[DataType]): Nat = {
    p.t match {
      case Some(IndexType(n)) =>
        p match {
          case i: IdentifierExpr => NamedVar(i.name, RangeAdd(0, n, 1))
          case ApplyExpr(fun, arg) => liftIndexExpr(liftFunctionExpr(fun)(arg))
          case BinOpExpr(op, lhs, rhs) => binOpToNat(op, liftIndexExpr(lhs), liftIndexExpr(rhs))
          case IfThenElseExpr(_, _, _) => ???
          case LiteralExpr(lit) => lit match {
            case i: IndexData => i.n
            case _ => throw new Exception("This should never happen")
          }
          case NatExpr(_) => throw new Exception("This should never happen")
          case NatDependentApplyExpr(fun, arg) => liftIndexExpr(liftNatDependentFunctionExpr(fun)(arg))
          case TypeDependentApplyExpr(fun, arg) => liftIndexExpr(liftTypeDependentFunctionExpr(fun)(arg))
          case UnaryOpExpr(op, e) => unOpToNat(op, liftIndexExpr(e))
          case prim: PrimitiveExpr => prim match {
            //TODO can we use our knowledge of n somehow?
            case UnsafeAsIndex(n, e, _) => liftNatExpr(e)
            case _ => ???
          }
        }
      case _ => throw new Exception("This should never happen")
    }
  }

  def liftNatExpr(p: Expr[DataType]): Nat = {
    p.t match {
      case Some(NatType) =>
        p match {
          case NatExpr(n) => n
          case i: IdentifierExpr => NamedVar(i.name, StartFromRange(0))
          case ApplyExpr(fun, arg) => liftNatExpr(liftFunctionExpr(fun)(arg))
          case BinOpExpr(op, lhs, rhs) => binOpToNat(op, liftNatExpr(lhs), liftNatExpr(rhs))
          case IfThenElseExpr(_, _, _) => ???
          case LiteralExpr(_) => throw new Exception("This should never happen")
          case NatDependentApplyExpr(fun, arg) => liftNatExpr(liftNatDependentFunctionExpr(fun)(arg))
          case TypeDependentApplyExpr(fun, arg) => liftNatExpr(liftTypeDependentFunctionExpr(fun)(arg))
          case UnaryOpExpr(op, e) => unOpToNat(op, liftNatExpr(e))
          case prim: PrimitiveExpr => prim match {
            //TODO can we use our knowledge of n somehow?
            case AsNat(e, _) => liftIndexExpr(e)
            case _ => ???
          }
        }
      case pt => throw new Exception(s"Expected exp[nat] but found $pt.")
    }
  }

  def binOpToNat(op:Operators.Binary.Value, n1:Nat, n2:Nat): Nat = {
    import Operators.Binary._

    op match {
      case ADD => n1 + n2
      case SUB => n1 - n2
      case MUL => n1 * n2
      case DIV => n1 / n2
      case MOD => n1 % n2

      case _ => ???
    }
  }

  def unOpToNat(op:Operators.Unary.Value, n:Nat):Nat = ???
}
