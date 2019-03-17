package idealised.SurfaceLanguage

import idealised.SurfaceLanguage.Primitives._
import idealised.SurfaceLanguage.Semantics._
import idealised.SurfaceLanguage.Types._
import lift.arithmetic.{NamedVar, RangeAdd, StartFromRange}

import scala.language.implicitConversions

package object DSL {
  type DataExpr = Expr[DataType]

  implicit class BinOps(lhs: DataExpr) {
    def +(rhs: DataExpr) = BinOpExpr(Operators.Binary.ADD, lhs, rhs)
    def -(rhs: DataExpr) = BinOpExpr(Operators.Binary.SUB, lhs, rhs)
    def *(rhs: DataExpr) = BinOpExpr(Operators.Binary.MUL, lhs, rhs)
    def /(rhs: DataExpr) = BinOpExpr(Operators.Binary.DIV, lhs, rhs)
    def %(rhs: DataExpr) = BinOpExpr(Operators.Binary.MOD, lhs, rhs)
    def >(rhs: DataExpr) = BinOpExpr(Operators.Binary.GT, lhs, rhs)
    def <(rhs: DataExpr) = BinOpExpr(Operators.Binary.LT, lhs, rhs)
    def =:=(rhs: DataExpr) = BinOpExpr(Operators.Binary.EQ, lhs, rhs)
    def unary_- = UnaryOpExpr(Operators.Unary.NEG, lhs)
  }

  implicit class FunCall[T <: Type](f: Expr[DataType -> T]) {
    def apply(arg: DataExpr): Expr[T] = Lifting.liftFunctionExpr(f)(arg)
    def $(arg: DataExpr): Expr[T] = apply(arg)
  }

  implicit class FunCallExpr(arg: DataExpr) {
    def :>>[T <: Type](f: Expr[DataType -> T]): Expr[T] = f(arg)
    def <<:[T <: Type](f: Expr[DataType -> T]): Expr[T] = f(arg)
  }

  implicit class FunCallExprPair(args: (Expr[DataType], Expr[DataType])) {
    def :>>(z: (DataExpr, DataExpr) => Zip): Zip = z(args._1, args._2)
    def <<:(z: (DataExpr, DataExpr) => Zip): Zip = z(args._1, args._2)
  }

  implicit class CallNatDependentLambda[T <: Type](f: Expr[`(nat)->`[T]]) {
    def apply(arg: Nat): Expr[T] = Lifting.liftNatDependentFunctionExpr(f)(arg)
    def $(arg: Nat): Expr[T] = apply(arg)
  }

  implicit class CallNatDependentLambdaExpr(arg: Nat) {
    def :>>[T <: Type](f: Expr[`(nat)->`[T]]): Expr[T] = f(arg)
    def <<:[T <: Type](f: Expr[`(nat)->`[T]]): Expr[T] = f(arg)
  }

  implicit class CallTypeDependentLambda[T <: Type](f: Expr[`(dt)->`[T]]) {
    def apply(arg: DataType): Expr[T] = Lifting.liftTypeDependentFunctionExpr(f)(arg)
    def $(arg: DataType): Expr[T] = apply(arg)
  }

  implicit class CallTypeDependentLambdaExpr(arg: DataType) {
    def :>>[T <: Type](f: Expr[`(dt)->`[T]]): Expr[T] = f(arg)
    def <<:[T <: Type](f: Expr[`(dt)->`[T]]): Expr[T] = f(arg)
  }

  implicit class FunComp[T <: Type](f: Expr[DataType -> T]) {
    def o(g: Expr[DataType -> DataType]): Expr[DataType -> T] = fun(arg => f( g(arg) ) )
    def <<<(g: Expr[DataType -> DataType]): Expr[DataType -> T] = f o g
  }

  implicit class RevFunComp(f: Expr[DataType -> DataType]) {
    def >>>[T <: Type](g: Expr[DataType -> T]): Expr[DataType -> T] = g o f
  }

  implicit def toLiteralInt(i: Int): LiteralExpr = LiteralExpr(IntData(i))
  implicit def toLiteralFloat(f: Float): LiteralExpr = LiteralExpr(FloatData(f))
  implicit def toLiteralDouble(d: Double): LiteralExpr = LiteralExpr(DoubleData(d))
  implicit def toLiteralFloatN(v: VectorData): LiteralExpr = LiteralExpr(v)
  implicit def toNatExprNat(n: Nat): NatExpr = NatExpr(n)

  def fmapNatExpr(natExpr: DataExpr, f: Nat => Nat): NatExpr = {
    val liftedNat = Internal.natFromNatExpr(natExpr)
    val res = f(liftedNat)
    NatExpr(res)
  }

  def fmapNatExpr(natExpr1: DataExpr, natExpr2: DataExpr, f: (Nat, Nat) => Nat): NatExpr = {
    val liftedNat1 = Internal.natFromNatExpr(natExpr1)
    val liftedNat2 = Internal.natFromNatExpr(natExpr2)
    val res = f(liftedNat1, liftedNat2)
    NatExpr(res)
  }

  // this is safe as long as `f' returns a Nat value of less than `n'
  def fmapIndexExpr(indexExpr: DataExpr, f: Nat => Nat): DataExpr = {
    indexExpr.t match {
      case Some(IndexType(n)) => AsIndex(n, fmapNatExpr(indexAsNat(indexExpr), f))
      case x => throw new Exception(s"Expected ExpType(IndexType(n)) found: $x")
    }
  }

  implicit def toNatDependentLambda[T <: Type](p: Expr[T]): NatDependentLambdaExpr[T] =
    nFun(_ => p)

  implicit class ExpPhraseExtensions(e: DataExpr) {
    def _1 = Fst(e, None)
    def _2 = Snd(e, None)
  }

  private object Internal {
    def natFromIndexExpr(p: Expr[DataType]): Nat = {
      p.t match {
        case Some(IndexType(n)) =>
          p match {
            case i: IdentifierExpr => NamedVar(i.name, RangeAdd(0, n, 1))
            case ApplyExpr(fun, arg) => natFromIndexExpr(Lifting.liftFunctionExpr(fun)(arg))
            case BinOpExpr(op, lhs, rhs) => binOpToNat(op, natFromIndexExpr(lhs), natFromIndexExpr(rhs))
            case IfThenElseExpr(_, _, _) => ???
            case LiteralExpr(lit) => lit match {
              case i: IndexData => i.n
              case _ => throw new Exception("This should never happen")
            }
            case NatExpr(_) => throw new Exception("This should never happen")
            case NatDependentApplyExpr(fun, arg) => natFromIndexExpr(Lifting.liftNatDependentFunctionExpr(fun)(arg))
            case TypeDependentApplyExpr(fun, arg) => natFromIndexExpr(Lifting.liftTypeDependentFunctionExpr(fun)(arg))
            case UnaryOpExpr(op, e) => unOpToNat(op, natFromIndexExpr(e))
            case prim: PrimitiveExpr => prim match {
              //TODO can we use our knowledge of n somehow?
              case AsIndex(n, e, _) => natFromNatExpr(e)
              case _ => ???
            }
          }
        case _ => throw new Exception("This should never happen")
      }
    }

    def natFromNatExpr(p: Expr[DataType]): Nat = {
      p.t match {
        case Some(NatType) =>
          p match {
            case NatExpr(n) => n
            case i: IdentifierExpr => NamedVar(i.name, StartFromRange(0))
            case ApplyExpr(fun, arg) => natFromNatExpr(Lifting.liftFunctionExpr(fun)(arg))
            case BinOpExpr(op, lhs, rhs) => binOpToNat(op, natFromNatExpr(lhs), natFromNatExpr(rhs))
            case IfThenElseExpr(_, _, _) => ???
            case LiteralExpr(_) => throw new Exception("This should never happen")
            case NatDependentApplyExpr(fun, arg) => natFromNatExpr(Lifting.liftNatDependentFunctionExpr(fun)(arg))
            case TypeDependentApplyExpr(fun, arg) => natFromNatExpr(Lifting.liftTypeDependentFunctionExpr(fun)(arg))
            case UnaryOpExpr(op, e) => unOpToNat(op, natFromNatExpr(e))
            case prim: PrimitiveExpr => prim match {
              case IndexAsNat(e, _) => natFromIndexExpr(e)
              case _ => ???
            }
          }
        case pt => throw new Exception(s"Expected exp[nat] but found $pt.")
      }
    }

    def binOpToNat(op: Operators.Binary.Value, n1: Nat, n2: Nat): Nat = {
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

    def unOpToNat(op: Operators.Unary.Value, n: Nat): Nat = ???
  }
}
