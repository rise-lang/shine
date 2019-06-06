package idealised.SurfaceLanguage

import idealised.SurfaceLanguage.Primitives._
import idealised.SurfaceLanguage.Semantics._
import idealised.SurfaceLanguage.Types._
import lift.arithmetic.{NamedVar, RangeAdd, StartFromRange}

import scala.language.implicitConversions

package object DSL {

  implicit class BinOps(lhs: Expr) {
    def +(rhs: Expr) = BinOpExpr(Operators.Binary.ADD, lhs, rhs)
    def -(rhs: Expr) = BinOpExpr(Operators.Binary.SUB, lhs, rhs)
    def *(rhs: Expr) = BinOpExpr(Operators.Binary.MUL, lhs, rhs)
    def /(rhs: Expr) = BinOpExpr(Operators.Binary.DIV, lhs, rhs)
    def %(rhs: Expr) = BinOpExpr(Operators.Binary.MOD, lhs, rhs)
    def >(rhs: Expr) = BinOpExpr(Operators.Binary.GT, lhs, rhs)
    def <(rhs: Expr) = BinOpExpr(Operators.Binary.LT, lhs, rhs)
    def =:=(rhs: Expr) = BinOpExpr(Operators.Binary.EQ, lhs, rhs)
    def unary_- = UnaryOpExpr(Operators.Unary.NEG, lhs)
  }

  implicit class FunCall(f: Expr) {
    def apply(arg: Expr): Expr = Lifting.liftFunctionExpr(f)(arg)
    def $(arg: Expr): Expr = apply(arg)

    def apply(arg: Nat): Expr = Lifting.liftNatDependentFunctionExpr(f)(arg)
    def $(arg: Nat): Expr = apply(arg)

    def apply(arg: DataType): Expr = Lifting.liftTypeDependentFunctionExpr(f)(arg)
    def $(arg: DataType): Expr = apply(arg)
  }

  implicit class FunCallExpr(arg: Expr) {
    def :>>(f: Expr): Expr = f(arg)
    def <<:(f: Expr): Expr = f(arg)
  }

  implicit class FunCallExprPair(args: (Expr, Expr)) {
    def :>>(z: (Expr, Expr) => Zip): Zip = z(args._1, args._2)
    def <<:(z: (Expr, Expr) => Zip): Zip = z(args._1, args._2)
  }

  implicit class FunComp(f: Expr) {
    def o(g: Expr): Expr = fun(arg => f( g(arg) ) )
    def <<<(g: Expr): Expr = f o g
    def >>>(g: Expr): Expr = g o f
  }

  implicit def l(i: Int): LiteralExpr = LiteralExpr(IntData(i))
  implicit def l(f: Float): LiteralExpr = LiteralExpr(FloatData(f))
  implicit def l(d: Double): LiteralExpr = LiteralExpr(DoubleData(d))
  implicit def l(v: VectorData): LiteralExpr = LiteralExpr(v)

  implicit def toNatExprNat(n: Nat): NatExpr = NatExpr(n)

  def mapNatExpr(natExpr: Expr, f: Nat => Nat): NatExpr = {
    val liftedNat: Nat = Internal.natFromNatExpr(natExpr)
    val res = f(liftedNat)
    NatExpr(res)
  }

  def mapNatExpr(natExpr1: Expr, natExpr2: Expr, f: (Nat, Nat) => Nat): NatExpr = {
    val liftedNat1 = Internal.natFromNatExpr(natExpr1)
    val liftedNat2 = Internal.natFromNatExpr(natExpr2)
    val res = f(liftedNat1, liftedNat2)
    NatExpr(res)
  }

  // this is safe as long as `f' returns a Nat value of less than `n'
  def mapIndexExpr(indexExpr: Expr, f: Nat => Nat): Expr = {
    indexExpr.t match {
      case Some(IndexType(n)) => AsIndex(n, mapNatExpr(indexAsNat(indexExpr), f))
      case x => throw new Exception(s"Expected ExpType(IndexType(n)) found: $x")
    }
  }

  implicit class ExpPhraseExtensions(e: Expr) {
    def _1 = Fst(e, None)
    def _2 = Snd(e, None)
  }

  private object Internal {
    def natFromIndexExpr(p: Expr): Nat = {
      p.t match {
        case Some(IndexType(n)) =>
          p match {
            case i: IdentifierExpr => NamedVar(i.name, RangeAdd(0, n, 1))
            case ApplyExpr(fun, arg, _) => natFromIndexExpr(Lifting.liftFunctionExpr(fun)(arg))
            case BinOpExpr(op, lhs, rhs, _) => binOpToNat(op, natFromIndexExpr(lhs), natFromIndexExpr(rhs))
            case IfThenElseExpr(_, _, _, _) => ???
            case LiteralExpr(lit,_ ) => lit match {
              case i: IndexData => i.n
              case _ => throw new Exception("This should never happen")
            }
            case NatExpr(_) => throw new Exception("This should never happen")
            case NatDependentApplyExpr(fun, arg, _) => natFromIndexExpr(Lifting.liftNatDependentFunctionExpr(fun)(arg))
            case TypeDependentApplyExpr(fun, arg, _) => natFromIndexExpr(Lifting.liftTypeDependentFunctionExpr(fun)(arg))
            case UnaryOpExpr(op, e, _) => unOpToNat(op, natFromIndexExpr(e))
            case prim: PrimitiveExpr => prim match {
              //TODO can we use our knowledge of n somehow?
              case AsIndex(n, e, _) => natFromNatExpr(e)
              case _ => ???
            }
          }
        case _ => throw new Exception("This should never happen")
      }
    }

    // FIXME: this is matching on types before type inference
    def natFromNatExpr(p: Expr): Nat = {
      p.t match {
        case Some(NatType) =>
          p match {
            case NatExpr(n) => n
            case i: IdentifierExpr => NamedVar(i.name, StartFromRange(0))
            case ApplyExpr(fun, arg, _) => natFromNatExpr(Lifting.liftFunctionExpr(fun)(arg))
            case BinOpExpr(op, lhs, rhs, _) => binOpToNat(op, natFromNatExpr(lhs), natFromNatExpr(rhs))
            case IfThenElseExpr(_, _, _, _) => ???
            case LiteralExpr(_, _) => throw new Exception("This should never happen")
            case NatDependentApplyExpr(fun, arg, _) => natFromNatExpr(Lifting.liftNatDependentFunctionExpr(fun)(arg))
            case TypeDependentApplyExpr(fun, arg, _) => natFromNatExpr(Lifting.liftTypeDependentFunctionExpr(fun)(arg))
            case UnaryOpExpr(op, e, _) => unOpToNat(op, natFromNatExpr(e))
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
