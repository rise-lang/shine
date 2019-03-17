package idealised.DPIA.Phrases

import idealised.DPIA.Compilation.TranslationContext
import idealised.DPIA.FunctionalPrimitives.{AsIndex, AsNat}
import idealised.DPIA.Lifting.{liftFunction, liftNatDependentFunction, liftPair, liftTypeDependentFunction}
import idealised.DPIA.Semantics.OperationalSemantics
import idealised.DPIA.Semantics.OperationalSemantics.{IndexData, NatData}
import idealised.DPIA.Types._
import idealised.DPIA._
import idealised.{DPIA, SurfaceLanguage}
import idealised.SurfaceLanguage.Operators
import idealised.SurfaceLanguage.Operators.Binary._
import lift.arithmetic.{NamedVar, RangeAdd, StartFromRange}

sealed trait Phrase[T <: PhraseType] {
  final lazy val t: T = TypeOf(this)
}

final case class Identifier[T <: PhraseType](name: String, `type`: T)
  extends Phrase[T]

final case class Lambda[T1 <: PhraseType, T2 <: PhraseType](param: Identifier[T1], body: Phrase[T2])
  extends Phrase[T1 -> T2]

final case class Apply[T1 <: PhraseType, T2 <: PhraseType](fun: Phrase[T1 -> T2], arg: Phrase[T1])
  extends Phrase[T2]

final case class NatDependentLambda[T <: PhraseType](x: NatIdentifier, body: Phrase[T])
  extends Phrase[`(nat)->`[T]]

final case class TypeDependentLambda[T <: PhraseType](x: DataTypeIdentifier, body: Phrase[T])
  extends Phrase[`(dt)->`[T]]

final case class NatDependentApply[T <: PhraseType](fun: Phrase[`(nat)->`[T]], arg: Nat)
  extends Phrase[T]

final case class TypeDependentApply[T <: PhraseType](fun: Phrase[`(dt)->`[T]], arg: DataType)
  extends Phrase[T]

final case class Pair[T1 <: PhraseType, T2 <: PhraseType](fst: Phrase[T1], snd: Phrase[T2])
  extends Phrase[T1 x T2]

final case class Proj1[T1 <: PhraseType, T2 <: PhraseType](pair: Phrase[T1 x T2])
  extends Phrase[T1]

final case class Proj2[T1 <: PhraseType, T2 <: PhraseType](pair: Phrase[T1 x T2])
  extends Phrase[T2]

final case class IfThenElse[T <: PhraseType](cond: Phrase[ExpType], thenP: Phrase[T], elseP: Phrase[T])
  extends Phrase[T]

final case class UnaryOp(op: SurfaceLanguage.Operators.Unary.Value, p: Phrase[ExpType])
  extends Phrase[ExpType]

final case class BinOp(op: SurfaceLanguage.Operators.Binary.Value, lhs: Phrase[ExpType], rhs: Phrase[ExpType])
  extends Phrase[ExpType]

final case class Literal(d: OperationalSemantics.Data)
  extends Phrase[ExpType] {
  assert(!d.isInstanceOf[NatData])
  assert(!d.isInstanceOf[IndexData])
}

final case class Natural(d: Nat) extends Phrase[ExpType]

object Phrase {
  // substitutes `phrase` for `for` in `in`, i.e. in [ phrase / for ]
  def substitute[T1 <: PhraseType, T2 <: PhraseType](phrase: Phrase[T1],
                                                     `for`: Phrase[T1],
                                                     in: Phrase[T2]): Phrase[T2] = {
    object Visitor extends VisitAndRebuild.Visitor {
      override def apply[T <: PhraseType](p: Phrase[T]): Result[Phrase[T]] = {
        p match {
          case `for` => Stop(phrase.asInstanceOf[Phrase[T]])
          case Natural(n) =>
            val v = NamedVar(`for` match {
              case Identifier(name, _) => name
              case _ => ???
            })

            phrase.t match {
              case ExpType(NatType) =>
                  Stop(Natural(Nat.substitute(
                    Internal.NatFromNatExpr(phrase.asInstanceOf[Phrase[ExpType]]), v, n)).asInstanceOf[Phrase[T]])
              case ExpType(IndexType(_)) =>
                  Stop(Natural(Nat.substitute(
                    Internal.NatFromIndexExpr(phrase.asInstanceOf[Phrase[ExpType]]), v, n)).asInstanceOf[Phrase[T]])
              case _ => Continue(p, this)
            }
          case _ => Continue(p, this)
        }
      }
    }

    VisitAndRebuild(in, Visitor)
  }

  def substitute[T2 <: PhraseType](substitutionMap: Map[Phrase[_], Phrase[_]],
                                   in: Phrase[T2]): Phrase[T2] = {

    object Visitor extends VisitAndRebuild.Visitor {
      override def apply[T <: PhraseType](p: Phrase[T]): Result[Phrase[T]] = {
        if (substitutionMap.isDefinedAt(p)) {
          Stop(substitutionMap(p).asInstanceOf[Phrase[T]])
        } else {
          Continue(p, this)
        }
      }
    }

    VisitAndRebuild(in, Visitor)
  }


  object Internal {
    def NatFromIndexExpr(p: Phrase[ExpType]): Nat = {
      p.t match {
        case ExpType(IndexType(n)) =>
          p match {
            case i:Identifier[ExpType] => NamedVar(i.name, RangeAdd(0, n, 1))
            case Apply(fun, arg) => NatFromIndexExpr(liftFunction(fun)(arg))
            case BinOp(op, lhs, rhs) => binOpToNat(op, NatFromIndexExpr(lhs), NatFromIndexExpr(rhs))
            case DPIA.Phrases.IfThenElse(_, _, _) => ???
            case Literal(lit) => lit match {
              case i: IndexData => i.n
              case _ => throw new Exception("This should never happen")
            }
            case Natural(_) => throw new Exception("This should never happen")
            case NatDependentApply(fun, arg) => NatFromIndexExpr(liftNatDependentFunction(fun)(arg))
            case Proj1(pair) => NatFromIndexExpr(liftPair(pair)._1)
            case Proj2(pair) => NatFromIndexExpr(liftPair(pair)._2)
            case TypeDependentApply(fun, arg) => NatFromIndexExpr(liftTypeDependentFunction(fun)(arg))
            case UnaryOp(op, e) => unOpToNat(op, NatFromIndexExpr(e))
            case prim: ExpPrimitive => prim match {
              //TODO can we use our knowledge of n somehow?
              case AsIndex(n, e) => NatFromNatExpr(e)
              case _ => ???
            }
          }
        case _ => throw new Exception("This should never happen")
      }
    }

    def NatFromNatExpr(p: Phrase[ExpType]): Nat = {
      p.t match {
        case ExpType(NatType) =>
          p match {
            case Natural(n) => n
            case i: Identifier[ExpType] => NamedVar(i.name, StartFromRange(0))
            case Apply(fun, arg) => NatFromNatExpr(liftFunction(fun)(arg))
            case BinOp(op, lhs, rhs) => binOpToNat(op, NatFromNatExpr(lhs), NatFromNatExpr(rhs))
            case DPIA.Phrases.IfThenElse(_, _, _) => ???
            case Literal(_) => throw new Exception("This should never happen")
            case NatDependentApply(fun, arg) => NatFromNatExpr(liftNatDependentFunction(fun)(arg))
            case Proj1(pair) => NatFromNatExpr(liftPair(pair)._1)
            case Proj2(pair) => NatFromNatExpr(liftPair(pair)._2)
            case TypeDependentApply(fun, arg) => NatFromNatExpr(liftTypeDependentFunction(fun)(arg))
            case UnaryOp(op, e) => unOpToNat(op, NatFromNatExpr(e))
            case prim: ExpPrimitive => prim match {
              //TODO can we use our knowledge of n somehow?
              case AsNat(n, e) => NatFromIndexExpr(e)
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
}

sealed trait Primitive[T <: PhraseType] extends Phrase[T] {
  def `type`: T

  def prettyPrint: String

  def xmlPrinter: xml.Elem

  def visitAndRebuild(f: VisitAndRebuild.Visitor): Phrase[T]
}

trait ExpPrimitive extends Primitive[ExpType] {
  def eval(s: OperationalSemantics.Store): OperationalSemantics.Data

  def acceptorTranslation(A: Phrase[AccType])
                         (implicit context: TranslationContext): Phrase[CommandType]

  def mapAcceptorTranslation(f: Phrase[ExpType -> ExpType],
                             A: Phrase[AccType])
                            (implicit context: TranslationContext): Phrase[CommandType]

  def continuationTranslation(C: Phrase[ExpType -> CommandType])
                             (implicit context: TranslationContext): Phrase[CommandType]
}

trait AccPrimitive extends Primitive[AccType] {
  def eval(s: OperationalSemantics.Store): OperationalSemantics.AccIdentifier
}

trait CommandPrimitive extends Primitive[CommandType] {
  def eval(s: OperationalSemantics.Store): OperationalSemantics.Store
}