package idealised.DPIA.Phrases

import idealised.DPIA.Compilation.TranslationContext
import idealised.DPIA.FunctionalPrimitives.{AsIndex, IndexAsNat}
import idealised.DPIA.Lifting.{liftFunction, liftNatDependentFunction, liftPair, liftTypeDependentFunction}
import idealised.DPIA.Semantics.OperationalSemantics
import idealised.DPIA.Semantics.OperationalSemantics.{IndexData, NatData}
import idealised.DPIA.Types._
import idealised.DPIA._
import idealised.{DPIA, SurfaceLanguage}
import idealised.SurfaceLanguage.Operators
import lift.arithmetic.{NamedVar, RangeAdd, StartFromRange}

import scala.language.{postfixOps, reflectiveCalls}

sealed trait Phrase[T <: PhraseType] {
  val t: T // TODO? perform type checking at the same time
}

final case class Identifier[T <: PhraseType](name: String, `type`: T)
  extends Phrase[T] {

  override val t: T = `type`
  override def toString: String = s"($name : ${`type`})"
}

final case class Lambda[T1 <: PhraseType, T2 <: PhraseType](param: Identifier[T1], body: Phrase[T2])
  extends Phrase[T1 -> T2] {

  override val t: T1 -> T2 = param.t -> body.t
  override def toString: String = s"λ$param. $body"
}

final case class Apply[T1 <: PhraseType, T2 <: PhraseType](fun: Phrase[T1 -> T2], arg: Phrase[T1])
  extends Phrase[T2] {

  TypeCheck.check(fun.t.inT, arg.t) // FIXME: redundant with type checking
  override val t: T2 = fun.t.outT
  override def toString: String = s"($fun $arg)"
}

final case class NatDependentLambda[T <: PhraseType](x: NatIdentifier, body: Phrase[T])
  extends Phrase[`(nat)->`[T]] {

  override val t: `(nat)->`[T] = (x: NatIdentifier) -> body.t
  override def toString: String = s"Λ($x : nat). $body"
}

final case class TypeDependentLambda[T <: PhraseType](x: DataTypeIdentifier, body: Phrase[T])
  extends Phrase[`(dt)->`[T]] {

  override val t: `(dt)->`[T] = (x: DataTypeIdentifier) -> body.t
  override def toString: String = s"Λ($x : data). $body"
}

final case class NatDependentApply[T <: PhraseType](fun: Phrase[`(nat)->`[T]], arg: Nat)
  extends Phrase[T] {

  override val t: T = (fun.t.t `[` arg `/` fun.t.n `]`).asInstanceOf[T]
  override def toString: String = s"($fun $arg)"
}

final case class TypeDependentApply[T <: PhraseType](fun: Phrase[`(dt)->`[T]], arg: DataType)
  extends Phrase[T] {

  override val t: T = (fun.t.t `[` arg `/` fun.t.dt `]`).asInstanceOf[T]
  override def toString: String = s"($fun $arg)"
}

final case class Pair[T1 <: PhraseType, T2 <: PhraseType](fst: Phrase[T1], snd: Phrase[T2])
  extends Phrase[T1 x T2] {

  override val t: x[T1, T2] = fst.t x snd.t
}

final case class Proj1[T1 <: PhraseType, T2 <: PhraseType](pair: Phrase[T1 x T2])
  extends Phrase[T1] {

  override val t: T1 = pair.t.t1
}

final case class Proj2[T1 <: PhraseType, T2 <: PhraseType](pair: Phrase[T1 x T2])
  extends Phrase[T2] {

  override val t: T2 = pair.t.t2
}

final case class IfThenElse[T <: PhraseType](cond: Phrase[ExpType], thenP: Phrase[T], elseP: Phrase[T])
  extends Phrase[T] {

  // FIXME: redundant with type checking
  TypeCheck.check(thenP.t, elseP.t)
  override val t: T = thenP.t
}

final case class UnaryOp(op: SurfaceLanguage.Operators.Unary.Value, p: Phrase[ExpType])
  extends Phrase[ExpType] {

  override val t: ExpType = p.t
}

final case class BinOp(op: SurfaceLanguage.Operators.Binary.Value, lhs: Phrase[ExpType], rhs: Phrase[ExpType])
  extends Phrase[ExpType] {

  override val t: ExpType =
    op match {
      case Operators.Binary.GT |
           Operators.Binary.LT |
           Operators.Binary.EQ => exp"[$bool]"
      case _ => (lhs.t.dataType, rhs.t.dataType) match {
        case (t1, t2) if t1 == t2 => ExpType(t1)
        // TODO: Think about this more thoroughly ...
        case (IndexType(n), `int`) =>
          ExpType(IndexType(n))
        //              ExpType(IndexType(OperationalSemantics.toScalaOp(op)(n, OperationalSemantics.evalIntExp(rhs))))
        case (`int`, IndexType(n)) =>
          ExpType(IndexType(n))
        //              ExpType(IndexType(OperationalSemantics.toScalaOp(op)(OperationalSemantics.evalIntExp(lhs), n)))
        case (IndexType(n), IndexType(_)) =>
          //              ExpType(IndexType(OperationalSemantics.toScalaOp(op)(n, m)))
          ExpType(IndexType(n))

        case (lhsT, rhsT) =>
          throw new TypeException(s"Failed type checking: found" +
            s"`$lhsT' and `$rhsT', but expected them to match.")
      }
    }
}

final case class Literal(d: OperationalSemantics.Data)
  extends Phrase[ExpType] {

  assert(!d.isInstanceOf[NatData])
  assert(!d.isInstanceOf[IndexData])

  override val t: ExpType = ExpType(d.dataType)
}

final case class Natural(d: Nat) extends Phrase[ExpType] {
  override val t: ExpType = ExpType(NatType)
}

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
            case Apply(fun, arg) => NatFromIndexExpr(liftFunction(fun).reducing(arg))
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
            case Apply(fun, arg) => NatFromNatExpr(liftFunction(fun).reducing(arg))
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
              case IndexAsNat(n, e) => NatFromIndexExpr(e)
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