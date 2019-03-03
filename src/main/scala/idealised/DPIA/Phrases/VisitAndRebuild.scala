package idealised.DPIA.Phrases

import idealised.DPIA.Semantics.OperationalSemantics.IndexData
import idealised.DPIA.Types._
import idealised.DPIA._

object VisitAndRebuild {

  class Visitor {
    def apply[T <: PhraseType](p: Phrase[T]): Result[Phrase[T]] = Continue(p, this)
    def apply(ae: Nat): Nat = ae
    def apply(ft:NatNatFunction):NatNatFunction = NatNatFunction(ft.x, apply(ft.body))
    def apply(ft:NatDataTypeFunction):NatDataTypeFunction = NatDataTypeFunction(ft.x, apply[DataType](ft.body))
    def apply[T <: DataType](dt: T): T = dt

    abstract class Result[+T]
    case class Stop[T <: PhraseType](p: Phrase[T]) extends Result[Phrase[T]]
    case class Continue[T <: PhraseType](p: Phrase[T], v: Visitor) extends Result[Phrase[T]]
  }

  def apply[T <: PhraseType](phrase: Phrase[T], v: Visitor): Phrase[T] = {
    v(phrase) match {
      case r: v.Stop[T]@unchecked => r.p
      case c: v.Continue[T]@unchecked =>
        val v = c.v
        (c.p match {
          case i: Identifier[T] =>
            Identifier(i.name, visitAndRebuild(i.t, v))

          case Lambda(x, p) =>
            apply(x, v) match {
              case newX: Identifier[_] => Lambda(newX, apply(p, v))
              case _ => throw new Exception("This should not happen")
            }

          case Apply(p, q) =>
            Apply(apply(p, v), apply(q, v))

          case NatDependentLambda(a, p) =>
            NatDependentLambda(a, apply(p, v))

          case TypeDependentLambda(dt, p) =>
            TypeDependentLambda(dt, apply(p, v))

          case NatDependentApply(p, ae) =>
            NatDependentApply(apply(p, v), ae)

          case TypeDependentApply(p, dt) =>
            TypeDependentApply(apply(p, v), dt)

          case Pair(p, q) => Pair(apply(p, v), apply(q, v))

          case Proj1(p) => Proj1(apply(p, v))

          case Proj2(p) => Proj2(apply(p, v))

          case IfThenElse(cond, thenP, elseP) =>
            IfThenElse(apply(cond, v), apply(thenP, v), apply(elseP, v))

          case Literal(d) => d match {
            case IndexData(i, t) => Literal(IndexData(v(i), v(t)))
            case _ => Literal(d)
          }

          case UnaryOp(op, x) => UnaryOp(op, apply(x, v))

          case BinOp(op, lhs, rhs) => BinOp(op, apply(lhs, v), apply(rhs, v))

          case c: Primitive[T] => c.visitAndRebuild(v)
        }).asInstanceOf[Phrase[T]]
    }
  }

  private def visitAndRebuild(phraseType: PhraseType, v: Visitor): PhraseType = phraseType match {
    case ExpType(dt) => ExpType(v(dt))
    case AccType(dt) => AccType(v(dt))
    case CommandType() => CommandType()
    case PairType(t1, t2) => PairType(visitAndRebuild(t1, v), visitAndRebuild(t2, v))
    case FunctionType(inT, outT) => FunctionType(visitAndRebuild(inT, v), visitAndRebuild(outT, v))
    case PassiveFunctionType(inT, outT) => PassiveFunctionType(visitAndRebuild(inT, v), visitAndRebuild(outT, v))
    case NatDependentFunctionType(x, t) => NatDependentFunctionType(x, visitAndRebuild(t, v))
    case TypeDependentFunctionType(x, t) => TypeDependentFunctionType(x, visitAndRebuild(t, v))
  }

}
