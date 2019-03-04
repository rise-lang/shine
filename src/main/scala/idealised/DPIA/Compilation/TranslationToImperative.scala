package idealised.DPIA.Compilation

import idealised.DPIA.DSL._
import idealised.DPIA.Phrases._
import idealised.DPIA.Types._
import idealised.DPIA._
import idealised.SurfaceLanguage

import scala.language.reflectiveCalls

object TranslationToImperative {
  sealed trait AccExt {
    def get: Phrase[AccType] =
      throw new Exception("Expected basic acceptor")
  }
  final case class RawAcceptor(A: Phrase[AccType]) extends AccExt {
    override def get = A
  }
  final case class MapAcceptor(A: Phrase[AccType],
                               elem: Phrase[ExpType -> ExpType])
    extends AccExt
  /*
  final case class RecordAcceptor(A: Phrase[AccType],
                                  fst: Phrase[ExpType -> ExpType],
                                  snd: Phrase[ExpType -> ExpType])

    extends AccExt
    */
  object AccExt {
    def apply(A: Phrase[AccType]): AccExt = {
      RawAcceptor(A)
      /*
      A.t.dataType match {
        case _: BasicType => BasicAcceptor(A)
        case ArrayType(_, dt) => ArrayAcceptor(A, fun(exp"[$dt]")(x => x))
        case RecordType(fst, snd) => RecordAcceptor(A,
          fun(exp"[$fst]")(x => x), fun(exp"[$snd]")(x => x))
        case _ => ???
      }
    */
    }
  }

  def apply(p: Phrase[ExpType])
           (implicit context: TranslationContext): Phrase[CommandType] = {
    val outT = p.t
    val out = identifier("output", AccType(outT.dataType))
    acc(p)(AccExt(out))
  }

  def acc(E: Phrase[ExpType])
         (A: AccExt)
         (implicit context: TranslationContext): Phrase[CommandType] = {
    E match {
      case x: Identifier[ExpType] => A.get :=|x.t.dataType| x

      case c: Literal => A.get :=|c.t.dataType| c

      case u@UnaryOp(op, e) =>
        con(e)(λ(u.t)(x =>
          A.get :=|u.t.dataType| UnaryOp(op, x)
        ))

      case b@BinOp(op, e1, e2) =>
        con(e1)(λ(b.t)(x =>
          con(e2)(λ(b.t)(y =>
            A.get :=|b.t.dataType| BinOp(op, x, y)
          ))
        ))

      case ep: ExpPrimitive => A match {
        case RawAcceptor(a) => ep.acceptorTranslation(a)
        case MapAcceptor(a, elem) => ep.mapAcceptorTranslation(a, elem)
          /*
        case RecordAcceptor(a, fst, snd) =>
          ep.recordAcceptorTranslation(a, fst, snd)
          */
      }

      // on the fly beta-reduction
      case Apply(fun, arg) => acc(Lifting.liftFunction(fun)(arg))(A)
      case NatDependentApply(fun, arg) => acc(Lifting.liftNatDependentFunction(fun)(arg))(A)
      case TypeDependentApply(fun, arg) => acc(Lifting.liftTypeDependentFunction(fun)(arg))(A)

      case IfThenElse(cond, thenP, elseP) =>
        con(cond)(λ(cond.t) { x =>
          `if` (x) `then` acc(thenP)(A) `else` acc(elseP)(A)
        })

      case Proj1(_) => throw new Exception("This should never happen")
      case Proj2(_) => throw new Exception("This should never happen")
    }
  }

  def con(E: Phrase[ExpType])
         (C: Phrase[ExpType -> CommandType])
         (implicit context: TranslationContext): Phrase[CommandType] = {
    E match {
      case x: Identifier[ExpType] => C(x)

      case c: Literal => C(c)

      case u@UnaryOp(op, e) =>
        con(e)(λ(u.t)(x =>
          C(UnaryOp(op, x))
        ))

      case b@BinOp(op, e1, e2) =>
        con(e1)(λ(b.t)(x =>
          con(e2)(λ(b.t)(y =>
            C(BinOp(op, x, y))
          ))
        ))

      case ep: ExpPrimitive => ep.continuationTranslation(C)

      // on the fly beta-reduction
      case Apply(fun, arg) => con(Lifting.liftFunction(fun)(arg))(C)
      case NatDependentApply(fun, arg) => con(Lifting.liftNatDependentFunction(fun)(arg))(C)
      case TypeDependentApply(fun, arg) => con(Lifting.liftTypeDependentFunction(fun)(arg))(C)

      case IfThenElse(cond, thenP, elseP) =>
        con(cond)(λ(cond.t) { x =>
          `if`(x) `then` con(thenP)(C) `else` con(elseP)(C)
        })

      case Proj1(_) => throw new Exception("This should never happen")
      case Proj2(_) => throw new Exception("This should never happen")
    }
  }

}
