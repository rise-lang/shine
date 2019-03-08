package idealised.DPIA.Phrases

import idealised.DPIA.Compilation.TranslationContext
import idealised.DPIA.Semantics.OperationalSemantics
import idealised.DPIA.Types._
import idealised.DPIA._
import idealised.SurfaceLanguage

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
  extends Phrase[ExpType]

final case class NatArith(d: OperationalSemantics.NatValue)
  extends Phrase[ExpType]

object Phrase {
  // substitutes `phrase` for `for` in `in`, i.e. in [ phrase / for ]
  def substitute[T1 <: PhraseType, T2 <: PhraseType](phrase: Phrase[T1],
                                                     `for`: Phrase[T1],
                                                     in: Phrase[T2]): Phrase[T2] = {
    object Visitor extends VisitAndRebuild.Visitor {
      override def apply[T <: PhraseType](p: Phrase[T]): Result[Phrase[T]] = {
        if (`for` == p) {
          Stop(phrase.asInstanceOf[Phrase[T]])
        } else {
          Continue(p, this)
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

  /**
   * Return all the Nat parameters used in the (curried) function that is expressed by the phrase.
   */
  def natParamsOfUncurriedFunction[T <: PhraseType](phrase: Phrase[T]): List[NatIdentifier] = {
    def recurseWithResultList[T <: PhraseType](phrase: Phrase[T],
                                               params: List[NatIdentifier]): List[NatIdentifier] = {
      phrase match {
        case NatDependentLambda(x, body) => recurseWithResultList(body, x :: params)
        case Lambda(x, body) => recurseWithResultList(body, params)
        case _ => params.reverse
      }
    }

    recurseWithResultList(phrase, Nil)
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

  def continuationTranslation(C: Phrase[ExpType -> CommandType])
                             (implicit context: TranslationContext): Phrase[CommandType]
}

trait AccPrimitive extends Primitive[AccType] {
  def eval(s: OperationalSemantics.Store): OperationalSemantics.AccIdentifier
}

trait CommandPrimitive extends Primitive[CommandType] {
  def eval(s: OperationalSemantics.Store): OperationalSemantics.Store
}