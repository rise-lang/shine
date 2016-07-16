package Core

import Compiling.SubstituteImplementations

sealed trait Phrase[T <: PhraseType] {
  lazy val t: T = `type`

  def `type`: T = TypeOf(this)

  def typeCheck(): Unit = TypeChecker(this)
}

final case class IdentPhrase[T <: PhraseType](name: String, override val `type`: T)
  extends Phrase[T]

final case class LambdaPhrase[T1 <: PhraseType, T2 <: PhraseType](param: IdentPhrase[T1], body: Phrase[T2])
  extends Phrase[T1 -> T2]

final case class ApplyPhrase[T1 <: PhraseType, T2 <: PhraseType](fun: Phrase[T1 -> T2], arg: Phrase[T1])
  extends Phrase[T2]

final case class NatDependentLambdaPhrase[T <: PhraseType](x: NatIdentifier, body: Phrase[T])
  extends Phrase[`(nat)->`[T]]

final case class NatDependentApplyPhrase[T <: PhraseType](fun: Phrase[`(nat)->`[T]], arg: Nat)
  extends Phrase[T]

final case class PairPhrase[T1 <: PhraseType, T2 <: PhraseType](fst: Phrase[T1], snd: Phrase[T2])
  extends Phrase[T1 x T2]

final case class Proj1Phrase[T1 <: PhraseType, T2 <: PhraseType](pair: Phrase[T1 x T2])
  extends Phrase[T1]

final case class Proj2Phrase[T1 <: PhraseType, T2 <: PhraseType](pair: Phrase[T1 x T2])
  extends Phrase[T2]

final case class IfThenElsePhrase[T <: PhraseType](cond: Phrase[ExpType], thenP: Phrase[T], elseP: Phrase[T])
  extends Phrase[T]

final case class UnaryOpPhrase(op: UnaryOpPhrase.Op.Value, p: Phrase[ExpType])
  extends Phrase[ExpType]

object UnaryOpPhrase {

  object Op extends Enumeration {
    val NEG = Value("-")
  }

}

final case class BinOpPhrase(op: BinOpPhrase.Op.Value, lhs: Phrase[ExpType], rhs: Phrase[ExpType])
  extends Phrase[ExpType]

object BinOpPhrase {

  object Op extends Enumeration {
    val ADD = Value("+")
    val SUB = Value("-")
    val MUL = Value("*")
    val DIV = Value("/")
    val MOD = Value("%")
    val GT = Value(">")
    val LT = Value("<")
  }

}

final case class LiteralPhrase(d: OperationalSemantics.Data, dt: DataType)
  extends Phrase[ExpType] {
  override lazy val t = ExpType(dt)
}

object Phrase {
  // substitutes `phrase` for `for` in `in`, i.e. in [ phrase / for ]
  def substitute[T1 <: PhraseType, T2 <: PhraseType](phrase: Phrase[T1],
                                                     `for`: Phrase[T1],
                                                     in: Phrase[T2]): Phrase[T2] = {
    object fun extends VisitAndRebuild.fun {
      override def pre[T <: PhraseType](p: Phrase[T]): Result[Phrase[T]] = {
        if (`for` == p) {
          Stop(phrase.asInstanceOf[Phrase[T]])
        } else {
          Continue(p, this)
        }
      }
    }

    VisitAndRebuild(in, fun)
  }

  def substitute[T2 <: PhraseType](map: Map[Phrase[_], Phrase[_]],
                                   in: Phrase[T2]): Phrase[T2] = {

    object fun extends VisitAndRebuild.fun {
      override def pre[T <: PhraseType](p: Phrase[T]): Result[Phrase[T]] = {
        if (map.isDefinedAt(p)) {
          Stop(map(p).asInstanceOf[Phrase[T]])
        } else {
          Continue(p, this)
        }
      }
    }

    VisitAndRebuild(in, fun)
  }
}

trait TypeInferable {
  def inferTypes: TypeInferable
}

sealed trait Combinator[T <: PhraseType] extends Phrase[T] {
  override def `type`: T

  override def typeCheck(): Unit

  def prettyPrint: String

  def xmlPrinter: xml.Elem

  def visitAndRebuild(f: VisitAndRebuild.fun): Phrase[T]
}

sealed trait ExpCombinator extends Combinator[ExpType] with TypeInferable {
  def eval(s: OperationalSemantics.Store): OperationalSemantics.Data
}

sealed trait AccCombinator extends Combinator[AccType] {
  def eval(s: OperationalSemantics.Store): OperationalSemantics.AccIdentifier
}

sealed trait CommandCombinator extends Combinator[CommandType] {
  override val `type` = comm

  def eval(s: OperationalSemantics.Store): OperationalSemantics.Store
}

abstract class HighLevelCombinator extends ExpCombinator {
  def rewriteToImperativeAcc(A: Phrase[AccType]): Phrase[CommandType]

  def rewriteToImperativeExp(C: Phrase[ExpType -> CommandType]): Phrase[CommandType]
}

abstract class MidLevelCombinator extends CommandCombinator {
  def substituteImpl(env: SubstituteImplementations.Environment): Phrase[CommandType]
}

abstract class LowLevelExpCombinator extends ExpCombinator {
  def rewriteToImperativeAcc(A: Phrase[AccType]): Phrase[CommandType]

  def rewriteToImperativeExp(C: Phrase[ExpType -> CommandType]): Phrase[CommandType]
}

abstract class LowLevelAccCombinator extends AccCombinator

abstract class LowLevelCommCombinator extends CommandCombinator
