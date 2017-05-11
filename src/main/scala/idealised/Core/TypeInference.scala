package idealised.Core

import scala.language.postfixOps
import scala.language.reflectiveCalls

class TypeInferenceException(msg: String) extends TypeException(msg)

object TypeInference {

  def error(found: String, expected: String): Nothing = {
    throw new TypeInferenceException(s"Found $found but expected $expected")
  }

  def error(msg: String): Nothing = {
    throw new TypeInferenceException(msg)
  }

  def check[T <: PhraseType](phrase: Phrase[T]): Unit = {
    if (phrase.t == null) {
      error(s"Found phrase $phrase without proper type")
    }
  }

  case class typeInference() extends VisitAndRebuild.Visitor {
    override def apply[T <: PhraseType](phrase: Phrase[T]): Result[Phrase[T]] = {
      phrase match {
        case x: Identifier[T] =>
          x.t match {
            case null => error("Found IdentPhrase without proper type")
            case _: PhraseType => Stop(x)
          }
        case i: TypeInferable[T]@unchecked =>
          Stop(i.inferTypes)

        case _ => Continue(phrase, this)
      }
    }
  }

  case class setParamAndTypeInference(t: PhraseType) extends VisitAndRebuild.Visitor {
    override def apply[T <: PhraseType](phrase: Phrase[T]): Result[Phrase[T]] = {
      phrase match {
        case l@Lambda(x, p) =>
          val newX = Identifier(newName(), t)
          Continue(l `[` newX  `/` x `]`, typeInference())
        case _ => throw new Exception("This should not happen")
      }
    }
  }

  case class setParamsAndTypeInference(t1: PhraseType, t2: PhraseType) extends VisitAndRebuild.Visitor {
    override def apply[T <: PhraseType](phrase: Phrase[T]): Result[Phrase[T]] = {
      phrase match {
        case Lambda(x, p) =>
          val newX = Identifier(newName(), t1)
          val b1 = p `[` newX `/` x `]`
          val newBody = VisitAndRebuild(b1, setParamAndTypeInference(t2))
          val newL = Lambda(newX, newBody)
          Continue(newL.asInstanceOf[Phrase[T]], typeInference())
        case _ => throw new Exception("This should not happen")
      }
    }
  }

  def apply[T <: PhraseType](phrase: Phrase[T]): Phrase[T] = {
    VisitAndRebuild(phrase, typeInference())
  }

  def setParamAndInferType[T1 <: PhraseType, T2 <: PhraseType](phrase: Phrase[T1 -> T2], t: T1): Phrase[T1 -> T2] = {
    VisitAndRebuild(phrase, setParamAndTypeInference(t))
  }

  def setParamsAndInferTypes[T1 <: PhraseType, T2 <: PhraseType, T3 <: PhraseType](phrase: Phrase[T1 -> (T2 -> T3)], t1: T1, t2: T2): Phrase[T1 -> (T2 -> T3)] = {
    VisitAndRebuild(phrase, setParamsAndTypeInference(t1, t2))
  }

}
