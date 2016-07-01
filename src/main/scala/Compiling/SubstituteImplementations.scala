package Compiling

import Core._
import LowLevelCombinators.New

import scala.collection.immutable


object SubstituteImplementations {

  case class Environment(addressSpace: immutable.Map[String, AddressSpace])

  object Environment {
    def apply(): Environment = Environment(immutable.Map[String, AddressSpace]())
  }

  def apply(phrase: Phrase[CommandType], env: Environment): Phrase[CommandType] = {

    case class fun(env: Environment) extends VisitAndRebuild.fun {
      override def apply[T <: PhraseType](p: Phrase[T]): Result[Phrase[T]] = {
        p match {
          case ml: MidLevelCombinator =>
            Continue(ml.substituteImpl(env).asInstanceOf[Phrase[T]], this)

          // add the addressSpace of every new variable to the environment
          case New(dt, addressSpace, f) =>
            val p = f match {
              case LambdaPhrase(param, _) => param
              case _ => throw new Exception("This should not happen")
            }
            Continue(New(dt, addressSpace, f).asInstanceOf[Phrase[T]],
              fun(env.copy(env.addressSpace.updated(p.name, addressSpace))))

          case _ => Continue(p, this)
        }
      }
    }

    VisitAndRebuild(phrase, fun(env))
  }

}
