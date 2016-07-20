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

    case class fun(env: Environment) extends VisitAndRebuild.Visitor {
      override def apply[T <: PhraseType](p: Phrase[T]): Result[Phrase[T]] = {
        p match {
          case ml: MidLevelCombinator =>
            Continue(ml.substituteImpl(env).asInstanceOf[Phrase[T]], this)

          // add the addressSpace of every new variable to the environment
          case New(dt, addressSpace, f) =>
            val param = f match {
              case LambdaPhrase(param_, _) => param_
              case _ => throw new Exception("This should not happen")
            }
            Continue(p,
              fun(env.copy(env.addressSpace.updated(param.name, addressSpace))))

          case _ => Continue(p, this)
        }
      }
    }

    VisitAndRebuild(phrase, fun(env))
  }

}
