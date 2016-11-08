package idealised.Compiling

import idealised.Core._
import idealised.LowLevelCombinators._

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

  def getAddressSpace(phrase: Phrase[AccType], env: Environment): Option[AddressSpace] = {
    phrase match {
      case IdentPhrase(name, _) => Some(env.addressSpace(name))

      case p: Proj1Phrase[AccType, _] => getAddressSpace(Lift.liftPair(p.pair)._1, env)
      case p: Proj2Phrase[_, AccType] => getAddressSpace(Lift.liftPair(p.pair)._2, env)

      case f: FstAcc    => getAddressSpace(f.record, env)
      case i: IdxAcc    => getAddressSpace(i.array, env)
      case j: JoinAcc   => getAddressSpace(j.array, env)
      case r: RecordAcc => ???
      case s: SndAcc    => getAddressSpace(s.record, env)
      case s: SplitAcc  => getAddressSpace(s.array, env)
      case t: TruncAcc  => getAddressSpace(t.array, env)

      case ApplyPhrase(_, _) | NatDependentApplyPhrase(_, _) |
           TypeDependentApplyPhrase(_, _) | IfThenElsePhrase(_, _, _) |
           _: LowLevelAccCombinator => ???
    }
  }

}
