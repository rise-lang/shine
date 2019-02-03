package idealised.DPIA.Compilation

import idealised.DPIA.ImperativePrimitives._
import idealised.DPIA.Phrases._
import idealised.DPIA.Types.{AccType, CommandType, PhraseType}
import idealised.DPIA._

import scala.collection.immutable


object SubstituteImplementations {

  case class Environment(addressSpace: immutable.Map[String, AddressSpace])

  object Environment {
    def apply(): Environment = Environment(immutable.Map[String, AddressSpace]())
  }

  def apply(phrase: Phrase[CommandType], env: Environment)
           (implicit context: TranslationContext): Phrase[CommandType] = {

    case class fun(env: Environment) extends VisitAndRebuild.Visitor {
      override def apply[T <: PhraseType](p: Phrase[T]): Result[Phrase[T]] = {
        p match {
          case ip: Intermediate[T]@unchecked =>
            Continue(ip.substituteImpl(env), this)

          // add the addressSpace of every new variable to the environment
          case New(_, addressSpace, f) =>
            val param = f match {
              case Lambda(param_, _) => param_
              case _ => throw new Exception("This should not happen")
            }
            Continue(p, fun(env.copy(env.addressSpace.updated(param.name, addressSpace))))

          case _ => Continue(p, this)
        }
      }
    }

    VisitAndRebuild(phrase, fun(env))
  }

  def getAddressSpace(phrase: Phrase[AccType], env: Environment): Option[AddressSpace] = {
    phrase match {
      case Identifier(name, _) => Some(env.addressSpace(name))

      case p: Proj1[AccType, _] => getAddressSpace(Lifting.liftPair(p.pair)._1, env)
      case p: Proj2[_, AccType] => getAddressSpace(Lifting.liftPair(p.pair)._2, env)

      case f: RecordAcc1    => getAddressSpace(f.record, env)
      case i: IdxAcc    => getAddressSpace(i.array, env)
      case j: JoinAcc   => getAddressSpace(j.array, env)
      case s: RecordAcc2    => getAddressSpace(s.record, env)
      case s: SplitAcc  => getAddressSpace(s.array, env)
      case t: TakeAcc  => getAddressSpace(t.array, env)

      case Apply(_, _) | NatDependentApply(_, _) |
           TypeDependentApply(_, _) | IfThenElse(_, _, _) | _: AccPrimitive => ???
    }
  }

}
