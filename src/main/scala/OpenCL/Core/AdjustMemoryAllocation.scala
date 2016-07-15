package OpenCL.Core

import Core._
import LowLevelCombinators._

import scala.language.postfixOps
import scala.language.reflectiveCalls

object AdjustMemoryAllocation {

  def apply[T <: PhraseType](phrase: Phrase[T]): Phrase[T] = {

    var allocations = List[(DataType, AddressSpace, IdentPhrase[VarType])]()

    case class fun(env: List[(IdentPhrase[ExpType], Nat)]) extends VisitAndRebuild.fun {
      override def pre[U <: PhraseType](p: Phrase[U]): Result[Phrase[U]] = {
        p match {
          // remember index var and length for each par for
          case pf: AbstractParFor =>
            pf.body match {
              case LambdaPhrase(param, _) =>
                Continue(pf, fun((param, pf.n) :: env))
              case _ => throw new Exception("This should not happen")
            }
          case _ => Continue(p, this)
        }
      }

      override def post[U <: PhraseType](p: Phrase[U]): Phrase[U] = {
        import DSL.typed._
        p match {
          case New(dt, addressSpace, f) if addressSpace != PrivateMemory =>
            val newDt = env.foldLeft(dt)( (dt_, head) =>ArrayType(head._2, dt_))
            val newParam = IdentPhrase(newName(), VarType(newDt))
            allocations = (newDt, addressSpace, newParam) :: allocations
            val p = f(newParam)

            env.foldLeft(p)( (p1, head) => {
              val (i, n) = head
              val p2 = Phrase.substitute(π1(newParam) `@` i, `for`=π1(newParam), in=p1)
              val p3 = Phrase.substitute(π2(newParam) `@` i, `for`=π2(newParam), in=p2)
              p3
            }).asInstanceOf[Phrase[U]]
//            val (i, n) = env.head
//            val newDt = ArrayType(n, dt)
//            val newParam = IdentPhrase(newName(), VarType(newDt))
//            allocations = (newDt, addressSpace, newParam) :: allocations
//            val p1 = f(newParam) `[` (π1(newParam) `@` i) `/` π1(newParam) `]`
//            val p2 = Phrase.substitute(π2(newParam) `@` i, `for` = π2(newParam), in = p1)
//            p2.asInstanceOf[Phrase[U]]
          case _ => p
        }
      }
    }

    val body = VisitAndRebuild(phrase, fun(List()))
    allocations.foldLeft( body.asInstanceOf[Phrase[CommandType]] )((b, alloc) =>
      New(alloc._1, alloc._2, LambdaPhrase(alloc._3, b))
    ).asInstanceOf[Phrase[T]]
  }

}
