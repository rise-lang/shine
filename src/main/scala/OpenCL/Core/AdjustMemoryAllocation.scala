package OpenCL.Core

import Core._
import LowLevelCombinators._

object AdjustMemoryAllocation {

  def apply[T <: PhraseType](phrase: Phrase[T]): Phrase[T] = {

    case class fun(numGlb: Nat, numLcl: Nat, numPvt: Nat) extends VisitAndRebuild.fun {
      override def apply[U <: PhraseType](p: Phrase[U]): Result[Phrase[U]] = {
        p match {
          case n: New =>
            n.addressSpace match {
              case GlobalMemory =>
                n.f.asInstanceOf[LambdaPhrase[(ExpType x AccType), CommandType]].param.name
                val nn = New(n.dt, GlobalMemory, VisitAndRebuild(n.f, this))
                Stop(nn.asInstanceOf[Phrase[U]])
              case LocalMemory => Continue(n, this)
              case PrivateMemory => Continue(n, this)
            }

//          case pf: AbstractParFor =>
//            pf match {
//              case ParForGlobal(_, _, _, _) | ParForWorkgroup(_, _, _, _) =>
//                val t = DataType.toType(pf.out.t.dataType)
//                val len = ir.Type.getMaxLength(t)
//                Continue(pf, fun(numGlb * len, numLcl, numPvt))
//
//              case ParForLocal(_, _, _, _) =>
//                val t = DataType.toType(pf.out.t.dataType)
//                val len = ir.Type.getMaxLength(t)
//                Continue(pf, fun(numGlb * len, numLcl * len, numPvt))
//
//              case _ => Continue(pf, this)
//            }

          case _ => Continue(p, this)
        }
      }
    }

    VisitAndRebuild(phrase, fun(1, 1, 1))
  }

}
