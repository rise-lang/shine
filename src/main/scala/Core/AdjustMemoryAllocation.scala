package Core

import CommandPatterns._
import Core.PhraseType.x
import apart.arithmetic.ArithExpr

object AdjustMemoryAllocation {

  def apply[T <: PhraseType](phrase: Phrase[T]): Phrase[T] = {

    case class fun(numGlb: ArithExpr, numLcl: ArithExpr, numPvt: ArithExpr) extends VisitAndRebuild.fun {
      override def apply[U <: PhraseType](p: Phrase[U]): Result[Phrase[U]] = {
        p match {
          case n: New =>
            n.addressSpace match {
              case GlobalMemory =>
                n.f.asInstanceOf[LambdaPhrase[(ExpType x AccType), CommandType]].param.name
                val nn = New(n.dt, GlobalMemory, VisitAndRebuild(n.f, this))
                nn.t = n.t.asInstanceOf[CommandType]
                Replace(nn.asInstanceOf[Phrase[U]])
              case LocalMemory => Continue(this)
              case PrivateMemory => Continue(this)
            }

          case pf: AbstractParFor =>
            pf match {
              case ParForGlobal(_, _, _) | ParForWorkgroup(_, _, _) =>
                val t = DataType.toType(pf.out.t.dataType)
                val len = ir.Type.getMaxLength(t)
                Continue(fun(numGlb * len, numLcl, numPvt))

              case ParForLocal(_, _, _) =>
                val t = DataType.toType(pf.out.t.dataType)
                val len = ir.Type.getMaxLength(t)
                Continue(fun(numGlb * len, numLcl * len, numPvt))

              case _ => Continue(this)
            }

          case _ => Continue(this)
        }
      }
    }

    VisitAndRebuild(phrase, fun(1, 1, 1))
  }

}
