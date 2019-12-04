package elevate.rise.rules

import elevate.core.{Failure, RewriteResult, Strategy, Success}
import elevate.rise.Rise
import lift.core._
import lift.core.primitives._
import lift.core.DSL._

object specialize {

  case object mapSeq extends Strategy[Rise] {
    def apply(e: Rise): RewriteResult[Rise] = e match {
      case m@Map() => Success(MapSeq()(m.t))
      case _       => Failure(mapSeq)
    }
    override def toString = "mapSeq"
  }

  case object mapGlobal extends Strategy[Rise] {
    def apply(e: Rise): RewriteResult[Rise] = e match {
      case m @ Map() => Success(lift.OpenCL.primitives.MapGlobal(0)(m.t))
      case _ => Failure(mapGlobal)
    }
    override def toString = "mapGlobal"
  }

  // only transforms maps which contain ForeignFunctions or mapSeqs
  case object mapSeqCompute extends Strategy[Rise] {
    def apply(e: Rise): RewriteResult[Rise] = e match {
      // (mapSeq λη1. (my_abs η1))
      case App(m @ Map(), l @ Lambda(_, App(ForeignFunction(_), _))) => Success(App(MapSeq()(m.t), l)(e.t))
      // (map λη1. ((mapSeq λη2. (my_abs η2)) η1))
      case App(m @ Map(), l @ Lambda(_, App(App(MapSeq(), _), _))) => Success(App(MapSeq()(m.t), l)(e.t))
      case _ => Failure(mapSeqCompute)
    }
    override def toString = "mapSeqCompute"
  }

  case object reduceSeq extends Strategy[Rise] {
    def apply(e: Rise): RewriteResult[Rise] = e match {
      case Reduce() => Success(DSL.reduceSeq)
      case _ => Failure(reduceSeq)
    }
    override def toString = "reduceSeq"
  }

  case object reduceSeqUnroll extends Strategy[Rise] {
    def apply(e: Rise): RewriteResult[Rise] = e match {
      case Reduce() | ReduceSeq() => Success(DSL.reduceSeqUnroll)
      case _ => Failure(reduceSeqUnroll)
    }
    override def toString = "reduceSeqUnroll"
  }

  case class slideSeq(rot: SlideSeq.Rotate, write_dt1: Expr) extends Strategy[Rise] {
    def apply(e: Rise): RewriteResult[Rise] = e match {
      case Slide() => Success(nFun(sz => nFun(sp =>
        DSL.slideSeq(rot)(sz)(sp)(write_dt1)(fun(x => x))
      )))
      case _ => Failure(slideSeq(rot, write_dt1))
    }
    override def toString = s"slideSeq($rot, $write_dt1)"
  }
}