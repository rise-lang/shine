package elevate.fsmooth

import elevate.core._
import _root_.FSmooth.VectorFunctionConstants.build
import _root_.FSmooth._
import _root_.FSmooth.ScalarFunctionConstants._
import _root_.FSmooth.VectorFunctionConstants._

object rules {

  // lambda calculus rules

  case object funToLet extends Strategy[FSmooth] {
    def apply(e: FSmooth): RewriteResult[FSmooth] = e match {
      //case Application(Abstraction(xs, e, _), es, _) => Let(xs, es, e)
      case _ => Failure(funToLet)
    }
  }

  case object letFission extends Strategy[FSmooth] {
    def apply(e: FSmooth): RewriteResult[FSmooth] = e match {
      case Let(x, Let(y, e0, e1, _), e2, _) => Success(Let(x, e0, Let(y, x, e1)))
      case _ => Failure(letFission)
    }
  }

  case object letInitDuplication extends Strategy[FSmooth] {
    def apply(e: FSmooth): RewriteResult[FSmooth] = e match {
      case Let(x, e00, Let(y, e01, e1, _), _) if e00 == e01 => Success(Let(x, e00, Let(y, x, e1)))
      case _ => Failure(letInitDuplication)
    }
  }

  case object letSwap extends Strategy[FSmooth] {
    def apply(e: FSmooth): RewriteResult[FSmooth] = e match {
      case Let(x, e0, Let(y, e1, e2, _), _) => Success(Let(y, e1, Let(x, e0, e2)))
      case _ => Failure(letSwap)
    }
  }

  // conditional rules

  case object uselessConditional extends Strategy[FSmooth] {
    def apply(e: FSmooth): RewriteResult[FSmooth] = e match {
      case Conditional(_, e0, e1, _) if e0 == e1 => Success(e1)
      case _ => Failure(uselessConditional)
    }
  }

  // loop fusion rules

//  case object buildGet extends Strategy[FSmooth] {
//    def apply(e: FSmooth): RewriteResult[FSmooth] = e match {
//      case Application(Application(`get`(_),
//        Application(Application(build, e0, _), e1, _), _), e2) =>
//    }
//  }

  case object lengthBuild extends Strategy[FSmooth] {
    def apply(e: FSmooth): RewriteResult[FSmooth] = e match {
      case Application(`length`(_), Seq(Application(Application(`build`(_), Seq(e0), _), e1, _), _), _) =>
        Success(e0) // e0 == Seq[Expr] ?!
      case  _ => Failure(lengthBuild)
    }
  }

  // loop normalisation rules

//  case object trivialFold extends Strategy[FSmooth] {
//    def apply(e: FSmooth): RewriteResult[FSmooth] = e match {
//      case Application(`ifold`(_), )
//    }
//  }

}
