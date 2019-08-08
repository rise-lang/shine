package elevate.fsmooth

import elevate.core._
import _root_.FSmooth.VectorFunctionConstants.build
import _root_.FSmooth._
import _root_.FSmooth.DSL._
import _root_.FSmooth.ScalarFunctionConstants._
import _root_.FSmooth.VectorFunctionConstants._

object rules {

  // lambda calculus rules

  case object funToLet extends Strategy[FSmooth] {
    def apply(e: FSmooth): RewriteResult[FSmooth] = e match {
      case Application(Abstraction(Seq(x), e0, _), Seq(e1), _) => Success(Let(x, e1, e0))
      case _ => Failure(funToLet)
    }
  }

  case object letPartialEvaluation extends Strategy[FSmooth] {
    def apply(e: FSmooth): RewriteResult[FSmooth] = e match {
      case Let(x, e0, e1, _) => Success(substitute(Seq(x), Seq(e0), e1))
      case _ => Failure(letPartialEvaluation)
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

  case object letApplication extends Strategy[FSmooth] {
    def apply(e: FSmooth): RewriteResult[FSmooth] = e match {
      case Application(f, Seq(Let(x, e0, e1, _)), _) => Success(Let(x, e0, Application(f, Seq(e1))))
      case _ => Failure(letApplication)
    }
  }

  // ring-structure rules

  case object additionZero extends Strategy[FSmooth] {
    def apply(e: FSmooth): RewriteResult[FSmooth] = e match {
      case Application(`+`(_), Seq(e, ScalarValue(0)), _) => Success(e)
      case Application(`+`(_), Seq(ScalarValue(0), e), _) => Success(e)
      case _ => Failure(additionZero)
    }
  }

  case object multiplicationOne extends Strategy[FSmooth] {
    def apply(e: FSmooth): RewriteResult[FSmooth] = e match {
      case Application(`*`(_), Seq(e, ScalarValue(1)), _) => Success(e)
      case Application(`*`(_), Seq(ScalarValue(1), e), _) => Success(e)
      case _ => Failure(multiplicationOne)
    }
  }

  case object multiplicationZero extends Strategy[FSmooth] {
    def apply(e: FSmooth): RewriteResult[FSmooth] = e match {
      case Application(`*`(_), Seq(e, ScalarValue(0)), _) => Success(ScalarValue(0))
      case Application(`*`(_), Seq(ScalarValue(0), e), _) => Success(ScalarValue(0))
      case _ => Failure(multiplicationZero)
    }
  }

  case object additionSimplification extends Strategy[FSmooth] {
    def apply(e: FSmooth): RewriteResult[FSmooth] = e match {
      case Application(`+`(_), Seq(e1, Application(`-`(_), Seq(e2), _)), _) if e1 == e2 => Success(ScalarValue(0))
      case Application(`-`(_), Seq(e1, e2), _) if e1 == e2 => Success(ScalarValue(0))
      case _ => Failure(additionSimplification)
    }
  }

  case object factorization extends Strategy[FSmooth] {
    def apply(e: FSmooth): RewriteResult[FSmooth] = e match {
      case Application(`+`(_), Seq(
        Application(`*`(_), Seq(e01, e1), _),
        Application(`*`(_), Seq(e02, e2), _),
      ), _) if e01 == e02 => Success(Application(`*`(freshTypeVar), Seq(e01, Application(`+`(freshTypeVar), Seq(e1, e2)))))
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
