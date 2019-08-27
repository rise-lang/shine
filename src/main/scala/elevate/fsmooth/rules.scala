package elevate.fsmooth

import elevate.core._
import elevate.fsmooth.traversal._
import _root_.FSmooth.VectorFunctionConstants.build
import _root_.FSmooth._
import _root_.FSmooth.DSL._
import _root_.FSmooth.ScalarFunctionConstants._
import _root_.FSmooth.VectorFunctionConstants._
import _root_.FSmooth.PairFunctionConstants._
import _root_.FSmooth.ValueConstants._
import elevate.core.strategies.predicate._

object rules {

  // lambda calculus rules

  case object funToLet extends Strategy[FSmooth] {
    def apply(e: FSmooth): RewriteResult[FSmooth] = e match {
      case Application(Abstraction(Seq(x), e0, _), Seq(e1), _) => Success(Let(x, e1, e0))
      case _ => Failure(funToLet)
    }
  }

  case object funToLet2 extends Strategy[FSmooth] {
    def apply(e: FSmooth): RewriteResult[FSmooth] = e match {
      case Application(Abstraction(Seq(x,y), e0, _), Seq(e1, e2), _) => Success(
        Let(x, e1, Let(y, e2, e0))
      )
      case _ => Failure(funToLet2)
    }
  }

  case object letPartialEvaluation extends Strategy[FSmooth] {
    def apply(e: FSmooth): RewriteResult[FSmooth] = e match {
      case Let(x, e0, e1, _) => Success(substitute(x, e0, e1))
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
        Application(`*`(_), Seq(e02, e2), _)), _) if e01 == e02 =>
        Success(Application(`*`(freshTypeVar), Seq(e01, Application(ScalarFunctionConstants.`+`(freshTypeVar), Seq(e1, e2)))))
      case _ => Failure(factorization)
    }
  }

  // conditional rules

  case object conditionTrue extends Strategy[FSmooth] {
    def apply(e: FSmooth): RewriteResult[FSmooth] = e match {
      case Conditional(`true`, e1, e2, _) => Success(e1)
      case _ => Failure(conditionTrue)
    }
  }

  case object conditionFalse extends Strategy[FSmooth] {
    def apply(e: FSmooth): RewriteResult[FSmooth] = e match {
      case Conditional(`false`, e1, e2, _) => Success(e2)
      case _ => Failure(conditionFalse)
    }
  }

  case object uselessConditional extends Strategy[FSmooth] {
    def apply(e: FSmooth): RewriteResult[FSmooth] = e match {
      case Conditional(_, e0, e1, _) if e0 == e1 => Success(e1)
      case _ => Failure(uselessConditional)
    }
  }

  case object conditionalPartialEvalution extends Strategy[FSmooth] {
    def apply(e: FSmooth): RewriteResult[FSmooth] = e match {
      case Conditional(e0, e1, e2, _) =>
        Success(Conditional(e0, substitute(e0, `true`, e1), substitute(e0, `false`, e2)))
      case _ => Failure(conditionalPartialEvalution)
    }
  }

  case object conditionApplication extends Strategy[FSmooth] {
    def apply(e: FSmooth): RewriteResult[FSmooth] = e match {
      case Application(f, Seq(Conditional(e0, e1, e2, _)), _) =>
        Success(Conditional(e0, Application(f, Seq(e1)), Application(f, Seq(e2))))
      case _ => Failure(conditionApplication)
    }
  }

  case object conditionApplication2 extends Strategy[FSmooth] {
    def apply(e: FSmooth): RewriteResult[FSmooth] = e match {
      case Application(f, Seq(arg0, Conditional(e0, e1, e2, _)), _) =>
        Success(Conditional(e0, Application(f, Seq(arg0, e1)), Application(f, Seq(arg0, e2))))
      case _ => Failure(conditionApplication)
    }
  }

  // loop fusion rules

  case object buildGet extends Strategy[FSmooth] {
    def apply(e: FSmooth): RewriteResult[FSmooth] = e match {
      case Application(`get`(_), Seq(Application(`build`(_), Seq(e0, e1), _),  e2), _) =>
        Success(Application(e1, Seq(e2)))
      case _ => Failure(buildGet)
    }
  }

  case object lengthBuild extends Strategy[FSmooth] {
    def apply(e: FSmooth): RewriteResult[FSmooth] = e match {
      case Application(`length`(_), Seq(Application(`build`(_), Seq(e0, e1), _)), _) =>
        Success(e0)
      case  _ => Failure(lengthBuild)
    }
  }

  // loop normalisation rules

  case object trivialFold extends Strategy[FSmooth] {
    def apply(e: FSmooth): RewriteResult[FSmooth] = e match {
      case Application(`ifold`(_), Seq(f, z, ScalarValue(0)), _) => Success(z)
      case _ => Failure(trivialFold)
    }
  }

  case object foldInsertFun extends Strategy[FSmooth] {
    def apply(e: FSmooth): RewriteResult[FSmooth] = e match {
      case Application(`ifold`(_), Seq(f, z, n), _) =>
        val (a, i) = (Variable("a"), Variable("i"))
        Success(Application(VectorFunctionConstants.`ifold`(freshTypeVar), Seq(
          Abstraction(Seq(a, i), Application(f, Seq(a, Application(ScalarFunctionConstants.`+`(freshTypeVar), Seq(i, ScalarValue(1)))))),
          Application(f, Seq(z, ScalarValue(0))),
          Application(ScalarFunctionConstants.`-`(freshTypeVar), Seq(n, ScalarValue(1)))
        )))
      case _ => Failure(foldInsertFun)
    }
  }

  case object foldSimplification extends Strategy[FSmooth] {
    def apply(e: FSmooth): RewriteResult[FSmooth] = e match {
      case Application(`ifold`(_), Seq(Abstraction(Seq(a1,i), a2, _), z, n), _) if a1 == a2 =>
        Success(z)
      case _ => Failure(foldSimplification)
    }
  }

  case object foldConditional extends Strategy[FSmooth] {
    def apply(e: FSmooth): RewriteResult[FSmooth] = e match {
      case Application(`ifold`(_), Seq(
        Abstraction(Seq(a1, i1), Conditional(Application(`=:=`(_), Seq(i2, e0), _), e1, a2, _), _),
        z,
        n), _) =>
        //if a1 == a2 && i1 == i2 && !contains[FSmooth](a1).apply(e0) && !contains[FSmooth](i1).apply(e0) =>
        Success(Let(a1, z, Let(i1, e0, e1)))
      case _ => Failure(foldConditional)
    }
  }

  // tuple normalisation rules

  case object pairFst extends Strategy[FSmooth] {
    def apply(e: FSmooth): RewriteResult[FSmooth] = e match {
      case Application(`fst`(_), Seq(Application(`pair`(_), Seq(e0, e1), _)), _) => Success(e0)
      case _ => Failure(pairFst)
    }
  }

  case object pairSnd extends Strategy[FSmooth] {
    def apply(e: FSmooth): RewriteResult[FSmooth] = e match {
      case Application(`snd`(_), Seq(Application(`pair`(_), Seq(e0, e1), _)), _) => Success(e1)
      case _ => Failure(pairSnd)
    }
  }

  // loop fission rule

  case object loopFission extends Strategy[FSmooth] {
    def apply(e: FSmooth): RewriteResult[FSmooth] = e match {
      case Application(`ifold`(_), Seq(
      Abstraction(Seq(a1,i1), Application(`pair`(_), Seq(
        Application(f0, Seq(Application(`fst`(_), Seq(a2), _), i2), _),
        Application(f1, Seq(Application(`snd`(_), Seq(a3), _), i3), _)), _), _),
      Application(`pair`(_), Seq(z0, z1), _), n), _) if a1 == a2 && a1 == a3 && i1 == i2 && i1 == i2 =>

        Success(Application(PairFunctionConstants.`pair`(freshTypeVar), Seq(
          Application(VectorFunctionConstants.`ifold`(freshTypeVar), Seq(f0, z0, n)),
          Application(VectorFunctionConstants.`ifold`(freshTypeVar), Seq(f1, z1, n))
        )))

      case _ => Failure(loopFission)
    }
  }

}
