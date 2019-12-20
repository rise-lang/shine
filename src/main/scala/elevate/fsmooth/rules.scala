package elevate.fsmooth

import elevate.core._
import FSmooth.VectorFunctionConstants.build
import FSmooth._
import FSmooth.DSL._
import FSmooth.ScalarFunctionConstants._
import FSmooth.VectorFunctionConstants._
import FSmooth.PairFunctionConstants._
import FSmooth.ValueConstants._

/* Implementing rules for the F~ language as described in:

@article{DBLP:journals/pacmpl/ShaikhhaFVJ19,
  author    = {Amir Shaikhha and
               Andrew Fitzgibbon and
               Dimitrios Vytiniotis and
               Simon {Peyton Jones}},
  title     = {Efficient differentiable programming in a functional array-processing
               language},
  journal   = {{PACMPL}},
  volume    = {3},
  number    = {{ICFP}},
  pages     = {97:1--97:30},
  year      = {2019}
}

 */
object rules {

  // lambda calculus rules

  case object funToLet extends Strategy[FSmooth] {
    def apply(e: FSmooth): RewriteResult[FSmooth] = e match {
      case Application(Abstraction(Seq(x), e0, _), Seq(e1), _) => Success(Let(x, e1, e0))
      case _                                                   => Failure(funToLet)
    }
    override def toString: String = "(fun x -> e_0) e_1 ~> let x = e_1 in e_0"
  }

  case object funToLet2 extends Strategy[FSmooth] {
    def apply(e: FSmooth): RewriteResult[FSmooth] = e match {
      case Application(Abstraction(Seq(x,y), e0, _), Seq(e1, e2), _) => Success(Let(x, e1, Let(y, e2, e0)))
      case _                                                         => Failure(funToLet2)
    }
    override def toString: String = "(fun x -> e_0) e_1 ~> let x = e_1 in e_0 (version 2)"
  }

  case object letPartialEvaluation extends Strategy[FSmooth] {
    def apply(e: FSmooth): RewriteResult[FSmooth] = e match {
      case Let(x, e0, e1, _) => Success(replace(x).`with`(e0).in(e1))
      case _                 => Failure(letPartialEvaluation)
    }
    override def toString: String = "let x = e_0 in e_1 ~> e_1[x / e_0]"
  }

  case object letFission extends Strategy[FSmooth] {
    def apply(e: FSmooth): RewriteResult[FSmooth] = e match {
      case Let(x, Let(y, e0, e1, _), e2, _) => Success(Let(x, e0, Let(y, x, e1)))
      case _                                => Failure(letFission)
    }
    override def toString: String = "let x = let y = e_0 in e_1 ~> let y = e_0 in let x = e_1 in e_2"
  }

  case object letInitDuplication extends Strategy[FSmooth] {
    def apply(e: FSmooth): RewriteResult[FSmooth] = e match {
      case Let(x, e00, Let(y, e01, e1, _), _) if e00 == e01 => Success(Let(x, e00, Let(y, x, e1)))
      case _                                                => Failure(letInitDuplication)
    }
    override def toString: String = "let x = e_0 in let y = e_0 in e_1 ~> let x = e_0 in let y = x in e_1"
  }

  case object letSwap extends Strategy[FSmooth] {
    def apply(e: FSmooth): RewriteResult[FSmooth] = e match {
      case Let(x, e0, Let(y, e1, e2, _), _) => Success(Let(y, e1, Let(x, e0, e2)))
      case _                                => Failure(letSwap)
    }
    override def toString: String = "let x = e_0 in let y = e_1 in e_2 ~> let y = e_1 in let x = e_0 in e_2"
  }

  case object letApplication extends Strategy[FSmooth] {
    def apply(e: FSmooth): RewriteResult[FSmooth] = e match {
      case Application(f, Seq(Let(x, e0, e1, _)), _) => Success(Let(x, e0, Application(f, Seq(e1))))
      case _                                         => Failure(letApplication)
    }
    override def toString: String = "f(let x = e_0 in e_1 ~> let x = e_0 in f(e_1)"
  }

  // ring-structure rules

  case object additionZero extends Strategy[FSmooth] {
    def apply(e: FSmooth): RewriteResult[FSmooth] = e match {
      case Application(`+`(_), Seq(e, ScalarValue(0)), _) => Success(e)
      case Application(`+`(_), Seq(ScalarValue(0), e), _) => Success(e)
      case _                                              => Failure(additionZero)
    }
    override def toString: String = "e + 0 = 0 + e ~> e"
  }

  case object multiplicationOne extends Strategy[FSmooth] {
    def apply(e: FSmooth): RewriteResult[FSmooth] = e match {
      case Application(`*`(_), Seq(e, ScalarValue(1)), _) => Success(e)
      case Application(`*`(_), Seq(ScalarValue(1), e), _) => Success(e)
      case _                                              => Failure(multiplicationOne)
    }
    override def toString: String = "e * 1 = 1 * e ~> e"
  }

  case object multiplicationZero extends Strategy[FSmooth] {
    def apply(e: FSmooth): RewriteResult[FSmooth] = e match {
      case Application(`*`(_), Seq(e, ScalarValue(0)), _) => Success(ScalarValue(0))
      case Application(`*`(_), Seq(ScalarValue(0), e), _) => Success(ScalarValue(0))
      case _                                              => Failure(multiplicationZero)
    }
    override def toString: String = "e * 0 = 0 * e ~> 0"
  }

  case object additionSimplification extends Strategy[FSmooth] {
    def apply(e: FSmooth): RewriteResult[FSmooth] = e match {
      case Application(`+`(_), Seq(e1,
           Application(`-`(_), Seq(e2), _)), _) if e1 == e2 => Success(ScalarValue(0))
      case Application(`-`(_), Seq(e1, e2), _)  if e1 == e2 => Success(ScalarValue(0))
      case _                                                => Failure(additionSimplification)
    }
    override def toString: String = "e + -e = e - e ~> 0"
  }

  case object factorization extends Strategy[FSmooth] {
    def apply(e: FSmooth): RewriteResult[FSmooth] = e match {
      case Application(`+`(_), Seq(
           Application(`*`(_), Seq(e01, e1), _),
           Application(`*`(_), Seq(e02, e2), _)), _) if e01 == e02 =>
        Success(Application(`*`(freshTypeVar), Seq(e01,
                Application(ScalarFunctionConstants.`+`(freshTypeVar), Seq(e1, e2)))))
      case _ => Failure(factorization)
    }
    override def toString: String = "e_0 * e_1 + e_0 * e_2 ~> e_0 * (e_1 + e_2)"
  }

  // conditional rules

  case object conditionTrue extends Strategy[FSmooth] {
    def apply(e: FSmooth): RewriteResult[FSmooth] = e match {
      case Conditional(`true`, e1, _, _) => Success(e1)
      case _                              => Failure(conditionTrue)
    }
    override def toString: String = "if true then e_1 else e_2 ~> e_1"
  }

  case object conditionFalse extends Strategy[FSmooth] {
    def apply(e: FSmooth): RewriteResult[FSmooth] = e match {
      case Conditional(`false`, _, e2, _) => Success(e2)
      case _                              => Failure(conditionFalse)
    }
    override def toString: String = "if false then e_1 else e_2 ~> e_2"
  }

  case object uselessConditional extends Strategy[FSmooth] {
    def apply(e: FSmooth): RewriteResult[FSmooth] = e match {
      case Conditional(_, e0, e1, _) if e0 == e1 => Success(e1)
      case _                                     => Failure(uselessConditional)
    }
    override def toString: String = "if e_0 then e_1 else e_1 ~> e_1"
  }

  case object conditionalPartialEvalution extends Strategy[FSmooth] {
    def apply(e: FSmooth): RewriteResult[FSmooth] = e match {
      case Conditional(e0, e1, e2, _) => Success(Conditional(e0,
                                            replace(e0).`with`(`true`).in(e1),
                                            replace(e0).`with`(`false`).in(e2)))
      case _                          => Failure(conditionalPartialEvalution)
    }
    override def toString: String = "if e_0 then e_1 else e_2 ~> if e_0 then e_1[e_0 / true] else e_2[e_0 / false]"
  }

  case object conditionApplication extends Strategy[FSmooth] {
    def apply(e: FSmooth): RewriteResult[FSmooth] = e match {
      case Application(f, Seq(Conditional(e0, e1, e2, _)), _) =>
        Success(Conditional(e0, Application(f, Seq(e1)), Application(f, Seq(e2))))
      case _ => Failure(conditionApplication)
    }
    override def toString: String = "f(if e_0 then e_1 else e_2) ~> if e_0 then f(e_1) else f(e_2)"
  }

  case object conditionApplication2 extends Strategy[FSmooth] {
    def apply(e: FSmooth): RewriteResult[FSmooth] = e match {
      case Application(f, Seq(arg0, Conditional(e0, e1, e2, _)), _) =>
        Success(Conditional(e0, Application(f, Seq(arg0, e1)), Application(f, Seq(arg0, e2))))
      case _ => Failure(conditionApplication)
    }
    override def toString: String = "f(if e_0 then e_1 else e_2) ~> if e_0 then f(e_1) else f(e_2) (version2)"
  }

  // loop fusion rules

  case object buildGet extends Strategy[FSmooth] {
    def apply(e: FSmooth): RewriteResult[FSmooth] = e match {
      case Application(`get`(_), Seq(Application(`build`(_), Seq(_,e1), _), e2), _) => Success(Application(e1, Seq(e2)))
      case _                                                                        => Failure(buildGet)
    }
    override def toString: String = "(build e_0 e_1)[e_2] ~> e_1 e_2"
  }

  case object lengthBuild extends Strategy[FSmooth] {
    def apply(e: FSmooth): RewriteResult[FSmooth] = e match {
      case Application(`length`(_), Seq(Application(`build`(_), Seq(e0, _), _)), _) => Success(e0)
      case  _                                                                       => Failure(lengthBuild)
    }
    override def toString: String = "length(build e_0 e_1) ~> e_0"
  }

  // loop normalisation rules

  case object trivialFold extends Strategy[FSmooth] {
    def apply(e: FSmooth): RewriteResult[FSmooth] = e match {
      case Application(`ifold`(_), Seq(_, z, ScalarValue(0)), _) => Success(z)
      case _                                                     => Failure(trivialFold)
    }
    override def toString: String = "ifold f z 0 ~> z"
  }

  case object foldInsertFun extends Strategy[FSmooth] {
    def apply(e: FSmooth): RewriteResult[FSmooth] = e match {
      case Application(`ifold`(_), Seq(f, z, n), _) =>
        val (a, i) = (Variable("a"), Variable("i"))
        Success(Application(VectorFunctionConstants.`ifold`(freshTypeVar), Seq(
          Abstraction(Seq(a, i),
          Application(f, Seq(a, Application(ScalarFunctionConstants.`+`(freshTypeVar), Seq(i, ScalarValue(1)))))),
          Application(f, Seq(z, ScalarValue(0))),
          Application(ScalarFunctionConstants.`-`(freshTypeVar), Seq(n, ScalarValue(1)))
        )))
      case _ => Failure(foldInsertFun)
    }
    override def toString: String = "ifold f z n ~> ifold(fun a i -> f a (i+1))(f z 0)(n -1)"
  }

  case object foldSimplification extends Strategy[FSmooth] {
    def apply(e: FSmooth): RewriteResult[FSmooth] = e match {
      case Application(`ifold`(_), Seq(Abstraction(Seq(a1, _), a2, _), z, _), _) if a1 == a2 => Success(z)
      case _                                                                                 => Failure(foldSimplification)
    }
    override def toString: String = "ifold(fun a i -> a) z n ~> z"
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
    override def toString: String = "ifold(fun a i -> if (i = e_0) then e_1 else a) z n ~> e_1 (if e_0 does not mention a or i)"
  }

  // tuple normalisation rules

  case object pairFst extends Strategy[FSmooth] {
    def apply(e: FSmooth): RewriteResult[FSmooth] = e match {
      case Application(`fst`(_), Seq(Application(`pair`(_), Seq(e0, _), _)), _) => Success(e0)
      case _                                                                    => Failure(pairFst)
    }
    override def toString: String = "fst (e_0, e_1) ~> e_0"
  }

  case object pairSnd extends Strategy[FSmooth] {
    def apply(e: FSmooth): RewriteResult[FSmooth] = e match {
      case Application(`snd`(_), Seq(Application(`pair`(_), Seq(_, e1), _)), _) => Success(e1)
      case _                                                                    => Failure(pairSnd)
    }
    override def toString: String = "snd(e_0, e_1) ~> e_1"
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
    override def toString: String = "loopFission"
  }
}
