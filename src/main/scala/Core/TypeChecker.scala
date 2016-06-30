package Core

import scala.language.reflectiveCalls

class TypeException(msg: String) extends Exception(msg)

object TypeChecker {

  def error(found: String, expected: String): Nothing = {
    throw new TypeException(s"Type error: found $found expected $expected")
  }

  def error(found: PhraseType, expected: PhraseType): Nothing = {
    error(ToString(found), ToString(expected))
  }

  def check(found: PhraseType, expected: PhraseType): Unit = {
    if (found != expected) { error(found, expected) }
  }

  implicit class CheckHelper(p1: PhraseType) {
    def =?=(p2: PhraseType): Unit = check(p1, p2)

    def =?=(f: (PhraseType => Unit)) = f(p1)

    def |(p2: PhraseType): (PhraseType => Unit) = (p: PhraseType) => {
      if (!(p == p1 || p ==p2 )) {
        error(ToString(p), "")
      }
    }
  }

//  def setParamType[T1 <: PhraseType, T2 <: PhraseType](p: Phrase[T1 -> T2], t: T1): Unit = {
//    p match {
//      case l: LambdaPhrase[T1, T2] =>
//        l.param.t match {
//          case _ => l.param.t = t
//        }
//
//      case app: ApplyPhrase[a, T1 -> T2] =>
//        val fun = Lift.liftFunction(app.fun)
//        setParamType(fun(app.arg), t)
//
//      case app: NatDependentApplyPhrase[T1 -> T2] =>
//        val fun = Lift.liftNatDependentFunction(app.fun)
//        setParamType(fun(app.arg), t)
//
//      case p1: Proj1Phrase[T1 -> T2, b] =>
//        val pair = Lift.liftPair(p1.pair)
//        setParamType(pair._1, t)
//
//      case p2: Proj2Phrase[a, T1 -> T2] =>
//        val pair = Lift.liftPair(p2.pair)
//        setParamType(pair._2, t)
//
//      case IfThenElsePhrase(_, thenP, elseP) =>
//        setParamType(thenP, t)
//        setParamType(elseP, t)
//
//      case IdentPhrase(_, _) => throw new Exception("This should never happen")
//    }
//  }
//
//  def setSecondParamType[T1 <: PhraseType,
//                         T2 <: PhraseType,
//                         T3 <: PhraseType](p: Phrase[T1 -> (T2 -> T3)], t: T2): Unit = {
//    p match {
//      case l: LambdaPhrase[T1, T2 -> T3] =>  setParamType(l.body, t)
//
//      case app: ApplyPhrase[a, T1 -> (T2 -> T3)] =>
//        val fun = Lift.liftFunction(app.fun)
//        setSecondParamType(fun(app.arg), t)
//
//      case app: NatDependentApplyPhrase[T1 -> (T2 -> T3)] =>
//        val fun = Lift.liftNatDependentFunction(app.fun)
//        setSecondParamType(fun(app.arg), t)
//
//      case p1: Proj1Phrase[T1 -> (T2 -> T3), b] =>
//        val pair = Lift.liftPair(p1.pair)
//        setSecondParamType(pair._1, t)
//
//      case p2: Proj2Phrase[a, T1 -> (T2 -> T3)] =>
//        val pair = Lift.liftPair(p2.pair)
//        setSecondParamType(pair._2, t)
//
//      case IfThenElsePhrase(_, thenP, elseP) =>
//        setSecondParamType(thenP, t)
//        setSecondParamType(elseP, t)
//
//      case IdentPhrase(_, _) => throw new Exception("This should never happen")
//    }
//  }
//
//  def setThirdParamType[T1 <: PhraseType,
//                        T2 <: PhraseType,
//                        T3 <: PhraseType,
//                        T4 <: PhraseType](p: Phrase[T1 -> (T2 -> (T3 -> T4))], t: T3): Unit = {
//    p match {
//      case l: LambdaPhrase[T1, T2 -> (T3 -> T4)] =>  setSecondParamType(l.body, t)
//
//      case app: ApplyPhrase[a, T1 -> (T2 -> (T3 -> T4))] =>
//        val fun = Lift.liftFunction(app.fun)
//        setThirdParamType(fun(app.arg), t)
//
//      case app: NatDependentApplyPhrase[T1 -> (T2 -> (T3 -> T4))] =>
//        val fun = Lift.liftNatDependentFunction(app.fun)
//        setThirdParamType(fun(app.arg), t)
//
//      case p1: Proj1Phrase[T1 -> (T2 -> (T3 -> T4)), b] =>
//        val pair = Lift.liftPair(p1.pair)
//        setThirdParamType(pair._1, t)
//
//      case p2: Proj2Phrase[a, T1 -> (T2 -> (T3 -> T4))] =>
//        val pair = Lift.liftPair(p2.pair)
//        setThirdParamType(pair._2, t)
//
//      case IfThenElsePhrase(_, thenP, elseP) =>
//        setThirdParamType(thenP, t)
//        setThirdParamType(elseP, t)
//
//      case IdentPhrase(_, _) => throw new Exception("This should never happen")
//    }
//  }

  def apply[T <: PhraseType](phrase: Phrase[T]): T = {
    (phrase match {
      case x: IdentPhrase[T] => x.t

      case LambdaPhrase(x, p) => x.t -> p.t

      case ApplyPhrase(p, q) =>
        p.t match {
          case FunctionType(t1, t2) =>
            t1 =?= q.t
            t2
          case x => error(x.toString, FunctionType.toString)
        }

      case NatDependentLambdaPhrase(a, p) => a -> p.t

      case NatDependentApplyPhrase(p, e) =>
        p.t match {
          case NatDependentFunctionType(a, t) =>
            t `[` e `/` a `]`
          case x => error(x.toString, NatDependentFunctionType.toString)
        }

      case PairPhrase(p, q) => p.t x q.t

      case Proj1Phrase(p) =>
        p.t match {
          case PairType(t1, _) => t1
          case x => error(x.toString, PairType.toString)
        }

      case Proj2Phrase(p) =>
        p.t match {
          case PairType(_, t2) => t2
          case x => error(x.toString, PairType.toString)
        }

      case IfThenElsePhrase(cond, thenP, elseP) =>
        cond.t =?= (exp"[$int]" | exp"[$bool")
        thenP.t =?= elseP.t

        thenP.t

      case LiteralPhrase(d) => ExpType(d.dataType)

      case UnaryOpPhrase(op, x) =>
        x.t match {
          case ExpType(dt) => x.t
          case y => error(y.toString, ExpType.toString)
        }

      case BinOpPhrase(op, lhs, rhs) =>
        op match {
          case BinOpPhrase.Op.GT | BinOpPhrase.Op.LT =>
            (lhs.t, rhs.t) match {
              case (ExpType(dt1), ExpType(dt2)) =>
                check(lhs.t, rhs.t)
                exp"[$bool]"
              case x => error(x.toString, "(ExpType, ExpType)")
            }
          case _ =>
            (lhs.t, rhs.t) match {
              case (ExpType(dt1), ExpType(dt2)) =>
                check(lhs.t, rhs.t)
                lhs.t
              case x => error(x.toString, "(ExpType, ExpType)")
            }
        }

      case p: ExpPattern => p.typeCheck()

      case p: AccPattern => p.typeCheck()

      case p: IntermediateCommandPattern => p.typeCheck()

    }).asInstanceOf[T]
  }

}
