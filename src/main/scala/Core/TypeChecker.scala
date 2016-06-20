package Core

import PhraseType._

class TypeException(msg: String) extends Exception(msg)

object TypeChecker {

  def error(found: String, expected: String) = {
    throw new TypeException("Type error: found " + found + " expected " + expected)
  }

  def check(current: PhraseType, expected: PhraseType) = {
    if (current != expected) {
      error(current.toString, expected.toString)
    }
  }

  def setParamType[T1 <: PhraseType, T2 <: PhraseType](p: Phrase[T1 -> T2], t: T1): Unit = {
    p match {
      case l: LambdaPhrase[T1, T2] =>
        l.param.t match {
          case null => l.param.t = t // infer the type if not set
          case _ =>
        }

      case app: ApplyPhrase[a, T1 -> T2] =>
        val fun = Lift.liftFunction(app.fun)
        setParamType(fun(app.arg), t)

      case p1: Proj1Phrase[T1 -> T2, b] =>
        val pair = Lift.liftPair(p1.pair)
        setParamType(pair._1, t)

      case p2: Proj2Phrase[a, T1 -> T2] =>
        val pair = Lift.liftPair(p2.pair)
        setParamType(pair._2, t)

      case IfThenElsePhrase(_, thenP, elseP) =>
        setParamType(thenP, t)
        setParamType(elseP, t)

      case IdentPhrase(_) => throw new Exception("This should never happen")
    }
  }

  def setSecondParamType[T1 <: PhraseType,
                         T2 <: PhraseType,
                         T3 <: PhraseType](p: Phrase[T1 -> (T2 -> T3)], t: T2): Unit = {
    p match {
      case l: LambdaPhrase[T1, T2 -> T3] =>  setParamType(l.body, t)

      case app: ApplyPhrase[a, T1 -> (T2 -> T3)] =>
        val fun = Lift.liftFunction(app.fun)
        setSecondParamType(fun(app.arg), t)

      case p1: Proj1Phrase[T1 -> (T2 -> T3), b] =>
        val pair = Lift.liftPair(p1.pair)
        setSecondParamType(pair._1, t)

      case p2: Proj2Phrase[a, T1 -> (T2 -> T3)] =>
        val pair = Lift.liftPair(p2.pair)
        setSecondParamType(pair._2, t)

      case IfThenElsePhrase(_, thenP, elseP) =>
        setSecondParamType(thenP, t)
        setSecondParamType(elseP, t)

      case IdentPhrase(_) => throw new Exception("This should never happen")
    }
  }

  def setThirdParamType[T1 <: PhraseType,
                        T2 <: PhraseType,
                        T3 <: PhraseType,
                        T4 <: PhraseType](p: Phrase[T1 -> (T2 -> (T3 -> T4))], t: T3): Unit = {
    p match {
      case l: LambdaPhrase[T1, T2 -> (T3 -> T4)] =>  setSecondParamType(l.body, t)

      case app: ApplyPhrase[a, T1 -> (T2 -> (T3 -> T4))] =>
        val fun = Lift.liftFunction(app.fun)
        setThirdParamType(fun(app.arg), t)

      case p1: Proj1Phrase[T1 -> (T2 -> (T3 -> T4)), b] =>
        val pair = Lift.liftPair(p1.pair)
        setThirdParamType(pair._1, t)

      case p2: Proj2Phrase[a, T1 -> (T2 -> (T3 -> T4))] =>
        val pair = Lift.liftPair(p2.pair)
        setThirdParamType(pair._2, t)

      case IfThenElsePhrase(_, thenP, elseP) =>
        setThirdParamType(thenP, t)
        setThirdParamType(elseP, t)

      case IdentPhrase(_) => throw new Exception("This should never happen")
    }
  }

  def apply[T <: PhraseType](p: Phrase[T]): T = {
    val phraseType = (p match {

      case i: IdentPhrase[T] =>
        if (i.t == null)
          throw new TypeException("Type error: type not set for " + i)
        i.t

      case LambdaPhrase(param, body) =>
        TypeChecker(param) -> TypeChecker(body)

      case ApplyPhrase(fun, arg) =>
        setParamType(fun, TypeChecker(arg))
        TypeChecker(fun) match {
          case ft: FunctionType[_, _] =>
            check(TypeChecker(arg), ft.inT)
            ft.outT
          case t => error(t.toString, FunctionType.toString)
        }

      case PairPhrase(a, b) => TypeChecker(a) x TypeChecker(b)

      case Proj1Phrase(pair) =>
        TypeChecker(pair) match {
          case pt: PairType[_, _] => pt.t1
          case t => error(t.toString, PairType.toString)
        }

      case Proj2Phrase(pair) =>
        TypeChecker(pair) match {
          case pt: PairType[_, _] => pt.t2
          case t => error(t.toString, PairType.toString)
        }

      case IfThenElsePhrase(cond, thenP, elseP) =>
        val condT = TypeChecker(cond)
        if (condT != ExpType(int) && condT != ExpType(bool)) {
          error(condT.toString, expected = "int or boolean")
        }

        val thenPT = TypeChecker(thenP)
        val elsePT = TypeChecker(elseP)
        check(thenPT, elsePT)
        thenPT

      case LiteralPhrase(d) => ExpType(d.dataType)

      case UnaryOpPhrase(op, x) =>
        TypeChecker(x) match {
          case ExpType(dt) => ExpType(dt)
          case x => error(x.toString, expected = "ExpType")
        }

      case BinOpPhrase(op, lhs, rhs) =>
        op match {
          case BinOpPhrase.Op.GT | BinOpPhrase.Op.LT =>
            (TypeChecker(lhs), TypeChecker(rhs)) match {
              case (ExpType(dt1), ExpType(dt2)) if dt1 == dt2 =>
                ExpType(bool)
              case x => error(x.toString, expected = "")
            }
          case _ => (TypeChecker(lhs), TypeChecker(rhs)) match {
            case (ExpType(dt1), ExpType(dt2)) if dt1 == dt2 =>
              ExpType(dt1)
            case x => error(x.toString, expected = "")
          }
        }

      case p: ExpPattern => p.typeCheck()

      case p: AccPattern => p.typeCheck()

      case p: IntermediateCommandPattern => p.typeCheck()

    }).asInstanceOf[T]
    p.t = phraseType
    p.t
  }

}
