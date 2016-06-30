package Core

class TypeInferenceException(msg: String) extends TypeException(msg)

object TypeInference {

  def error(found: String, expected: String): Nothing = {
    throw new TypeInferenceException(s"Found $found but expected $expected")
  }

  def error(msg: String): Nothing = {
    throw new TypeInferenceException(msg)
  }

  def check[T <: PhraseType](phrase: Phrase[T]): Unit = {
    if (phrase.t == null) {
      error(s"Found phrase $phrase without proper type")
    }
  }

  def apply[T <: PhraseType](phrase: Phrase[T]): Phrase[T] = {
    (phrase match {
      case x: IdentPhrase[T] =>
        x.t match {
          case null => error("Found IdentPhrase without proper type")
          case t: PhraseType => x
        }

      case LambdaPhrase(x, p) =>
        TypeInference(x) match {
          case newX: IdentPhrase[_] => LambdaPhrase(newX, TypeInference(p))
          case _ => throw new Exception("This should not happen")
        }

      case ApplyPhrase(p, q) =>
        ApplyPhrase(TypeInference(p), TypeInference(q))

      case NatDependentLambdaPhrase(a, p) =>
        NatDependentLambdaPhrase(a, TypeInference(p))

      case NatDependentApplyPhrase(p, e) =>
        NatDependentApplyPhrase(TypeInference(p), e)

      case PairPhrase(p, q) => PairPhrase(TypeInference(p), TypeInference(q))

      case Proj1Phrase(p) => Proj1Phrase(TypeInference(p))

      case Proj2Phrase(p) => Proj2Phrase(TypeInference(p))

      case IfThenElsePhrase(cond, thenP, elseP) =>
        IfThenElsePhrase(TypeInference(cond), TypeInference(thenP), TypeInference(elseP))

      case l: LiteralPhrase => l

      case UnaryOpPhrase(op, x) => UnaryOpPhrase(op, TypeInference(x))

      case BinOpPhrase(op, lhs, rhs) => BinOpPhrase(op, TypeInference(lhs), TypeInference(rhs))

      case p: ExpPattern => p.inferTypes()

      case p: AccPattern => p

      case p: IntermediateCommandPattern => p
    }).asInstanceOf[Phrase[T]]
  }

  def setParamType[T1 <: PhraseType, T2 <: PhraseType](phrase: Phrase[T1 -> T2], t: T1): Phrase[T1 -> T2] = {
    phrase match {
      case l@LambdaPhrase(x, p) =>
        val newX = x.copy(`type` = t)
        OperationalSemantics.substitute(newX, `for`=x, in=l)

      case ApplyPhrase(fun, arg) =>
        setParamType(Lift.liftFunction(fun)(arg), t)

      case NatDependentApplyPhrase(fun, arg) =>
        setParamType(Lift.liftNatDependentFunction(fun)(arg), t)

      case Proj1Phrase(pair) =>
        setParamType(Lift.liftPair(pair)._1, t)

      case Proj2Phrase(pair) =>
        setParamType(Lift.liftPair(pair)._2, t)

      case IfThenElsePhrase(cond, thenP, elseP) =>
        IfThenElsePhrase(cond,
          setParamType(thenP, t),
          setParamType(elseP, t))

      case IdentPhrase(_, _) => throw new Exception("This should never happen")
    }
  }

  def setParamTypes[T1 <: PhraseType, T2 <: PhraseType, T3 <: PhraseType](phrase: Phrase[T1 -> (T2 -> T3)], t1: T1, t2: T2): Phrase[T1 -> (T2 -> T3)] = {
    phrase match {
      case LambdaPhrase(x, p) =>
        LambdaPhrase(x.copy(`type` = t1), setParamType(p, t2))

      case ApplyPhrase(fun, arg) =>
        setParamTypes(Lift.liftFunction(fun)(arg), t1, t2)

      case NatDependentApplyPhrase(fun, arg) =>
        setParamTypes(Lift.liftNatDependentFunction(fun)(arg), t1, t2)

      case Proj1Phrase(pair) =>
        setParamTypes(Lift.liftPair(pair)._1, t1, t2)

      case Proj2Phrase(pair) =>
        setParamTypes(Lift.liftPair(pair)._2, t1, t2)

      case IfThenElsePhrase(cond, thenP, elseP) =>
        IfThenElsePhrase(cond,
          setParamTypes(thenP, t1, t2),
          setParamTypes(elseP, t1, t2))

      case IdentPhrase(_, _) => throw new Exception("This should never happen")
    }
  }

  def setSecondParamType[T1 <: PhraseType, T2 <: PhraseType, T3 <: PhraseType](phrase: Phrase[T1 -> (T2 -> T3)], t: T2): Phrase[T1 -> (T2 -> T3)] = {
    phrase match {
      case LambdaPhrase(x, p) =>
        LambdaPhrase(x, setParamType(p, t))

      case ApplyPhrase(fun, arg) =>
        setSecondParamType(Lift.liftFunction(fun)(arg), t)

      case NatDependentApplyPhrase(fun, arg) =>
        setSecondParamType(Lift.liftNatDependentFunction(fun)(arg), t)

      case Proj1Phrase(pair) =>
        setSecondParamType(Lift.liftPair(pair)._1, t)

      case Proj2Phrase(pair) =>
        setSecondParamType(Lift.liftPair(pair)._2, t)

      case IfThenElsePhrase(cond, thenP, elseP) =>
        IfThenElsePhrase(cond,
          setSecondParamType(thenP, t),
          setSecondParamType(elseP, t))

      case IdentPhrase(_, _) => throw new Exception("This should never happen")
    }
  }

  def setThirdParamType[T1 <: PhraseType, T2 <: PhraseType, T3 <: PhraseType, T4 <: PhraseType](phrase: Phrase[T1 -> (T2 -> (T3 -> T4))], t: T3): Phrase[T1 -> (T2 -> (T3 -> T4))] = {
    phrase match {
      case LambdaPhrase(x, p) =>
        LambdaPhrase(x, setSecondParamType(p, t))

      case ApplyPhrase(fun, arg) =>
        setThirdParamType(Lift.liftFunction(fun)(arg), t)

      case NatDependentApplyPhrase(fun, arg) =>
        setThirdParamType(Lift.liftNatDependentFunction(fun)(arg), t)

      case Proj1Phrase(pair) =>
        setThirdParamType(Lift.liftPair(pair)._1, t)

      case Proj2Phrase(pair) =>
        setThirdParamType(Lift.liftPair(pair)._2, t)

      case IfThenElsePhrase(cond, thenP, elseP) =>
        IfThenElsePhrase(cond,
          setThirdParamType(thenP, t),
          setThirdParamType(elseP, t))

      case IdentPhrase(_, _) => throw new Exception("This should never happen")
    }
  }
}
