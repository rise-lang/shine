package shine.DPIA.Types

import shine.DPIA._
import shine.DPIA.Phrases._

class TypeException(msg: String) extends Exception(msg)

object TypeCheck {
  def apply[T <: PhraseType](phrase: Phrase[T]): Unit = {
    phrase match {
      case Identifier(_, _) =>

      case Lambda(x, p) => TypeCheck(x); TypeCheck(p)

      case Apply(p, q) =>
        TypeCheck(p)
        TypeCheck(q)
        errorIfNotEqOrSubtype(q.t, p.t.inT)

      case DepLambda(_, p) => TypeCheck(p)

      case DepApply(p, _) => TypeCheck(p)

      case LetNat(_, defn, body) => TypeCheck(defn); TypeCheck(body)

      case PhrasePair(p, q) => TypeCheck(p); TypeCheck(q)

      case Proj1(p) => TypeCheck(p)

      case Proj2(p) => TypeCheck(p)

      case IfThenElse(cond, thenP, elseP) =>
        TypeCheck(cond)
        TypeCheck(thenP)
        TypeCheck(elseP)
        check(cond.t, expT(int, read) | expT(bool, read))
        //This is more restrictive than it has to be.
        //We could also allow: thenP <= elseP || elseP <= thenP.
        errorIfNotEq(thenP.t, elseP.t)

      case Literal(_) =>

      case Natural(_) =>

      case UnaryOp(op, x) =>
        TypeCheck(x)
        x.t match {
          case ExpType(_, `read`) =>
          case y => error(y.toString, s"$op ${ExpType.toString}")
        }

      case BinOp(op, lhs, rhs) =>
        TypeCheck(lhs)
        TypeCheck(rhs)
        (lhs.t, rhs.t) match {
          case (ExpType(dt1, `read`), ExpType(dt2, `read`))
            if dt1.isInstanceOf[BasicType] && dt2.isInstanceOf[BasicType] =>
              if (lhs.t != rhs.t) {
                error(found = s"${lhs.t} and ${rhs.t}",
                  expected = "them to match")
              }
          case (x1, x2) =>
            error(s"$x1 $op $x2", s"exp[b] $op exp[b]")
        }

      case _: Primitive[_] =>
    }
  }

  def errorIfNotEq(sub: PhraseType, pt: PhraseType): Unit = {
    if (!(sub == pt))
      throw new TypeException(
        s"Type error: found $sub, expected $pt")
  }

  def errorIfNotEqOrSubtype(sub: PhraseType, pt: PhraseType): Unit = {
    if (!(sub `<=` pt))
      throw new TypeException(
        s"Type error: found $sub which is not a subtype of expected $pt")
  }

  def check(found: PhraseType, test: PhraseType => Unit): Unit = {
    test(found)
  }

  implicit class AtLeastOneEqCheckHelper(p1: PhraseType) {
    def |(p2: PhraseType): PhraseType => Unit = (p: PhraseType) => {
      if (!(p == p1 || p == p2))
        throw new TypeException(
          s"Type error: found $p, expected $p1 or $p2")
    }
  }

  implicit class EqOrSubtypeCheckHelper[T <: PhraseType](p: Phrase[T]) {
    def checkTypeEqOrSubtype(pt: PhraseType): Boolean = {
      TypeCheck(p)
      p.t `<=` pt
    }
  }

  implicit class SubTypeCheckHelper(subType: PhraseType) {
    def `<=`(superType: PhraseType): Boolean = {
      subtypeCheck(subType, superType)
    }
  }

  private def subtypeCheck(
    subType: PhraseType,
    superType: PhraseType
  ): Boolean = {
    if (subType == superType) return true

    (subType, superType) match {
      case (ExpType(bSub: DataType, accessSub), ExpType(bSuper, _))
        if bSub == bSuper =>
          accessSub == read && notContainingArrayType(bSub)
      case (FunType(subInT, subOutT), FunType(superInT, superOutT)) =>
        subtypeCheck(superInT, subInT) && subtypeCheck(subOutT,  superOutT)
      case (DepFunType(subInT, subOutT), DepFunType(superInT, superOutT)) =>
        subInT == superInT && subtypeCheck(subOutT, superOutT)
      case _ => false
    }
  }

  def notContainingArrayType(composed: DataType): Boolean = composed match {
      case _: BasicType => true
      case PairType(first, second) =>
        notContainingArrayType(first) && notContainingArrayType(second)
      case _ => false
  }
}
