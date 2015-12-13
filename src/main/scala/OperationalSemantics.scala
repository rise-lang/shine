
import PhraseType._

import scala.collection.immutable.HashMap

object OperationalSemantics {

  type Store = HashMap[String, Int]

  def substitute[T1 <: PhraseType, T2 <: PhraseType](p1: Phrase[T1],
                                                     p2: Phrase[T1],
                                                     in: Phrase[T2]): Phrase[T2] = {
    if (p2.eq(in)) {
      p1.asInstanceOf[Phrase[T2]] // T1 == T2
    } else {
      in match {
        // these cases must all be `<: Phrase[T2]`, because they match on in.
        // The casts should be unnecessary
        case l: Lambda[_, _] =>
          val newParam = substitute(p1, p2, l.param)
          val newBody = substitute(p1, p2, l.body)
          Lambda(newParam, newBody).asInstanceOf[Phrase[T2]]

        case app: Apply[a, T2] =>
          val newFun = substitute(p1, p2, app.fun)
          val newArg = substitute(p1, p2, app.arg)
          Apply(newFun, newArg)

        case pair: Pair[a, b] =>
          Pair(substitute(p1, p2, pair.fst), substitute(p1, p2, pair.snd)).asInstanceOf[Phrase[T2]]

        case p: Proj1[T2, b] => Proj1(substitute(p1, p2, p.pair))
        case p: Proj2[a, T2] => Proj2(substitute(p1, p2, p.pair))

        case Seq(c1, c2) =>
          Seq(substitute(p1, p2, c1), substitute(p1, p2, c2)).asInstanceOf[Phrase[T2]]

        case New(f) =>
          New(substitute(p1, p2, f)).asInstanceOf[Phrase[T2]]

        case Assign(lhs, rhs) =>
          Assign(substitute(p1, p2, lhs), substitute(p1, p2, rhs)).asInstanceOf[Phrase[T2]]

        case i: IfThenElse[T2] =>
          val newCond = substitute(p1, p2, i.cond)
          val newThenP = substitute(p1, p2, i.thenP)
          val newElseP = substitute(p1, p2, i.elseP)
          IfThenElse(newCond, newThenP, newElseP)

        case For(n, body) =>
          For(substitute(p1, p2, n), substitute(p1, p2, body)).asInstanceOf[Phrase[T2]]

        case BinOp(op, lhs, rhs) =>
          BinOp(op, substitute(p1, p2, lhs), substitute(p1, p2, rhs)).asInstanceOf[Phrase[T2]]

        case _: Ident[_]   => in
        case _: IntLiteral => in
        case _: SkipPhrase => in
      }
    }
  }

  def evalFunction[T1 <: PhraseType,
                   T2 <: PhraseType](s: Store, p: Phrase[T1 -> T2]): (Phrase[T1] => Phrase[T2]) = {
    p match {
      case i: Ident[T1 -> T2] => throw new Exception("Don't know how to implement this")

      case l: Lambda[T1, T2] =>
        (arg: Phrase[T1]) => substitute(arg, l.param, in=l.body)

      case app: Apply[a, T1 -> T2] =>
        val fun = evalFunction(s, app.fun)
        evalFunction(s, fun(app.arg))

      case p1: Proj1[T1 -> T2, b] =>
        val pair = evalPair(s, p1.pair)
        evalFunction(s, pair._1)

      case p2: Proj2[a, T1 -> T2] =>
        val pair = evalPair(s, p2.pair)
        evalFunction(s, pair._2)

      case ifThenElse: IfThenElse[T1 -> T2] =>
        val cond = evalExp(s, ifThenElse.cond)
        if (cond == 0) {
          evalFunction(s, ifThenElse.thenP)
        } else {
          evalFunction(s, ifThenElse.elseP)
        }
    }
  }

  def evalPair[T1 <: PhraseType,
               T2 <: PhraseType](s: Store, p: Phrase[T1 x T2]): (Phrase[T1], Phrase[T2]) = {
    p match {
      case i: Ident[T1 x T2] => throw new Exception("Don't know how to implement this")

      case pair: Pair[T1, T2] => (pair.fst, pair.snd)

      case app: Apply[a, T1 x T2] =>
        val fun = evalFunction(s, app.fun)
        evalPair(s, fun(app.arg))

      case p1: Proj1[T1 x T2, b] =>
        val pair = evalPair(s, p1.pair)
        evalPair(s, pair._1)

      case p2: Proj2[a, T1 x T2] =>
        val pair = evalPair(s, p2.pair)
        evalPair(s, pair._2)

      case ifThenElse: IfThenElse[T1 x T2] =>
        val cond = evalExp(s, ifThenElse.cond)
        if (cond == 0) {
          evalPair(s, ifThenElse.thenP)
        } else {
          evalPair(s, ifThenElse.elseP)
        }
    }
  }

  def evalExp(s: Store, p: Phrase[ExpType]): Int = {
    p match {
      case v: Ident[ExpType] => s(v.name)

      case app: Apply[a, ExpType] =>
        val fun = evalFunction(s, app.fun)
        evalExp(s, fun(app.arg))

      case p1: Proj1[ExpType, b] =>
        val pair = evalPair(s, p1.pair)
        evalExp(s, pair._1)

      case p2: Proj2[a, ExpType] =>
        val pair = evalPair(s, p2.pair)
        evalExp(s, pair._2)

      case ifThenElse: IfThenElse[ExpType] =>
        val cond = evalExp(s, ifThenElse.cond)
        if (cond == 0) {
          evalExp(s, ifThenElse.thenP)
        } else {
          evalExp(s, ifThenElse.elseP)
        }

      case z: IntLiteral => z.i

      case op: BinOp =>
        op.op match {
          case BinOp.Op.ADD => evalExp(s, op.lhs) + evalExp(s, op.rhs)
          case BinOp.Op.SUB => evalExp(s, op.lhs) - evalExp(s, op.rhs)
          case BinOp.Op.MUL => evalExp(s, op.lhs) * evalExp(s, op.rhs)
          case BinOp.Op.DIV => evalExp(s, op.lhs) / evalExp(s, op.rhs)
          case BinOp.Op.MOD => evalExp(s, op.lhs) % evalExp(s, op.rhs)
        }
    }
  }

  def evalAcc(s: Store, p: Phrase[AccType]): String = {
    p match {
      case v: Ident[AccType] => v.name

      case app: Apply[a, AccType] =>
        val fun = evalFunction(s, app.fun)
        evalAcc(s, fun(app.arg))

      case p1: Proj1[AccType, b] =>
        val pair = evalPair(s, p1.pair)
        evalAcc(s, pair._1)

      case p2: Proj2[a, AccType] =>
        val pair = evalPair(s, p2.pair)
        evalAcc(s, pair._2)

      case ifThenElse: IfThenElse[AccType] =>
        val cond = evalExp(s, ifThenElse.cond)
        if (cond == 0) {
          evalAcc(s, ifThenElse.thenP)
        } else {
          evalAcc(s, ifThenElse.elseP)
        }
    }
  }

  def evalCommand(s: Store, p: Phrase[CommandType]): Store = {
    p match {
      case i: Ident[CommandType] =>
        throw new Exception("I don't known how to implement this")

      case app: Apply[a, CommandType] =>
        val fun = evalFunction(s, app.fun)
        evalCommand(s, fun(app.arg))

      case p1: Proj1[CommandType, b] =>
        val pair = evalPair(s, p1.pair)
        evalCommand(s, pair._1)

      case p2: Proj2[a, CommandType] =>
        val pair = evalPair(s, p2.pair)
        evalCommand(s, pair._2)

      case _: SkipPhrase => s

      case Seq(c1, c2) =>
        val s1 = evalCommand(s, c1)
        evalCommand(s1, c2)

      case New(f) => evalNew(s, f)

      case Assign(lhs, rhs) =>
        val varName = evalAcc(s, lhs)
        val value = evalExp(s, rhs)
        assert( s.contains(varName) )
        s + (varName -> value)

      case ifThenElse: IfThenElse[CommandType] =>
        val cond = evalExp(s, ifThenElse.cond)
        if (cond == 0) {
          evalCommand(s, ifThenElse.thenP)
        } else {
          evalCommand(s, ifThenElse.elseP)
        }

      case For(nP, bodyP) =>
        val body = evalFunction(s, bodyP)
        val n = evalExp(s, nP)
        var s1 = s
        for (i <- 0 until n) {
          s1 = evalCommand(s1, body(IntLiteral(i)))
        }
        s1
    }
  }

  def evalNew(s: Store, f: Phrase[ ExpType x AccType -> CommandType ]): Store = {
    f match {
      case v: Ident[ExpType x AccType -> CommandType] =>
        throw new Exception("I don't known how to implement this")

      case l: Lambda[ExpType x AccType, CommandType] =>
        val params = evalPair(s, l.param)
        val varName = evalAcc(s, params._2)
        val s1 = evalCommand(s + (varName -> 0), l.body)
        s1 - varName

      case app: Apply[a, ExpType x AccType -> CommandType] =>
        val fun = evalFunction(s, app.fun)
        evalNew(s, fun(app.arg))

      case p1: Proj1[ExpType x AccType -> CommandType, b] =>
        val pair = evalPair(s, p1.pair)
        evalNew(s, pair._1)

      case p2: Proj2[a, ExpType x AccType -> CommandType] =>
        val pair = evalPair(s, p2.pair)
        evalNew(s, pair._2)

      case ifThenElse: IfThenElse[ExpType x AccType -> CommandType] =>
        val cond = evalExp(s, ifThenElse.cond)
        if (cond == 0) {
          evalNew(s, ifThenElse.thenP)
        } else {
          evalNew(s, ifThenElse.elseP)
        }
    }
  }

}
