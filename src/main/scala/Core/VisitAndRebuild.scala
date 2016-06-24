package Core

import Core.OperationalSemantics.newName
import apart.arithmetic.ArithExpr

object VisitAndRebuild {

  class fun {
    def apply[T <: PhraseType](p: Phrase[T]): Result[Phrase[T]] = Continue(p, this)
    def apply(ae: ArithExpr): ArithExpr = ae
    def apply(dt: DataType): DataType = dt

    abstract class Result[+T]
    case class Stop[T <: PhraseType](p: Phrase[T]) extends Result[Phrase[T]]
    case class Continue[T <: PhraseType](p: Phrase[T], f: fun) extends Result[Phrase[T]]

  }

  def apply[T <: PhraseType](p: Phrase[T], f: fun): Phrase[T] = {
    f(p) match {
      case r: f.Stop[T]@unchecked => r.p
      case c: f.Continue[T]@unchecked =>
        val f = c.f
        val res = (c.p match {
          case _: IdentPhrase[_] => c.p
          case _: LiteralPhrase => c.p
          case l: LambdaPhrase[_, _] => LambdaPhrase(l.param, apply(l.body, f))
          case l: NatDependentLambdaPhrase[_] => NatDependentLambdaPhrase(l.x, apply(l.body, f))
          case app: ApplyPhrase[a, T] => ApplyPhrase(apply(app.fun, f), apply(app.arg, f))
          case app: NatDependentApplyPhrase[T] => NatDependentApplyPhrase(apply(app.fun, f), app.arg)
          case pair: PairPhrase[_, _] => PairPhrase(apply(pair.fst, f), apply(pair.snd, f))
          case p: Proj1Phrase[T, b] => Proj1Phrase(apply(p.pair, f))
          case p: Proj2Phrase[a, T] => Proj2Phrase(apply(p.pair, f))
          case i: IfThenElsePhrase[T] => IfThenElsePhrase(apply(i.cond, f), apply(i.thenP, f), apply(i.elseP, f))
          case u: UnaryOpPhrase => UnaryOpPhrase(u.op, apply(u.p, f))
          case b: BinOpPhrase => BinOpPhrase(b.op, apply(b.lhs, f), apply(b.rhs, f))
          case e: ExpPattern => e.visitAndRebuild(f)
          case a: AccPattern => a.visitAndRebuild(f)
          case c: IntermediateCommandPattern => c.visitAndRebuild(f)
        }).asInstanceOf[Phrase[T]]
        res.t = c.p.t
        res
    }
  }

  case class copyFun(map: scala.collection.immutable.Map[String, String])
    extends VisitAndRebuild.fun {

    override def apply[T2 <: PhraseType](p: Phrase[T2]): Result[Phrase[T2]] = {
      p match {
        case l: LambdaPhrase[a, b] =>
          val newParam = IdentPhrase[a](newName())
          newParam.t = l.param.t
          val newBody  =
            VisitAndRebuild(l.body, copyFun(map.updated(l.param.name, newParam.name)))
          val newL = LambdaPhrase(newParam, newBody).asInstanceOf[Phrase[T2]]
          newL.t = l.t
          Continue(newL, this)
        case i: IdentPhrase[T2] =>
          val newI = IdentPhrase[T2](map.getOrElse(i.name, i.name))
          newI.t = i.t
          Continue(newI, this)
        case _ => Continue(p, this)
      }
    }

  }

  def copy[T <: PhraseType](p: Phrase[T]): Phrase[T] = {
    VisitAndRebuild(p, copyFun(Map[String, String]()))
  }

}
