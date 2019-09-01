package idealised.DPIA.Phrases

import idealised.DPIA.Compilation.TranslationContext
import idealised.DPIA.FunctionalPrimitives.AsIndex
import idealised.DPIA.Lifting.{liftDependentFunction, liftFunction, liftPair}
import idealised.DPIA.Semantics.OperationalSemantics
import idealised.DPIA.Semantics.OperationalSemantics.{IndexData, NatData}
import idealised.DPIA.Types._
import idealised.DPIA._
import idealised.{DPIA, SurfaceLanguage}
import idealised.SurfaceLanguage.Operators
import lift.arithmetic.{NamedVar, RangeAdd, StartFromRange}

import scala.language.{postfixOps, reflectiveCalls}

sealed trait Phrase[T <: PhraseType] {
  val t: T // TODO? perform type checking at the same time
}

final case class Identifier[T <: PhraseType](name: String, `type`: T)
  extends Phrase[T] {

  override val t: T = `type`
  override def toString: String = s"($name : ${`type`})"
}

final case class Lambda[T1 <: PhraseType, T2 <: PhraseType](param: Identifier[T1], body: Phrase[T2])
  extends Phrase[T1 ->: T2] {

  override val t: T1 ->: T2 = param.t ->: body.t
  override def toString: String = s"λ$param. $body"
}

final case class Apply[T1 <: PhraseType, T2 <: PhraseType](fun: Phrase[T1 ->: T2], arg: Phrase[T1])
  extends Phrase[T2] {

  TypeCheck.check(fun.t.inT, arg.t) // FIXME: redundant with type checking
  override val t: T2 = fun.t.outT
  override def toString: String = s"($fun $arg)"
}

final case class DepLambda[K <: Kind, T <: PhraseType](x: K#I, body: Phrase[T])
  extends Phrase[K `()->:` T] {
  override val t: DepFunType[K, T] = DepFunType[K, T](x, body.t)
  override def toString: String = s"Λ(${x.name} : ${x.getClass.getName.dropWhile(_!='$').drop(1).takeWhile(_!='$')}). $body"
}

object DepLambda {
  def apply[K <: Kind](x: K#I): Object {
    def apply[T <: PhraseType](body: Phrase[T]): DepLambda[K, T]
  } = new {
    def apply[T <: PhraseType](body: Phrase[T]) = DepLambda(x, body)
  }
}

final case class DepApply[K <: Kind, T <: PhraseType](fun: Phrase[K `()->:` T], arg: K#T)
  extends Phrase[T] {

  override val t: T = PhraseType.substitute(arg, `for`=fun.t.x, in=fun.t.t).asInstanceOf[T]
  override def toString: String = s"($fun $arg)"
}

final case class LetNat[T1 <: PhraseType, T2 <: PhraseType](binder:LetNatIdentifier,
                                                            defn:Phrase[T1],
                                                            body:Phrase[T2]) extends Phrase[T2] {
  override val t: T2 = body.t
}

final case class Pair[T1 <: PhraseType, T2 <: PhraseType](fst: Phrase[T1], snd: Phrase[T2])
  extends Phrase[T1 x T2] {

  override val t: x[T1, T2] = fst.t x snd.t
}

final case class Proj1[T1 <: PhraseType, T2 <: PhraseType](pair: Phrase[T1 x T2])
  extends Phrase[T1] {

  override val t: T1 = pair.t.t1
}

final case class Proj2[T1 <: PhraseType, T2 <: PhraseType](pair: Phrase[T1 x T2])
  extends Phrase[T2] {

  override val t: T2 = pair.t.t2
}

final case class IfThenElse[T <: PhraseType](cond: Phrase[ExpType], thenP: Phrase[T], elseP: Phrase[T])
  extends Phrase[T] {

  // FIXME: redundant with type checking
  TypeCheck.check(thenP.t, elseP.t)
  override val t: T = thenP.t
}

final case class UnaryOp(op: SurfaceLanguage.Operators.Unary.Value, p: Phrase[ExpType])
  extends Phrase[ExpType] {

  override val t: ExpType = p.t
}

final case class BinOp(op: SurfaceLanguage.Operators.Binary.Value, lhs: Phrase[ExpType], rhs: Phrase[ExpType])
  extends Phrase[ExpType] {

  override val t: ExpType =
    op match {
      case Operators.Binary.GT |
           Operators.Binary.LT |
           Operators.Binary.EQ => exp"[$bool, $read]"
      case _ => (lhs.t.dataType, rhs.t.dataType) match {
        case (t1, t2) if t1 == t2 => ExpType(t1, read)
        case (lhsT, rhsT) =>
          throw new TypeException(s"Failed type checking: found" +
            s"`$lhsT' and `$rhsT', but expected them to match.")
      }
    }
}

final case class Literal(d: OperationalSemantics.Data)
  extends Phrase[ExpType] {

  assert(!d.isInstanceOf[NatData])
  assert(!d.isInstanceOf[IndexData])

  override val t: ExpType = ExpType(d.dataType, read)
}

final case class Natural(d: Nat) extends Phrase[ExpType] {
  override val t: ExpType = ExpType(NatType, read)
}

object Phrase {
  // substitutes `phrase` for `for` in `in`, i.e. in [ phrase / for ]
  def substitute[T1 <: PhraseType, T2 <: PhraseType](ph: Phrase[T1],
                                                     `for`: Phrase[T1],
                                                     in: Phrase[T2]): Phrase[T2] = {
    object Visitor extends VisitAndRebuild.Visitor {
      override def phrase[T <: PhraseType](p: Phrase[T]): Result[Phrase[T]] = {
        p match {
          case `for` => Stop(ph.asInstanceOf[Phrase[T]])
          case _ => Continue(p, this)
        }
      }
    }

    VisitAndRebuild(in, Visitor)
  }

  def substitute[T2 <: PhraseType](substitutionMap: Map[Phrase[_], Phrase[_]],
                                   in: Phrase[T2]): Phrase[T2] = {

    object Visitor extends VisitAndRebuild.Visitor {
      override def phrase[T <: PhraseType](p: Phrase[T]): Result[Phrase[T]] = {
        if (substitutionMap.isDefinedAt(p)) {
          Stop(substitutionMap(p).asInstanceOf[Phrase[T]])
        } else {
          Continue(p, this)
        }
      }
    }

    VisitAndRebuild(in, Visitor)
  }


  object Internal {
    case class TransientNat(n: Nat,
                            identifiers: Map[NamedVar, Phrase[ExpType]]) {
      def map(f: Nat => Nat): TransientNat =
        TransientNat(f(n), identifiers)

      def bind(f: Nat => TransientNat): TransientNat = {
        val other = f(n)
        TransientNat(other.n, identifiers ++ other.identifiers)
      }
    }

    object TransientNat {
      def apply(n: Nat): TransientNat = TransientNat(n, Map.empty)
    }

    // returns a transient nat corresponding to an exp[nat] or exp[idx[n]]
    // it allows to leverage nat simplification but should not be used in any exp[_] before being converted back
    // this is because expression Identifiers are temporarily embedded as NamedVars
    def transientNatFromExpr(p: Phrase[ExpType]): TransientNat = {
      import idealised.DPIA.FunctionalPrimitives.IndexAsNat

      p match {
        case i: Identifier[ExpType] => p.t match {
          case ExpType(IndexType(n), _) =>
            val v = NamedVar(s"id_${i.name}", RangeAdd(0, n, 1))
            TransientNat(v, Map(v -> IndexAsNat(n, i)))
          case ExpType(NatType, _) =>
            val v = NamedVar(s"id_${i.name}", RangeAdd(0, lift.arithmetic.PosInf, 1))
            TransientNat(v, Map(v -> i))
          case _ => throw new Exception("This should never happen")
        }
        case Natural(n) => TransientNat(n)
        case prim: ExpPrimitive => prim match {
          //TODO can we use our knowledge of n somehow?
          case AsIndex(n, e) => transientNatFromExpr(e)
          case IndexAsNat(_, e) => transientNatFromExpr(e)
          case _ => throw new Exception("This should never happen")
        }
        case UnaryOp(op, e) =>
          transientNatFromExpr(e).map(unOpToNat(op, _))
        case BinOp(op, lhs, rhs) =>
          transientNatFromExpr(lhs).bind(l =>
            transientNatFromExpr(rhs).map(r =>
              binOpToNat(op, l, r)))
        case Apply(fun, arg) => transientNatFromExpr(liftFunction(fun).reducing(arg))
        case DPIA.Phrases.IfThenElse(_, _, _) => throw new Exception("This should never happen")
        case Literal(lit) => lit match {
          case _ => throw new Exception("This should never happen")
          // NatData is Natural
          // IndexData is AsIndex
        }
        case DepApply(fun, arg) => (fun, arg) match {
          case (f, a: Nat) =>
            transientNatFromExpr(liftDependentFunction[NatKind, ExpType](f.asInstanceOf[Phrase[NatKind `()->:` ExpType]])(a))
          case _ => ???
        }
        case Proj1(pair) => transientNatFromExpr(liftPair(pair)._1)
        case Proj2(pair) => transientNatFromExpr(liftPair(pair)._2)
        case LetNat(_, _, _) => ???
      }
    }

    def binOpToNat(op: Operators.Binary.Value, n1: Nat, n2: Nat): Nat = {
      import Operators.Binary._

      op match {
        case ADD => n1 + n2
        case SUB => n1 - n2
        case MUL => n1 * n2
        case DIV => n1 / n2
        case MOD => n1 % n2

        case _ => ???
      }
    }

    def unOpToNat(op: Operators.Unary.Value, n: Nat): Nat = ???

    def exprFromTransientNat(tn: TransientNat): Phrase[ExpType] =
      eitherAsExpr(reconstructTransientNat(tn))

    private def reconstructTransientNat(tn: TransientNat): Either[Nat, Phrase[ExpType]] = {
      import lift.arithmetic._

      val reconstruct = (n: Nat) =>
        reconstructTransientNat(TransientNat(n, tn.identifiers))
      tn.n match {
        case v: NamedVar => tn.identifiers.get(v).map(Right(_)).getOrElse(Left(v))
        case c: Cst => Left(c)
        case Sum(ps) => reconstructTransientBinOpFold(
          Operators.Binary.ADD, reconstruct, ps)
        case Prod(ps) => reconstructTransientBinOpFold(
          Operators.Binary.MUL, reconstruct, ps)
        case IntDiv(n, d) =>
          reconstructTransientBinOp(Operators.Binary.DIV,
            reconstruct(n), reconstruct(d))
        case Mod(x, m) =>
          reconstructTransientBinOp(Operators.Binary.MOD,
            reconstruct(x), reconstruct(m))
        case Pow(b, e) =>
          Left(Pow(reconstruct(b).left.get, reconstruct(e).left.get))
        case Log(b, x) =>
          Left(Log(reconstruct(b).left.get, reconstruct(x).left.get))
        case _ => throw new Exception(s"did not expect ${tn.n}")
      }
    }

    private def reconstructTransientBinOpFold(op: Operators.Binary.Value,
                                              reconstruct: Nat => Either[Nat, Phrase[ExpType]],
                                              seq: Seq[Nat]): Either[Nat, Phrase[ExpType]] =
      seq.map(reconstruct).reduce[Either[Nat, Phrase[ExpType]]]({
        case (a, b) => reconstructTransientBinOp(op, a, b)
      })

    private def reconstructTransientBinOp(op: Operators.Binary.Value,
                                          a: Either[Nat, Phrase[ExpType]],
                                          b: Either[Nat, Phrase[ExpType]]): Either[Nat, Phrase[ExpType]] = {
      (a, b) match {
        case (Left(na), Left(nb)) => Left(binOpToNat(op, na, nb))
        case _ => Right(BinOp(op, eitherAsExpr(a), eitherAsExpr(b)))
      }
    }

    private def eitherAsExpr(e: Either[Nat, Phrase[ExpType]]): Phrase[ExpType] =
      e match {
        case Left(n) => Natural(n)
        case Right(e) => e
      }
  }
}

sealed trait Primitive[T <: PhraseType] extends Phrase[T] {
  def prettyPrint: String

  def xmlPrinter: xml.Elem

  def visitAndRebuild(f: VisitAndRebuild.Visitor): Phrase[T]
}

abstract class ExpPrimitive extends Primitive[ExpType] {
  def eval(s: OperationalSemantics.Store): OperationalSemantics.Data

  def fedeTranslation(env: Map[Identifier[ExpType], Identifier[AccType]])
                     (C: Phrase[AccType ->: AccType]) : Phrase[AccType] = ???

  def acceptorTranslation(A: Phrase[AccType])
                         (implicit context: TranslationContext): Phrase[CommType]

  def mapAcceptorTranslation(f: Phrase[ExpType ->: ExpType],
                             A: Phrase[AccType])
                            (implicit context: TranslationContext): Phrase[CommType]

  def continuationTranslation(C: Phrase[ExpType ->: CommType])
                             (implicit context: TranslationContext): Phrase[CommType]
}

trait AccPrimitive extends Primitive[AccType] {
  def eval(s: OperationalSemantics.Store): OperationalSemantics.AccIdentifier
}

trait CommandPrimitive extends Primitive[CommType] {
  def eval(s: OperationalSemantics.Store): OperationalSemantics.Store
}