package idealised.DPIA.Types

import idealised.DPIA.Phrases._
import idealised.DPIA.Semantics.OperationalSemantics.IndexData
import idealised.DPIA._
import lift.arithmetic.{ArithExpr, Var}

sealed trait PhraseType

sealed abstract class BasePhraseTypes extends PhraseType

final case class ExpType(dataType: DataType) extends BasePhraseTypes {
  override def toString = s"exp[$dataType]"
}

final case class AccType(dataType: DataType) extends BasePhraseTypes {
  override def toString = s"acc[$dataType]"
}

sealed case class CommandType() extends PhraseType {
  override def toString = "comm"
}

object comm extends CommandType

final case class PairType[T1 <: PhraseType, T2 <: PhraseType](t1: T1, t2: T2)
  extends PhraseType {
  override def toString = s"$t1 x $t2"
}

final case class FunctionType[T1 <: PhraseType, T2 <: PhraseType](inT: T1, outT: T2)
  extends PhraseType {
  override def toString = s"$inT -> $outT"
}

final case class PassiveFunctionType[T1 <: PhraseType, T2 <: PhraseType](inT: T1, outT: T2)
  extends PhraseType {
  override def toString = s"$inT ->p $outT"
}

final case class NatDependentFunctionType[T <: PhraseType](x: NatIdentifier, t: T)
  extends PhraseType {
  override def toString = s"($x : Nat) -> $t"
}

final case class TypeDependentFunctionType[T <: PhraseType](x: DataTypeIdentifier, t: T)
  extends PhraseType {
  override def toString = s"($x : dt) -> $t"
}

object PhraseType {

  def substitute[T <: PhraseType](dt: DataType,
                                  `for`: DataTypeIdentifier,
                                  in: Phrase[T]): Phrase[T] = {

    object Visitor extends Phrases.VisitAndRebuild.Visitor {
      override def apply[DT <: DataType](in: DT): DT = substitute(dt, `for`, in)
    }

    val p = Phrases.VisitAndRebuild(in, Visitor)
    TypeCheck(p)
    p

  }

  private def substitute[T <: DataType](dt: DataType, `for`: DataType, in: T): T = {
    if (`for` == in) {
      dt.asInstanceOf[T]
    } else {
      (in match {
        case _: BasicType => in
        case a: ArrayType => ArrayType(a.size, substitute(dt, `for`, a.elemType))
        case r: RecordType => RecordType(substitute(dt, `for`, r.fst), substitute(dt, `for`, r.snd))
      }).asInstanceOf[T]
    }
  }

  def substitute(dt: DataType, `for`: DataType, in: PhraseType): PhraseType = {
    in match {
      case b: BasePhraseTypes => b match {
        case e: ExpType => ExpType(substitute(dt, `for`, e.dataType))
        case a: AccType => AccType(substitute(dt, `for`, a.dataType))
      }
      case c: CommandType => c
      case p: PairType[_, _] =>
        PairType(substitute(dt, `for`, p.t1), substitute(dt, `for`, p.t2))
      case f: FunctionType[_, _] =>
        FunctionType(substitute(dt, `for`, f.inT), substitute(dt, `for`, f.outT))
      case pf: PassiveFunctionType[_, _] =>
        PassiveFunctionType(substitute(dt, `for`, pf.inT), substitute(dt, `for`, pf.outT))
      case nf: NatDependentFunctionType[_] =>
        NatDependentFunctionType(nf.x, substitute(dt, `for`, nf.t))
      case tf: TypeDependentFunctionType[_] =>
        TypeDependentFunctionType(tf.x, substitute(dt, `for`, tf.t))
    }
  }

  def substitute[T <: PhraseType](ae: Nat,
                                  `for`: NatIdentifier,
                                  in: Phrase[T]): Phrase[T] = {

    object Visitor extends Phrases.VisitAndRebuild.Visitor {
      override def apply[T2 <: PhraseType](p: Phrase[T2]): Result[Phrase[T2]] = {
        p match {
          case Identifier(name, _) =>
            if (`for`.name == name) {
              Stop(Literal(IndexData(ae, IndexType(ae.max))).asInstanceOf[Phrase[T2]])
            } else {
              Continue(p, this)
            }
          case Literal(IndexData(index, IndexType(size))) =>
            val newIndex = substitute(ae, `for`, in = index)
            val newSize = substitute(ae, `for`, in = size)
            Stop(Literal(IndexData(newIndex, IndexType(newSize))).asInstanceOf[Phrase[T2]])
          case _ =>
            Continue(p, this)
        }
      }

      override def apply(e: Nat): Nat = substitute(ae, `for`, e)

      override def apply[DT <: DataType](dt: DT): DT = substitute(ae, `for`, dt)
    }

    val p = Phrases.VisitAndRebuild(in, Visitor)
    TypeCheck(p)
    p

  }

  private def substitute[T <: DataType](ae: Nat, `for`: Nat, in: T): T = {
    (in match {
      case i: IndexType => IndexType(ArithExpr.substitute(i.size, Map((`for`, ae))))
      case b: BasicType => b
      case a: ArrayType =>
        ArrayType(ArithExpr.substitute(a.size, Map((`for`, ae))),
          substitute(ae, `for`, a.elemType))
      case r: RecordType =>
        RecordType(substitute(ae, `for`, r.fst), substitute(ae, `for`, r.snd))
    }).asInstanceOf[T]
  }

  private def substitute(ae: Nat, `for`: NatIdentifier, in: Nat): Nat = {
    in.visitAndRebuild {
      case v: Var =>
        if (`for`.name == v.name) {
          ae
        } else {
          v
        }
      case e => e
    }
  }

  def substitute(ae: Nat, `for`: Nat, in: PhraseType): PhraseType = {
    in match {
      case b: BasePhraseTypes => b match {
        case e: ExpType => ExpType(substitute(ae, `for`, e.dataType))
        case a: AccType => AccType(substitute(ae, `for`, a.dataType))
      }
      case c: CommandType => c
      case p: PairType[_, _] =>
        PairType(substitute(ae, `for`, p.t1), substitute(ae, `for`, p.t2))
      case f: FunctionType[_, _] =>
        FunctionType(substitute(ae, `for`, f.inT), substitute(ae, `for`, f.outT))
      case pf: PassiveFunctionType[_, _] =>
        PassiveFunctionType(substitute(ae, `for`, pf.inT), substitute(ae, `for`, pf.outT))
      case nf: NatDependentFunctionType[_] =>
        NatDependentFunctionType(nf.x, substitute(ae, `for`, nf.t))
      case tf: TypeDependentFunctionType[_] =>
        TypeDependentFunctionType(tf.x, substitute(ae, `for`, tf.t))
    }
  }

}
