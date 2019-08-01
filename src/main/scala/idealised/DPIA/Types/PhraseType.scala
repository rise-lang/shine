package idealised.DPIA.Types

import idealised.DPIA.FunctionalPrimitives.AsIndex
import idealised.DPIA.Phrases._
import idealised.DPIA._

sealed trait PhraseType

sealed abstract class BasePhraseTypes extends PhraseType

final case class ExpType(dataType: DataType, accessType: AccessType) extends BasePhraseTypes {
  override def toString = s"exp[$dataType, $accessType]"
}

final case class AccType(dataType: DataType) extends BasePhraseTypes {
  override def toString = s"acc[$dataType]"
}

sealed case class CommType() extends PhraseType {
  override def toString = "comm"
}

object comm extends CommType

final case class PairType[T1 <: PhraseType, T2 <: PhraseType](t1: T1, t2: T2)
  extends PhraseType {
  override def toString = s"$t1 x $t2"
}

final case class FunType[T <: PhraseType, +R <: PhraseType](inT: T, outT: R)
  extends PhraseType {
  override def toString = s"($inT) -> $outT"
}

final case class PassiveFunType[T <: PhraseType, +R <: PhraseType](inT: T, outT: R)
  extends PhraseType {
  override def toString = s"($inT) ->p $outT"
}

final case class DepFunType[K <: Kind, +R <: PhraseType](x: K#I, t: R)
  extends PhraseType {
  override def toString = s"(${x.name}: ${Kind.formatKindName(x.getClass.getName)}) -> $t"
}

object PhraseType {

  def substitute[K <: Kind, T <: PhraseType](x: K#T, `for`: K#I, in: Phrase[T]): Phrase[T] = (x, `for`) match {
    case (dt: DataType, forDt: DataTypeIdentifier)  => substitute(dt, forDt, in)
    case (n: Nat, forN: NatIdentifier)              => substitute(n, forN, in)
    case (a: AddressSpace, forA: AddressSpaceIdentifier) => substitute(a, forA, in)
  }

  def substitute[K <: Kind](x: K#T, `for`: K#I, in: PhraseType): PhraseType = (x, `for`) match {
    case (dt: DataType, forDt: DataTypeIdentifier)  => substitute(dt, forDt, in)
    case (n: Nat, forN: NatIdentifier)              => substitute(n, forN, in)
    case (a: AddressSpace, forA: AddressSpaceIdentifier) => substitute(a, forA, in)
  }

  def substitute[T <: PhraseType](dt: DataType,
                                  `for`: DataTypeIdentifier,
                                  in: Phrase[T]): Phrase[T] = {

    object Visitor extends Phrases.VisitAndRebuild.Visitor {
      override def data[DT <: DataType](in: DT): DT = DataType.substitute(dt, `for`, in)
    }

    val p = Phrases.VisitAndRebuild(in, Visitor)
    TypeCheck(p)
    p

  }

  def substitute(dt: DataType, `for`: DataType, in: PhraseType): PhraseType = {
    in match {
      case b: BasePhraseTypes => b match {
        case e: ExpType => ExpType(DataType.substitute(dt, `for`, e.dataType), e.accessType)
        case a: AccType => AccType(DataType.substitute(dt, `for`, a.dataType))
      }
      case c: CommType => c
      case p: PairType[_, _] =>
        PairType(substitute(dt, `for`, p.t1), substitute(dt, `for`, p.t2))
      case f: FunType[_, _] =>
        FunType(substitute(dt, `for`, f.inT), substitute(dt, `for`, f.outT))
      case pf: PassiveFunType[_, _] =>
        PassiveFunType(substitute(dt, `for`, pf.inT), substitute(dt, `for`, pf.outT))
      case df: DepFunType[_, _] =>
        DepFunType(df.x, substitute(dt, `for`, df.t))
    }
  }

  def substitute[T <: PhraseType](ae: Nat,
                                  `for`: NatIdentifier,
                                  in: Phrase[T]): Phrase[T] = {

    object Visitor extends Phrases.VisitAndRebuild.Visitor {
      override def phrase[T2 <: PhraseType](p: Phrase[T2]): Result[Phrase[T2]] = {
        p match {
          case Identifier(name, _) =>
            if (`for`.name == name) {
              Stop(AsIndex(ae.max, Natural(ae)).asInstanceOf[Phrase[T2]])
            } else {
              Continue(p, this)
            }
          case Natural(n) =>
            Stop(Natural(Nat.substitute(ae, `for`, n)).asInstanceOf[Phrase[T2]])
          case _ =>
            Continue(p, this)
        }
      }

      override def nat[N <: Nat](n: N): N = Nat.substitute(ae, `for`, in=n)

      override def data[DT <: DataType](dt: DT): DT = DataType.substitute(ae, `for`, in=dt)
    }

    val p = Phrases.VisitAndRebuild(in, Visitor)
    TypeCheck(p)
    p

  }


  def substitute(ae: Nat, `for`: Nat, in: PhraseType): PhraseType = {
    in match {
      case b: BasePhraseTypes => b match {
        case e: ExpType => ExpType(DataType.substitute(ae, `for`, e.dataType), e.accessType)
        case a: AccType => AccType(DataType.substitute(ae, `for`, a.dataType))
      }
      case c: CommType => c
      case p: PairType[_, _] =>
        PairType(substitute(ae, `for`, p.t1), substitute(ae, `for`, p.t2))
      case f: FunType[_, _] =>
        FunType(substitute(ae, `for`, f.inT), substitute(ae, `for`, f.outT))
      case pf: PassiveFunType[_, _] =>
        PassiveFunType(substitute(ae, `for`, pf.inT), substitute(ae, `for`, pf.outT))
      case df: DepFunType[_, _] =>
        DepFunType(df.x, substitute(ae, `for`, df.t))
    }
  }

  def substitute[T <: PhraseType](addr: AddressSpace,
                                  `for`: AddressSpaceIdentifier,
                                  in: Phrase[T]): Phrase[T] = {

    object Visitor extends Phrases.VisitAndRebuild.Visitor {
      override def addressSpace(a: AddressSpace): AddressSpace =
        if (a == `for`) { addr } else { a }
    }

    Phrases.VisitAndRebuild(in, Visitor)
  }

  def substitute(addr: AddressSpace,
                 `for`: AddressSpaceIdentifier,
                 in: PhraseType): PhraseType = {
    in
  }
}
