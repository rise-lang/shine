package shine.DPIA.Types

import rise.core.types.{DepFunType => _, FunType => _, Type => _, TypeIdentifier => _, TypePlaceholder => _, _}
import rise.core.substitute.{natInType => substituteNatInType, typeInType => substituteTypeInType}
import rise.core.types.DataType.DataTypeIdentifier
import shine.DPIA.Phrases._
import shine.DPIA._
import shine.DPIA.primitives.functional.NatAsIndex

object substitute {
  def apply[T, I, U <: PhraseType](kind: Kind[T, I], x: T, `for`: I, in: Phrase[U]): Phrase[U] =
    (x, `for`) match {
      case (dt: DataType, forDt: DataTypeIdentifier) => substitute(dt, forDt, in)
      case (n: Nat, forN: NatIdentifier) => substitute(n, forN, in)
      case (a: AddressSpace, forA: AddressSpaceIdentifier) => substitute(a, forA, in)
      case (a: Access, forA: AccessIdentifier) => substitute(a, forA, in)
      case (n2n: NatToNat, forN2N: NatToNatIdentifier) => substitute(n2n, forN2N, in)
      case (n2d: NatToData, fotN2D: NatToDataIdentifier) => ??? //substitute(n2d, forN2D, in)
      case _ => throw new Exception(s"could not substitute $x for ${`for`} in $in")
    }

  def apply[T, I](kind: Kind[T, I], x: T, `for`: I, in: PhraseType): PhraseType =
    (x, `for`) match {
      case (dt: DataType, forDt: DataTypeIdentifier) => substitute(dt, forDt, in)
      case (n: Nat, forN: NatIdentifier) => substitute(n, forN, in)
      case (a: AddressSpace, forA: AddressSpaceIdentifier) => substitute(a, forA, in)
      case (a: Access, forA: AccessIdentifier) => ??? //substitute(a, forA, in)
      case (n2n: NatToNat, forN2N: NatToNatIdentifier) => substitute(n2n, forN2N, in)
      case (n2d: NatToData, forN2D: NatToDataIdentifier) => ??? //substitute(n2d, forN2D, in)
      case _ => throw new Exception(s"could not substitute $x for ${`for`} in $in")
    }

  def apply[T <: PhraseType](dt: DataType,
                             `for`: DataTypeIdentifier,
                             in: Phrase[T]): Phrase[T] = {

    object Visitor extends Phrases.VisitAndRebuild.Visitor {
      override def data[DT <: DataType](in: DT): DT = substituteTypeInType(dt, `for`, in)
    }

    val p = Phrases.VisitAndRebuild(in, Visitor)
    TypeCheck(p)
    p

  }

  def apply(dt: DataType, `for`: DataType, in: PhraseType): PhraseType = {
    in match {
      case b: BasePhraseType => b match {
        case e: ExpType => ExpType(substituteTypeInType(dt, `for`, e.dataType), e.accessType)
        case a: AccType => AccType(substituteTypeInType(dt, `for`, a.dataType))
      }
      case c: CommType => c
      case p: PhrasePairType[_, _] =>
        PhrasePairType(substitute(dt, `for`, p.t1), substitute(dt, `for`, p.t2))
      case f: FunType[_, _] =>
        FunType(substitute(dt, `for`, f.inT), substitute(dt, `for`, f.outT))
      case pf: PassiveFunType[_, _] =>
        PassiveFunType(substitute(dt, `for`, pf.inT), substitute(dt, `for`, pf.outT))
      case df: DepFunType[_, _] =>
        DepFunType(df.kind, df.x, substitute(dt, `for`, df.t))
    }
  }

  def apply[T <: PhraseType](ae: Nat,
                             `for`: NatIdentifier,
                             in: Phrase[T]): Phrase[T] = {

    object Visitor extends Phrases.VisitAndRebuild.Visitor {
      override def phrase[T2 <: PhraseType](p: Phrase[T2]): Result[Phrase[T2]] = {
        p match {
          case Identifier(name, _) =>
            if (`for`.name == name) {
              Stop(NatAsIndex(ae.max, Natural(ae)).asInstanceOf[Phrase[T2]])
            } else {
              Continue(p, this)
            }
          case Natural(n) =>
            Stop(Natural(Nat.substitute(ae, `for`, n)).asInstanceOf[Phrase[T2]])
          case _ =>
            Continue(p, this)
        }
      }

      override def nat[N <: Nat](n: N): N =
        Nat.substitute(ae, `for`, in = n)

      override def data[DT <: DataType](dt: DT): DT =
        substituteNatInType(ae, `for`, in = dt)
    }

    val p = Phrases.VisitAndRebuild(in, Visitor)
    TypeCheck(p)
    p

  }


  def apply(n: Nat, `for`: NatIdentifier, in: PhraseType): PhraseType = {
    in match {
      case b: BasePhraseType => b match {
        case e: ExpType => ExpType(substituteNatInType(n, `for`, e.dataType), e.accessType)
        case a: AccType => AccType(substituteNatInType(n, `for`, a.dataType))
      }
      case c: CommType => c
      case p: PhrasePairType[_, _] =>
        PhrasePairType(substitute(n, `for`, p.t1), substitute(n, `for`, p.t2))
      case f: FunType[_, _] =>
        FunType(substitute(n, `for`, f.inT), substitute(n, `for`, f.outT))
      case pf: PassiveFunType[_, _] =>
        PassiveFunType(substitute(n, `for`, pf.inT), substitute(n, `for`, pf.outT))
      case df: DepFunType[_, _] =>
        DepFunType(df.kind, df.x, substitute(n, `for`, df.t))
    }
  }

  def apply[T <: PhraseType](addr: AddressSpace,
                             `for`: AddressSpaceIdentifier,
                             in: Phrase[T]): Phrase[T] = {
    object Visitor extends Phrases.VisitAndRebuild.Visitor {
      override def addressSpace(a: AddressSpace): AddressSpace =
        if (a == `for`) {
          addr
        } else {
          a
        }
    }
    Phrases.VisitAndRebuild(in, Visitor)
  }

  def apply(addr: AddressSpace,
            `for`: AddressSpaceIdentifier,
            in: PhraseType): PhraseType = {
    // address spaces do not appear syntactically in phrase types
    in
  }

  def apply[T <: PhraseType](n2n: NatToNat,
                             `for`: NatToNatIdentifier,
                             in: Phrase[T]): Phrase[T] = {
    object Visitor extends Phrases.VisitAndRebuild.Visitor {
      override def natToNat(ft: NatToNat): NatToNat = ft match {
        case i: NatToNatIdentifier if i == `for` => n2n
        case _ => ft
      }
    }
    Phrases.VisitAndRebuild(in, Visitor)
  }

  def apply(n2n: NatToNat,
            `for`: NatToNatIdentifier,
            in: PhraseType): PhraseType = {
    // NatToNat does not appear syntactically in phrase types
    in
  }

  def apply[T <: PhraseType](acc: Access,
                             `for`: AccessIdentifier,
                             in: Phrase[T]): Phrase[T] = {
    object Visitor extends Phrases.VisitAndRebuild.Visitor {
      override def access(w: Access): Access = if (w == `for`) acc else w
    }
    Phrases.VisitAndRebuild(in, Visitor)
  }

  def apply(acc: Access,
            `for`: AccessIdentifier,
            in: PhraseType): PhraseType = {
    object Visitor extends Phrases.VisitAndRebuild.Visitor {
      override def access(w: Access): Access = if (w == `for`) acc else w
    }
    Phrases.VisitAndRebuild.visitPhraseTypeAndRebuild(in, Visitor)
  }
}
