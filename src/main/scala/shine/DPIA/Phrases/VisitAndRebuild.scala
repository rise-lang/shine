package shine.DPIA.Phrases

import shine.DPIA.Types._
import shine.DPIA._

object VisitAndRebuild {

  class Visitor {
    def phrase[T <: PhraseType](p: Phrase[T]): Result[Phrase[T]] = Continue(p, this)

    def nat[N <: Nat](n: N): N = n

    def data[T <: DataType](dt: T): T = dt

    def natToNat[N <: NatToNat](ft: N): N = (ft match {
      case NatToNatLambda(n, body) => NatToNatLambda(nat(n), nat(body))
      case i: NatToNatIdentifier => i
    }).asInstanceOf[N]

    def natToData[N <: NatToData](ft: N): N = (ft match {
      case NatToDataLambda(n, body) => NatToDataLambda(nat(n), data(body))
      case i: NatToDataIdentifier => i
    }).asInstanceOf[N]

    def access(w: AccessType): AccessType = w

    def addressSpace(a: AddressSpace): AddressSpace = a

    abstract class Result[+T]

    case class Stop[T <: PhraseType](p: Phrase[T]) extends Result[Phrase[T]]

    case class Continue[T <: PhraseType](p: Phrase[T], v: Visitor) extends Result[Phrase[T]]

  }

  def apply[T <: PhraseType](phrase: Phrase[T], v: Visitor): Phrase[T] = {
    v.phrase(phrase) match {
      case r: v.Stop[T]@unchecked => r.p
      case c: v.Continue[T]@unchecked =>
        val v = c.v
        (c.p match {
          case i: Identifier[T] =>
            Identifier(i.name, visitPhraseTypeAndRebuild(i.t, v))

          case Lambda(x, p) =>
            apply(x, v) match {
              case newX: Identifier[_] => Lambda(newX, apply(p, v))
              case badX => throw new Exception(s"${badX} is not an identifier")
            }

          case Apply(p, q) =>
            Apply(apply(p, v), apply(q, v))

          case DepLambda(a, p) => a match {
            case n: NatIdentifier =>
              DepLambda[NatKind, PhraseType](
                NatIdentifier(
                  v.nat(n).asInstanceOf[arithexpr.arithmetic.NamedVar].name),
                apply(p, v))
            case dt: DataTypeIdentifier =>
              DepLambda[DataKind, PhraseType](
                v.data(dt).asInstanceOf[DataTypeIdentifier],
                apply(p, v))
            case ad: AddressSpaceIdentifier =>
              DepLambda[AddressSpaceKind, PhraseType](
                v.addressSpace(ad).asInstanceOf[AddressSpaceIdentifier],
                apply(p, v))
            case ac: AccessTypeIdentifier =>
              DepLambda[AccessKind, PhraseType](
                v.access(ac).asInstanceOf[AccessTypeIdentifier],
                apply(p, v))
            case n2n: NatToNatIdentifier =>
              DepLambda[NatToNatKind, PhraseType](
                v.natToNat(n2n).asInstanceOf[NatToNatIdentifier],
                apply(p, v))
            case n2d: NatToDataIdentifier =>
              DepLambda[NatToDataKind, PhraseType](
                v.natToData(n2d).asInstanceOf[NatToDataIdentifier],
                apply(p, v))
            case _ => ???
          }

          case DepApply(p, a) => a match {
            case n: Nat =>
              DepApply[NatKind, T](
                apply(p, v).asInstanceOf[Phrase[NatKind `()->:` T]],
                v.nat(n))
            case dt: DataType =>
              DepApply[DataKind, T](
                apply(p, v).asInstanceOf[Phrase[DataKind `()->:` T]],
                visitDataTypeAndRebuild(dt, v))
            case ad: AddressSpace =>
              DepApply[AddressSpaceKind, T](
                apply(p, v).asInstanceOf[Phrase[AddressSpaceKind `()->:` T]],
                v.addressSpace(ad))
            case ac: AccessType =>
              DepApply[AccessKind, T](
                apply(p, v).asInstanceOf[Phrase[AccessKind `()->:` T]],
                v.access(ac))
            case n2n: NatToNat =>
              DepApply[NatToNatKind, T](
                apply(p, v).asInstanceOf[Phrase[NatToNatKind `()->:` T]],
                v.natToNat(n2n))
            case n2d: NatToData =>
              DepApply[NatToDataKind, T](
                apply(p, v).asInstanceOf[Phrase[NatToDataKind `()->:` T]],
                v.natToData(n2d))
            case ph: PhraseType => ???
          }

          case LetNat(binder, defn, body) =>
            LetNat(binder, apply(defn, v), apply(body, v))

          case PhrasePair(p, q) => PhrasePair(apply(p, v), apply(q, v))

          case Proj1(p) => Proj1(apply(p, v))

          case Proj2(p) => Proj2(apply(p, v))

          case IfThenElse(cond, thenP, elseP) =>
            IfThenElse(apply(cond, v), apply(thenP, v), apply(elseP, v))

          case Literal(d) => Literal(d)

          case Natural(n) => Natural(n)

          case UnaryOp(op, x) => UnaryOp(op, apply(x, v))

          case BinOp(op, lhs, rhs) => BinOp(op, apply(lhs, v), apply(rhs, v))

          case c: Primitive[T] => c.visitAndRebuild(v)
        }).asInstanceOf[Phrase[T]]
    }
  }

  def visitPhraseTypeAndRebuild(pt: PhraseType, v: Visitor): PhraseType =
    pt match {
      case ExpType(dt, w) => ExpType(
        visitDataTypeAndRebuild(dt, v),
        v.access(w))
      case AccType(dt) => AccType(visitDataTypeAndRebuild(dt, v))
      case CommType() => CommType()
      case PhrasePairType(t1, t2) => PhrasePairType(
        visitPhraseTypeAndRebuild(t1, v), visitPhraseTypeAndRebuild(t2, v))
      case FunType(inT, outT) => FunType(
        visitPhraseTypeAndRebuild(inT, v), visitPhraseTypeAndRebuild(outT, v))
      case PassiveFunType(inT, outT) => PassiveFunType(
        visitPhraseTypeAndRebuild(inT, v), visitPhraseTypeAndRebuild(outT, v))
      case DepFunType(x, t) => x match {
        case n: NatIdentifier =>
          DepFunType[NatKind, PhraseType](
            NatIdentifier(
              v.nat(n).asInstanceOf[arithexpr.arithmetic.NamedVar].name),
            visitPhraseTypeAndRebuild(t, v))
        case dt: DataTypeIdentifier =>
          DepFunType[DataKind, PhraseType](
            v.data(dt).asInstanceOf[DataTypeIdentifier],
            visitPhraseTypeAndRebuild(t, v))
        case ad: AddressSpaceIdentifier =>
          DepFunType[AddressSpaceKind, PhraseType](
            v.addressSpace(ad).asInstanceOf[AddressSpaceIdentifier],
            visitPhraseTypeAndRebuild(t, v))
        case ac: AccessTypeIdentifier =>
          DepFunType[AccessKind, PhraseType](
            v.access(ac).asInstanceOf[AccessTypeIdentifier],
            visitPhraseTypeAndRebuild(t, v))
        case n2n: NatToNatIdentifier =>
          DepFunType[NatToNatKind, PhraseType](
            v.natToNat(n2n).asInstanceOf[NatToNatIdentifier],
            visitPhraseTypeAndRebuild(t, v))
        case n2d: NatToDataIdentifier =>
          DepFunType[NatToDataKind, PhraseType](
            v.natToData(n2d).asInstanceOf[NatToDataIdentifier],
            visitPhraseTypeAndRebuild(t, v))
      }
    }

  private def visitDataTypeAndRebuild(dt: DataType, v: Visitor): DataType =
    v.data(dt) match {
      case i: IndexType => IndexType(v.nat(i.size))
      case a: ArrayType =>
        ArrayType(v.nat(a.size), visitDataTypeAndRebuild(a.elemType, v))
      case vec: VectorType =>
        VectorType(v.nat(vec.size), v.data(vec.elemType))
      case r: PairType =>
        PairType(visitDataTypeAndRebuild(r.fst, v),
          visitDataTypeAndRebuild(r.snd, v))
      case d => d
    }
}
