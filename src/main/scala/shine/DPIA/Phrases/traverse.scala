package shine.DPIA.Phrases

import scala.language.implicitConversions
import util.monads._
import shine.DPIA.Types._
import shine.DPIA._

object traverse {
  trait ExprTraversal[M[+_]] extends Traversal[M] {
    override def `type`[T <: PhraseType] : T => M[T] = return_
  }
  trait PureTraversal extends Traversal[Pure] { override def monad = PureMonad }
  trait PureExprTraversal extends PureTraversal with ExprTraversal[Pure]

  def traverse[T <: PhraseType](e : Phrase[T], f : PureTraversal) : Phrase[T] = f.phrase(e).unwrap
  def traverse[T <: PhraseType, M[+_]](e : Phrase[T], f : Traversal[M]) : M[Phrase[T]] = f.phrase(e)
  def traverse[T <: PhraseType] (t : T, f : PureTraversal) : T = f.`type`(t).unwrap
  def traverse[T <: PhraseType, M[+_]](e : T, f : Traversal[M]) : M[T] = f.`type`(e)

  sealed trait VarType
  case object Binding extends VarType
  case object Reference extends VarType

  trait Traversal[M[+_]] {
    implicit def monad: Monad[M]
    def return_[T]: T => M[T] = monad.return_
    def bind[T, S]: M[T] => (T => M[S]) => M[S] = monad.bind

    def nat[N <: Nat] : N => M[N] = return_
    def identifier[T <: PhraseType]: VarType => Identifier[T] => M[Identifier[T]] = _ => i =>
      for {t1 <- `type`(i.t)}
        yield Identifier(i.name, t1)
    def typeIdentifier[I <: Kind.Identifier]: VarType => I => M[I] = _ => return_
    def typeIdentifierDispatch[I <: Kind.Identifier]: VarType => I => M[I] = vt => i => (i match {
      case n: NatIdentifier => bind(typeIdentifier(vt)(n))(nat)
      case dt: DataTypeIdentifier => bind(typeIdentifier(vt)(dt))(datatype)
      case a: AddressSpaceIdentifier => bind(typeIdentifier(vt)(a))(addressSpace)
      case ac: AccessTypeIdentifier => bind(typeIdentifier(vt)(ac))(accessType)
      case n2n: NatToNatIdentifier => bind(typeIdentifier(vt)(n2n))(natToNat)
      case n2d: NatToDataIdentifier => bind(typeIdentifier(vt)(n2d))(natToData)
    }).asInstanceOf[M[I]]
    def natDispatch : Nat => M[Nat] = {
      case i : NatIdentifier => bind(typeIdentifier(Reference)(i))(nat)
      case n => nat(n)
    }

    def addressSpace: AddressSpace => M[AddressSpace] = return_
    def accessType: AccessType => M[AccessType] = return_
    def data: Data => M[Data] = {
      case VectorData(vd) => return_(VectorData(vd) : Data)
      case NatData(n) =>
        for { n1 <- natDispatch(n) }
          yield NatData(n1)
      case IndexData(i, n) =>
        for { i1 <- natDispatch(i); n1 <- natDispatch(n) }
          yield IndexData(i1, n1)
      case ArrayData(ad) =>
        for { ad1 <- monad.traverseV(ad.map(data)) }
          yield ArrayData(ad1)
      case PairData(l, r) =>
        for { l1 <- data(l); r1 <- data(r) }
          yield PairData(l1, r1)
      case d => return_(d)
    }

    def datatype[ D <:DataType] : D => M[D] = d => (d match {
      case NatType => return_(NatType)
      case s : ScalarType => return_(s)
      case IndexType(size) =>
        for {n1 <- natDispatch(size)}
          yield IndexType(n1)
      case ArrayType(size, dt) =>
        for {n1 <- natDispatch(size); dt1 <- datatype(dt)}
          yield ArrayType(n1, dt1)
      case DepArrayType(n, n2d) =>
        for {n1 <- natDispatch(n); n2d1 <- natToData(n2d)}
          yield DepArrayType(n1, n2d1)
      case VectorType(size, dt) =>
        for {n1 <- natDispatch(size); dt1 <- datatype(dt)}
          yield VectorType(n1, dt1)
      case PairType(l, r) =>
        for {l1 <- datatype(l); r1 <- datatype(r)}
          yield PairType(l1, r1)
      case pair@DepPairType(x, e) =>
        for {x1 <- typeIdentifierDispatch(Binding)(x); e1 <- datatype(e)}
          yield DepPairType(x1, e1)
      case NatToDataApply(ntdf, n) =>
        for {ntdf1 <- natToData(ntdf); n1 <- natDispatch(n)}
          yield NatToDataApply(ntdf1, n1)
      case FragmentType(rs, cs, d3, dt, fk, l) =>
        for {rs1 <- natDispatch(rs); cs1 <- natDispatch(cs); d31 <- natDispatch(d3); dt1 <- datatype(dt)}
          yield FragmentType(rs1, cs1, d31, dt1, fk, l)

    }).asInstanceOf[M[D]]

    def natToNat: NatToNat => M[NatToNat] = {
      case i: NatToNatIdentifier => return_(i : NatToNat)
      case NatToNatLambda(n, body) =>
        for {n1 <- typeIdentifierDispatch(Binding)(n); body1 <- natDispatch(body)}
          yield NatToNatLambda(n1, body1)
    }

    def natToData: NatToData => M[NatToData] = {
      case i: NatToDataIdentifier => return_(i)
      case NatToDataLambda(n, body) =>
        for {n1 <- typeIdentifierDispatch(Binding)(n); body1 <- datatype(body)}
          yield NatToDataLambda(n1, body1)
    }

    def phrase[T <: PhraseType]: Phrase[T] => M[Phrase[T]] = {
      case i: Identifier[T] => for {i1 <- identifier(Reference)(i)} yield i1
      case Lambda(x, p) =>
        for {x1 <- identifier(Binding)(x); p1 <- phrase(p)}
          yield Lambda(x1, p1)
      case Apply(p, q) =>
        for {p1 <- phrase(p); q1 <- phrase(q)}
          yield Apply(p1, q1)
      case dl@DepLambda(i, p) =>
        for {i1 <- typeIdentifierDispatch(Binding)(i); p1 <- phrase(p)}
          yield DepLambda(i1, p1)(dl.kindName)
      case da@DepApply(f, x) => x match {
        case n: Nat =>
          for {f1 <- phrase(f); n1 <- natDispatch(n)}
            yield DepApply[NatKind, T](f1.asInstanceOf[Phrase[NatKind `()->:` T]], n1)
        case dt: DataType =>
          for {f1 <- phrase(f); dt1 <- datatype(dt)}
            yield DepApply[DataKind, T](f1.asInstanceOf[Phrase[DataKind `()->:` T]], dt1)
        case a: AddressSpace =>
          for {f1 <- phrase(f); a1 <- addressSpace(a)}
            yield DepApply[AddressSpaceKind, T](f1.asInstanceOf[Phrase[AddressSpaceKind `()->:` T]], a1)
        case n2n: NatToNat =>
          for {f1 <- phrase(f); n2n1 <- natToNat(n2n)}
            yield DepApply[NatToNatKind, T](f1.asInstanceOf[Phrase[NatToNatKind `()->:` T]], n2n1)
        case n2d: NatToData =>
          for {f1 <- phrase(f); n2d1 <- natToData(n2d)}
            yield DepApply[NatToDataKind, T](f1.asInstanceOf[Phrase[NatToDataKind `()->:` T]], n2d1)
      }
      case LetNat(binder, defn, body) =>
        for {defn1 <- phrase(defn); body1 <- phrase(body)}
          yield LetNat(binder, defn1, body1)
      case PhrasePair(p, q) =>
        for {p1 <- phrase(p); q1 <- phrase(q)}
          yield PhrasePair(p1, q1)
      case Proj1(p) =>
        for {p1 <- phrase(p)}
          yield Proj1(p1)
      case Proj2(p) =>
        for {p1 <- phrase(p)}
          yield Proj2(p1)
      case IfThenElse(cond, thenP, elseP) =>
        for {cond1 <- phrase(cond); thenP1 <- phrase(thenP); elseP1 <- phrase(elseP)}
          yield IfThenElse(cond1, thenP1, elseP1)
      case Literal(d) =>
        for {d1 <- data(d)}
          yield Literal(d1)
      case Natural(n) =>
        for {n1 <- natDispatch(n)}
          yield Natural(n1)
      case UnaryOp(op, x) =>
        for {x1 <- phrase(x)}
          yield UnaryOp(op, x1)
      case BinOp(op, lhs, rhs) =>
        for {lhs1 <- phrase(lhs); rhs1 <- phrase(rhs)}
          yield BinOp(op, lhs1, rhs1)
      case c: Primitive[T] => c.traverse(this)
    }

    def `type`[T <: PhraseType] : T => M[T] = t => (t match {
      case CommType() => return_(CommType())
      case ExpType(dt, w) =>
        for {dt1 <- datatype(dt); w1 <- accessType(w)}
          yield ExpType(dt1, w1)
      case AccType(dt) =>
        for {dt1 <- datatype(dt)}
          yield AccType(dt1)
      case PhrasePairType(l, r) =>
        for {l1 <- `type`(l); r1 <- `type`(r)}
          yield PhrasePairType(l1, r1)
      case FunType(inT, outT) =>
        for {inT1 <- `type`(inT); outT1 <- `type`(outT)}
          yield FunType(inT1, outT1)
      case PassiveFunType(inT, outT) =>
        for {inT1 <- `type`(inT); outT1 <- `type`(outT)}
          yield PassiveFunType(inT1, outT1)
      case df@DepFunType(x, t) =>
        for {x1 <- typeIdentifierDispatch(Binding)(x); t1 <- `type`(t)}
          yield DepFunType(x1, t1)(df.kindName)
    }).asInstanceOf[M[T]]
  }
}