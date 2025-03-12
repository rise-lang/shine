package rise.core

import scala.language.implicitConversions
import util.monads._
import rise.core.semantics._
import rise.core.types._
import rise.core.types.DataType._

object traverse {
  sealed trait VarType
  case object Binding extends VarType
  case object Reference extends VarType

  trait Traversal[M[_]] {
    protected[this] implicit def monad : Monad[M]
    def return_[T] : T => M[T] = monad.return_
    def bind[T,S] : M[T] => (T => M[S]) => M[S] = monad.bind

    // Identifiers are first routed through here before being sent
    // to nat/datatype/addressSpace/natToNat/natToData
    def typeIdentifier[I <: Kind.Identifier] : VarType => I => M[I] = _ => return_
    def identifier[I <: Identifier] : VarType => I => M[I] = _ => i =>
      for { t1 <- `type`(i.t)}
        yield i.setType(t1).asInstanceOf[I]
    def typeIdentifierDispatch[I <: Kind.Identifier] : VarType => I => M[I] = vt => i => (i match {
      case t: TypeKind.IDWrapper => typeIdentifier(vt)(t)
      case n: NatKind.IDWrapper => bind(nat(n.id))(i => typeIdentifier(vt)(NatKind.IDWrapper(i.asInstanceOf[NatIdentifier])))
      case dt: DataKind.IDWrapper => bind(datatype(dt.id))(i => typeIdentifier(vt)(DataKind.IDWrapper(i.asInstanceOf[DataTypeIdentifier])))
      case a: AddressSpaceKind.IDWrapper => bind(addressSpace(a.id))(i => typeIdentifier(vt)(AddressSpaceKind.IDWrapper(i.asInstanceOf[AddressSpaceIdentifier])))
      case m: MatrixLayoutKind.IDWrapper => bind(matrixLayout(m.id))(i => typeIdentifier(vt)(MatrixLayoutKind.IDWrapper(i.asInstanceOf[MatrixLayoutIdentifier])))
      case f: FragmentKind.IDWrapper => bind(fragmentKind(f.id))(i => typeIdentifier(vt)(FragmentKind.IDWrapper(i.asInstanceOf[FragmentIdentifier])))
      case n2n: NatToNatKind.IDWrapper => bind(natToNat(n2n.id))(i => typeIdentifier(vt)(NatToNatKind.IDWrapper(i.asInstanceOf[NatToNatIdentifier])))
      case n2d: NatToDataKind.IDWrapper => bind(natToData(n2d.id))(i => typeIdentifier(vt)(NatToDataKind.IDWrapper(i.asInstanceOf[NatToDataIdentifier])))
      case _ => ???
    }).asInstanceOf[M[I]]
    def natDispatch : VarType => Nat => M[Nat] = vt => {
      case i : NatIdentifier => bind(typeIdentifier(vt)(NatKind.IDWrapper(i)))(i => nat(i.id))
      case n => nat(n)
    }
    def matrixLayoutDispatch : VarType => MatrixLayout => M[MatrixLayout] = vt => {
      case i : MatrixLayoutIdentifier => bind(typeIdentifier(vt)(MatrixLayoutKind.IDWrapper(i)))(i => matrixLayout(i.id))
      case m => matrixLayout(m)
    }
    def fragmentKindDispatch : VarType => Fragment => M[Fragment] = vt => {
      case i : FragmentIdentifier => bind(typeIdentifier(vt)(FragmentKind.IDWrapper(i)))(i => fragmentKind(i.id))
      case m => fragmentKind(m)
    }
    def dataTypeDispatch : VarType => DataType => M[DataType] = vt => {
      case i : DataTypeIdentifier => bind(typeIdentifier(vt)(DataKind.IDWrapper(i)))(i => datatype(i.id))
      case d => datatype(d)
    }

    def nat : Nat => M[Nat] = return_
    def addressSpace : AddressSpace => M[AddressSpace] = return_
    def matrixLayout : MatrixLayout => M[MatrixLayout] = return_
    def fragmentKind : Fragment => M[Fragment] = return_

    def datatype : DataType => M[DataType] = {
      case i: DataTypeIdentifier => return_(i.asInstanceOf[DataType])
      case NatType               => return_(NatType : DataType)
      case s : ScalarType        => return_(s : DataType)
      case ArrayType(n, d) =>
        for {n1 <- natDispatch(Reference)(n); d1 <- dataTypeDispatch(Reference)(d)}
          yield ArrayType(n1, d1)
      case DepArrayType(n, n2d) =>
        for {n1 <- natDispatch(Reference)(n); n2d1 <- natToData(n2d)}
          yield DepArrayType(n1, n2d1)
      case PairType(p1, p2) =>
        for {p11 <- `type`(p1); p21 <- `type`(p2)}
          yield PairType(p11, p21)
      case DepPairType(kind, x, d) =>
        for {x1 <- typeIdentifierDispatch(Binding)(Kind.toIdentifier(kind, x)); d1 <- dataTypeDispatch(Reference)(d)}
          yield DepPairType(kind, Kind.fromIdentifier(kind, x1), d1)
      case IndexType(n) =>
        for {n1 <- natDispatch(Reference)(n)}
          yield IndexType(n1)
      case VectorType(n, d) =>
        for {n1 <- natDispatch(Reference)(n); d1 <- dataTypeDispatch(Reference)(d)}
          yield VectorType(n1, d1)
      case ManagedBufferType(dt) =>
        for {dt1 <- dataTypeDispatch(Reference)(dt)}
          yield ManagedBufferType(dt1)
      case o: OpaqueType => return_(o: DataType)
      case FragmentType(rows, columns, d3, dt, fragKind, layout) =>
        for {rows1 <- natDispatch(Reference)(rows);
             columns1 <- natDispatch(Reference)(columns);
             d31 <- natDispatch(Reference)(d3);
             dt1 <- dataTypeDispatch(Reference)(dt);
             fragKind1 <- fragmentKindDispatch(Reference)(fragKind);
             layout1 <- matrixLayoutDispatch(Reference)(layout)}
        yield FragmentType(rows1, columns1, d31, dt1, fragKind1, layout1)
      case NatToDataApply(ntdf, n) =>
        for {ntdf1 <- natToData(ntdf); n1 <- natDispatch(Reference)(n)}
          yield NatToDataApply(ntdf1, n1)
      case NatToDataApply(_, _) => ???
      case _ => ???
    }

    def natToNat : NatToNat => M[NatToNat] = {
      case i : NatToNatIdentifier => return_(i.asInstanceOf[NatToNat])
      case NatToNatLambda(x, e) =>
        for {x1 <- typeIdentifierDispatch(Binding)(NatKind.IDWrapper(x)); e1 <- natDispatch(Reference)(e)}
          yield NatToNatLambda(x1.id, e1)
    }

    def natToData : NatToData => M[NatToData] = {
      case i : NatToDataIdentifier => return_(i.asInstanceOf[NatToData])
      case NatToDataLambda(x, d) =>
        for {x1 <- typeIdentifierDispatch(Binding)(NatKind.IDWrapper(x)); d1 <- dataTypeDispatch(Reference)(d)}
          yield NatToDataLambda(x1.id, d1)
    }

    def data : Data => M[Data] = {
      case (sd : ScalarData) => return_(sd : Data)
      case VectorData(vd) => return_(VectorData(vd) : Data)
      case NatData(n) =>
        for { n1 <- natDispatch(Reference)(n) }
          yield NatData(n1)
      case IndexData(i, n) =>
        for { i1 <- natDispatch(Reference)(i); n1 <- natDispatch(Reference)(n) }
          yield IndexData(i1, n1)
      case ArrayData(ad) =>
        for { ad1 <- monad.traverse(ad.map(data)) }
          yield ArrayData(ad1)
      case PairData(l, r) =>
        for { l1 <- data(l); r1 <- data(r) }
          yield PairData(l1, r1)
    }

    def `type`[T <: ExprType ] : T => M[T] = t => (t match {
      case TypePlaceholder => return_(TypePlaceholder)
      case i: TypeIdentifier =>
        for { i1 <- typeIdentifierDispatch(Reference)(TypeKind.IDWrapper(i))}
          yield i1.id
      case dt: DataType => dataTypeDispatch(Reference)(dt)
      case FunType(a, b) =>
        for {a1 <- `type`(a); b1 <- `type`(b)}
          yield FunType(a1, b1)
      case DepFunType(kind, x, t) =>
        for { n1 <- typeIdentifierDispatch(Binding)(Kind.toIdentifier(kind, x)); t1 <- `type`(t)}
          yield DepFunType(kind, Kind.fromIdentifier(kind, n1), t1)
    }).asInstanceOf[M[T]]

    def expr : Expr => M[Expr] = {
      case i : Identifier => for { r <- identifier(Reference)(i) } yield r
      case l@Lambda(x, e) =>
        for {
          x1 <- identifier(Binding)(x)
          e1 <- expr(e)
          t1 <- `type`(l.t)
        } yield Lambda(x1, e1)(t1)
      case a@App(f, e) =>
        for {
          f1 <- expr(f)
          e1 <- expr(e)
          t1 <- `type`(a.t)
        } yield App(f1, e1)(t1)
      case dl@DepLambda(kind, x,e) =>
        for {n1 <- typeIdentifierDispatch(Binding)(Kind.toIdentifier(kind, x)); e1 <- expr(e); t1 <- `type`(dl.t)}
          yield DepLambda(kind, Kind.fromIdentifier(kind, n1), e1)(t1)
      case da@DepApp(NatKind, f, x: Nat) =>
        for {f1 <- expr(f); n1 <- natDispatch(Reference)(x); t1 <- `type`(da.t)}
          yield DepApp(NatKind, f1, n1)(t1)
      case da@DepApp(DataKind, f, x: DataType) =>
        for {f1 <- expr(f); dt1 <- `type`(x); t1 <- `type`(da.t)}
          yield DepApp(DataKind, f1, dt1)(t1)
      case da@DepApp(AddressSpaceKind, f, x: AddressSpace) =>
        for {f1 <- expr(f); a1 <- addressSpace(x); t1 <- `type`(da.t)}
          yield DepApp(AddressSpaceKind, f1, a1)(t1)
      case da@DepApp(NatToNatKind, f, x: NatToNat) =>
        for {f1 <- expr(f); n2n1 <- natToNat(x); t1 <- `type`(da.t)}
          yield DepApp(NatToNatKind, f1, n2n1)(t1)
      case da@DepApp(NatToDataKind, f, x: NatToData) =>
        for {f1 <- expr(f); n2d1 <- natToData(x); t1 <- `type`(da.t)}
          yield DepApp(NatToDataKind, f1, n2d1)(t1)
      case DepApp(_, _, _) =>
        ???
      case Literal(d) =>
        for { d1 <- data(d) }
          yield Literal(d1)
      case TypeAnnotation(e, t) =>
        for { e1 <- expr(e); t1 <- `type`(t)}
          yield TypeAnnotation(e1, t1)
      case TypeAssertion(e, t) =>
        for { e1 <- expr(e); t1 <- `type`(t)}
          yield TypeAssertion(e1, t1)
      case Opaque(e, t) =>
        for { e1 <- expr(e); t1 <- `type`(t)}
          yield Opaque(e1, t1)
      case p : Primitive =>
        for { t1 <- `type`(p.t) }
          yield p.setType(t1)
    }
  }

  trait ExprTraversal[M[_]] extends Traversal[M] {
    override def `type`[T <: ExprType] : T => M[T] = return_
  }

  trait PureTraversal extends Traversal[Pure] {override def monad : PureMonad.type = PureMonad }
  trait PureExprTraversal extends PureTraversal with ExprTraversal[Pure]
  trait AccumulatorTraversal[F,M[_]] extends Traversal[InMonad[M]#SetFst[F]#Type] {
    type Pair[T] = InMonad[M]#SetFst[F]#Type[T]
    implicit val accumulator : Monoid[F]
    implicit val wrapperMonad : Monad[M]
    def accumulate[T] : F => T => Pair[T] = f => t => wrapperMonad.return_((f, t))
    override protected[this] implicit def monad : PairMonoidMonad[F,M] = new PairMonoidMonad[F,M] {
      override val monoid = implicitly(accumulator)
      override val monad = implicitly(wrapperMonad)
    }
  }
  trait PureAccumulatorTraversal[F] extends AccumulatorTraversal[F, Pure] {
    override val wrapperMonad : PureMonad.type = PureMonad
  }

  def traverse(e: Expr, f: PureTraversal): Expr = f.expr(e).unwrap
  def traverse[T <: ExprType](t: T, f: PureTraversal): T = f.`type`(t).unwrap
  def traverse[F](e: Expr, f: PureAccumulatorTraversal[F]): (F, Expr) = f.expr(e).unwrap
  def traverse[F,T <: ExprType](t: T, f: PureAccumulatorTraversal[F]): (F, T) = f.`type`(t).unwrap
  def traverse[M[_]](e: Expr, f: Traversal[M]): M[Expr] = f.expr(e)
  def traverse[T <: ExprType, M[_]](e: T, f: Traversal[M]): M[T] = f.`type`(e)
}
