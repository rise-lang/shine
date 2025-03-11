package rise.core.DSL

import arithexpr.arithmetic.{Cst, RangeAdd}
import rise.core.types._
import rise.core.types.DataType._
import rise.core.{Expr, freshName}

import scala.language.implicitConversions

// scalastyle:off multiple.string.literals
object Type {

  // type level lambdas
  object n2dtFun {
    def apply(f: NatIdentifier => DataType): NatToDataLambda = {
      val x = NatIdentifier(freshName("n"))
      NatToDataLambda(x, f(x))
    }

    def apply(r: arithexpr.arithmetic.Range )(f: NatIdentifier => DataType): NatToDataLambda = {
      val x = NatIdentifier(freshName("n"), r)
      NatToDataLambda(x, f(x))
    }

    def apply(upperBound: Nat)(f: NatIdentifier => DataType): NatToDataLambda = {
      apply(RangeAdd(0, upperBound, 1))(f)
    }
  }

  object n2nFun {
    def apply(f: NatIdentifier => Nat): NatToNatLambda = {
      val x = NatIdentifier(freshName("n2n"))
      NatToNatLambda(x, f(x))
    }

    def apply(
               r: arithexpr.arithmetic.Range
             )(f: NatIdentifier => Nat): NatToNatLambda = {
      val x = NatIdentifier(freshName("n2n"), r)
      NatToNatLambda(x, f(x))
    }

    def apply(upperBound: Nat)(f: NatIdentifier => Nat): NatToNatLambda = {
      apply(RangeAdd(0, upperBound, 1))(f)
    }
  }

  case class NatFunctionWrapper[A](f: Nat => A)

  implicit def toNatFunctionWrapper[A](f: Nat => A): NatFunctionWrapper[A] =
    NatFunctionWrapper(f)

  case class DataTypeFunctionWrapper[A](f: DataType => A)

  implicit def toDataTypeFunctionWrapper[A](f: DataType => A): DataTypeFunctionWrapper[A] =
    DataTypeFunctionWrapper(f)

  case class NatToDataFunctionWrapper[A](f: NatToData => A)

  implicit def toNatToDataFunctionWrapper[A](f: NatToData => A): NatToDataFunctionWrapper[A] =
    NatToDataFunctionWrapper(f)

  case class NatToNatFunctionWrapper[A](f: NatToNat => A)

  implicit def toNatToNatFunctionWrapper[A](f: NatToNat => A): NatToNatFunctionWrapper[A] =
    NatToNatFunctionWrapper(f)

  case class AddressSpaceFunctionWrapper[A](f: AddressSpace => A)

  implicit def toAddressSpaceFunctionWrapper[A](f: AddressSpace => A): AddressSpaceFunctionWrapper[A] =
    AddressSpaceFunctionWrapper(f)

  case class MatrixLayoutWrapper[A](f: MatrixLayout => A)
  implicit def toMatrixLayoutWrapper[A](f: MatrixLayout => A): MatrixLayoutWrapper[A] =
    MatrixLayoutWrapper(f)

  case class FragementTypeWrapper[A](f: Fragment => A)
  implicit def toFragmentTypeWrapper[A](f: Fragment => A): FragementTypeWrapper[A] =
    FragementTypeWrapper(f)

  case class TypeFunctionWrapper[A](f: TypeIdentifier => A)

  implicit def toTypeFunctionWrapper[A](f: TypeIdentifier => A): TypeFunctionWrapper[A] =
    TypeFunctionWrapper(f)

  case class NatCollectionFunctionWrapper[A](f: NatCollectionIdentifier => A)

  implicit def toNatCollectionFunctionWrapper[A](f: NatCollectionIdentifier => A): NatCollectionFunctionWrapper[A] =
    NatCollectionFunctionWrapper(f)

  object expl {
    def apply(w: NatFunctionWrapper[ExprType]): ExprType = {
      val x = NatIdentifier(freshName("n"))
      DepFunType(NatKind, x, w.f(x))
    }

    def apply(w: DataTypeFunctionWrapper[ExprType]): ExprType = {
      val x = DataTypeIdentifier(freshName("dt"))
      DepFunType(DataKind, x, w.f(x))
    }

    def apply(w: NatToDataFunctionWrapper[ExprType]): ExprType = {
      val x = NatToDataIdentifier(freshName("n2d"))
      DepFunType(NatToDataKind, x, w.f(x))
    }

    def apply(w: NatToNatFunctionWrapper[ExprType]): ExprType = {
      val x = NatToNatIdentifier(freshName("n2n"))
      DepFunType(NatToNatKind, x, w.f(x))
    }

    def apply(w: AddressSpaceFunctionWrapper[ExprType]): ExprType = {
      val x = AddressSpaceIdentifier(freshName("a"))
      DepFunType(AddressSpaceKind, x, w.f(x))
    }
  }

  object impl {
    def apply[A](w: NatFunctionWrapper[A]): A = {
      w.f(NatIdentifier(freshName("n")))
    }

    def apply[A](w: DataTypeFunctionWrapper[A]): A = {
      w.f(DataTypeIdentifier(freshName("dt")))
    }

    def apply[A](w: NatToDataFunctionWrapper[A]): A = {
      w.f(NatToDataIdentifier(freshName("n2d")))
    }

    def apply[A](w: NatToNatFunctionWrapper[A]): A = {
      w.f(NatToNatIdentifier(freshName("n2n")))
    }

    def apply[A](w: AddressSpaceFunctionWrapper[A]): A = {
      w.f(AddressSpaceIdentifier(freshName("n2n")))
    }

    def apply[A](w: MatrixLayoutWrapper[A]): A = {
      w.f(MatrixLayoutIdentifier(freshName("ml")))
    }

    def apply[A](w: FragementTypeWrapper[A]): A = {
      w.f(FragmentIdentifier(freshName("ft")))
    }

    def apply[A](w: TypeFunctionWrapper[A]): A = {
      w.f(TypeIdentifier(freshName("t")))
    }

    def apply[A](w: NatCollectionFunctionWrapper[A]): A = {
      w.f(NatCollectionIdentifier(freshName("ns")))
    }
  }

  // dependent pairs
  object Nat {
    def `**`(f: Nat => DataType): ExprType = {
      val x = NatIdentifier(freshName("n"))
      DepPairType(NatKind, x, f(x))
    }
  }

  object NatCollection {
    def `**`(f: NatCollection => DataType): ExprType = {
      val x = NatCollectionIdentifier(freshName("ns"))
      DepPairType(NatCollectionKind, x, f(x))
    }
  }

  object `:Nat **` {
    def unapply(arg: DepPairType[Nat, NatIdentifier]): Option[(NatIdentifier, DataType)] =
      Some(arg.x, arg.t)
  }

  object `:NatCollection **` {
    def unapply(arg: DepPairType[NatCollection, NatCollectionIdentifier]): Option[(NatCollectionIdentifier, DataType)] =
      Some(arg.x, arg.t)
  }


  def freshTypeIdentifier: ExprType = impl { x: TypeIdentifier => x }

  implicit final class TypeConstructors(private val r: ExprType) extends AnyVal {
    @inline def ->:(t: ExprType): FunType[ExprType, ExprType] = FunType(t, r)
  }

  object ->: {
    def unapply[T <: ExprType, U <: ExprType](funType: FunType[T, U]): Option[(T, U)] = {
      Some((funType.inT, funType.outT))
    }
  }

  object `(Addr)->:` {
    def unapply[T, I, U <: ExprType](funType: DepFunType[T, I, U]): Option[(AddressSpaceIdentifier, U)] = {
      funType.x match {
        case a : AddressSpaceIdentifier => Some((a, funType.t))
        case _ => throw new Exception("Expected AddressSpace DepFunType")
      }
    }
  }

  object `(Nat)->:` {
    def unapply[T, I, U <: ExprType](funType: DepFunType[T, I, U]): Option[(NatIdentifier, U)] = {
      funType.x match {
        case n : NatIdentifier => Some((n, funType.t))
        case _ => throw new Exception("Expected Nat DepFunType")
      }
    }
  }

  object `(NatToNat)->:` {
    def unapply[T, I, U <: ExprType](funType: DepFunType[T, I, U]): Option[(NatToNatIdentifier, U)] = {
      funType.x match {
        case n : NatToNatIdentifier => Some((n, funType.t))
        case _ => throw new Exception("Expected NatToNat DepFunType")
      }
    }
  }

  implicit final class TupleTypeConstructors(private val a: DataType)
    extends AnyVal {
    @inline def x(b: DataType): PairType = PairType(a, b)
  }

  object x {
    def unapply(t: PairType): Option[(DataType, DataType)] = Some(t.dt1, t.dt2)
  }

  final case class ArrayTypeConstructorHelper(ns: Seq[Nat]) {
    @inline def `.`(n: Nat): ArrayTypeConstructorHelper =
      ArrayTypeConstructorHelper(ns :+ n)

    @inline def `.`(dt: DataType): ArrayType = {
      val nsr = ns.reverse
      nsr.tail.foldLeft(ArrayType(nsr.head, dt))((t, n) => ArrayType(n, t))
    }
  }

  implicit final class ArrayTypeConstructors(private val n: Nat)
    extends AnyVal {
    @inline def `.`(m: Nat): ArrayTypeConstructorHelper =
      ArrayTypeConstructorHelper(Seq(n, m))

    @inline def `.`(dt: DataType): ArrayType = ArrayType(n, dt)

    @inline def `.d`(ft: NatToData): DepArrayType =
      DepArrayType(n, ft)
    @inline def `.d`(f: NatIdentifier => DataType): DepArrayType =
      DepArrayType(n, NatToDataLambda(n, f))
  }

  implicit final class ArrayTypeConstructorsFromInt(private val n: Int)
    extends AnyVal {
    @inline def `.`(m: Nat): ArrayTypeConstructorHelper =
      ArrayTypeConstructorHelper(Seq(Cst(n), m))

    @inline def `.`(dt: DataType): ArrayType = ArrayType(Cst(n), dt)
  }

  object `.` {
    def unapply(arg: ArrayType): Option[(Nat, DataType)] =
      Some(arg.size, arg.elemType)
  }

  implicit final class DepArrayTypeConstructors(private val n: Nat)
    extends AnyVal {
    @inline def `*.`(f: Nat => DataType): DepArrayType = DepArrayType(n, f)

    @inline def `*.`(f: NatToData): DepArrayType = DepArrayType(n, f)
  }

  implicit final class NatCollectionConstructors(private val e: ToBeTyped[Expr])
    extends AnyVal {
    @inline def `#`(nats: Nat*): Nat = {
      NatCollectionFromArray(e)(nats: _*)
    }
  }

  object `*.` {
    def unapply(arg: DepArrayType): Option[(Nat, NatToData)] =
      Some(arg.size, arg.fdt)
  }

  @inline
  def idx(n: Nat): IndexType = IndexType(n)
}
