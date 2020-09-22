package rise.core

import arithexpr.arithmetic.{Cst, RangeAdd}
import rise.core.TypedDSL.TDSL
import rise.core.types._

// scalastyle:off multiple.string.literals
object TypeLevelDSL {
  implicit class TypeEqual(a: Type) {
    def =~=(b: Type): Boolean = (a, b) match {
      case (TypePlaceholder, _) => true
      case (_, TypePlaceholder) => true
      case _                    => a == b
    }
  }

  // type level lambdas
  object n2dtFun {
    def apply(f: NatIdentifier => DataType): NatToDataLambda = {
      val x = NatIdentifier(freshName("n"), isExplicit = true)
      NatToDataLambda(x, f(x))
    }

    def apply(
        r: arithexpr.arithmetic.Range
    )(f: NatIdentifier => DataType): NatToDataLambda = {
      val x = NatIdentifier(freshName("n"), r, isExplicit = true)
      NatToDataLambda(x, f(x))
    }

    def apply(
        upperBound: Nat
    )(f: NatIdentifier => DataType): NatToDataLambda = {
      apply(RangeAdd(0, upperBound, 1))(f)
    }
  }

  object n2nFun {
    def apply(f: NatIdentifier => Nat): NatToNatLambda = {
      val x = NatIdentifier(freshName("n2n"), isExplicit = true)
      NatToNatLambda(x, f(x))
    }

    def apply(
        r: arithexpr.arithmetic.Range
    )(f: NatIdentifier => Nat): NatToNatLambda = {
      val x = NatIdentifier(freshName("n2n"), r, isExplicit = true)
      NatToNatLambda(x, f(x))
    }

    def apply(upperBound: Nat)(f: NatIdentifier => Nat): NatToNatLambda = {
      apply(RangeAdd(0, upperBound, 1))(f)
    }
  }

  // dependent function types
  object forallNat {
    def apply(f: NatIdentifier => Type): Type = {
      val x = NatIdentifier(freshName("n"), isExplicit = true)
      DepFunType[NatKind, Type](x, f(x))
    }

    def unapply[K <: Kind, T <: Type](funType: DepFunType[K, T]): Option[(NatIdentifier, T)] = {
      funType.x match {
        case n: NatIdentifier => Some((n, funType.t))
        case _ => throw new Exception("Expected Nat DepFunType")
      }
    }
  }

  object forallDT {
    def apply(f: DataTypeIdentifier => Type): Type = {
      val x = DataTypeIdentifier(freshName("dt"), isExplicit = true)
      DepFunType[DataKind, Type](x, f(x))
    }

    def unapply[K <: Kind, T <: Type](funType: DepFunType[K, T]): Option[(DataTypeIdentifier, T)] = {
      funType.x match {
        case dt: DataTypeIdentifier => Some((dt, funType.t))
        case _ => throw new Exception("Expected DataType DepFunType")
      }
    }
  }

  object forallN2N {
    def apply(f: NatToNat => Type): Type = {
      val x = NatToNatIdentifier(freshName("n2n"), isExplicit = true)
      DepFunType[NatToNatKind, Type](x, f(x))
    }

    def unapply[K <: Kind, T <: Type](funType: DepFunType[K, T]): Option[(NatToNatIdentifier, T)] = {
      funType.x match {
        case n2n: NatToNatIdentifier => Some((n2n, funType.t))
        case _ => throw new Exception("Expected Nat to Nat DepFunType")
      }
    }
  }

  object forallN2DT {
    def apply(f: NatToData => Type): Type = {
      val x = NatToDataIdentifier(freshName("n2dt"), isExplicit = true)
      DepFunType[NatToDataKind, Type](x, f(x))
    }

    def unapply[K <: Kind, T <: Type](funType: DepFunType[K, T]): Option[(NatToDataIdentifier, T)] = {
      funType.x match {
        case n2dt: NatToDataIdentifier => Some((n2dt, funType.t))
        case _ => throw new Exception("Expected Nat to Data DepFunType")
      }
    }
  }

  object forallAddr {
    def apply(f: AddressSpaceIdentifier => Type): Type = {
      val x = AddressSpaceIdentifier(freshName("a"), isExplicit = true)
      DepFunType[AddressSpaceKind, Type](x, f(x))
    }

    def unapply[K <: Kind, T <: Type](funType: DepFunType[K, T]): Option[(AddressSpaceIdentifier, T)] = {
      funType.x match {
        case a: AddressSpaceIdentifier => Some((a, funType.t))
        case _ => throw new Exception("Expected AddressSpace DepFunType")
      }
    }
  }

  // dependent pairs
  object  n2dPairT {
    def apply(f: NatIdentifier => DataType): Type = {
      val x = NatIdentifier(freshName("n"), isExplicit = true)
      DepPairType[NatKind](x, f(x))
    }
  }

  object nats2dPairT {
    def apply(f: NatCollectionIdentifier => DataType): Type = {
      val x = NatCollectionIdentifier(freshName("ns"), isExplicit = true)
      DepPairType[NatCollectionKind](x, f(x))
    }
  }

  // types with implicit type parameters
  def implNat[A](f: NatIdentifier => A): A = {
    f(NatIdentifier(freshName("n")))
  }

  def implType[A](f: TypeIdentifier => A): A = {
    f(TypeIdentifier(freshName("t")))
  }
  def implDT[A](f: DataTypeIdentifier => A): A = {
    f(DataTypeIdentifier(freshName("dt")))
  }
  // TODO: BasicTypeIdentifier
  def implBT[A](f: DataTypeIdentifier => A): A = {
    f(DataTypeIdentifier(freshName("dt")))
  }
  // TODO: ScalarTypeIdentifier
  def implST[A](f: DataTypeIdentifier => A): A = {
    f(DataTypeIdentifier(freshName("dt")))
  }

  def implN2N[A](f: NatToNat => A): A = {
    f(NatToNatIdentifier(freshName("n2n")))
  }

  def implN2DT[A](f: NatToData => A): A = {
    f(NatToDataIdentifier(freshName("n2dt")))
  }

  def implAddr[A](f: AddressSpaceIdentifier => A): A = {
    f(AddressSpaceIdentifier(freshName("w")))
  }

  def implNatColl[A](f: NatCollectionIdentifier => A): A = {
    f(NatCollectionIdentifier(freshName("ns")))
  }

  def freshTypeIdentifier: Type = implType(identity)

  implicit final class TypeConstructors(private val r: Type) extends AnyVal {
    @inline def ->:(t: Type): FunType[Type, Type] = FunType(t, r)
  }

  object ->: {
    def unapply[T <: Type, U <: Type](funType: FunType[T, U]): Option[(T, U)] = {
      FunType.unapply(funType)
    }
  }

  implicit final class TupleTypeConstructors(private val a: DataType)
      extends AnyVal {
    @inline def x(b: DataType): PairType = PairType(a, b)
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
  }

  implicit final class ArrayTypeConstructorsFromInt(private val n: Int)
      extends AnyVal {
    @inline def `.`(m: Nat): ArrayTypeConstructorHelper =
      ArrayTypeConstructorHelper(Seq(Cst(n), m))
    @inline def `.`(dt: DataType): ArrayType = ArrayType(Cst(n), dt)
  }

  implicit final class DepArrayTypeConstructors(private val n: Nat)
    extends AnyVal {
    @inline def `..`(f: Nat => DataType): DepArrayType = DepArrayType(n, f)
    @inline def `..`(f: NatToData): DepArrayType = DepArrayType(n, f)
  }

  implicit final class NatCollectionConstructors(private val e: TDSL[Expr])
    extends AnyVal {
    @inline def `#`(nats: Nat*): Nat = {
      NatCollectionFromArray(e)(nats: _*)
    }
  }

}
// scalastyle:on multiple.string.literals
