package rise.core

import rise.arithmetic.{Cst, RangeAdd}
import rise.core.types._

object TypeLevelDSL {
  implicit class TypeEqual(a: Type) {
    def =~=(b: Type): Boolean = (a, b) match {
      case (TypePlaceholder, _) => true
      case (_, TypePlaceholder) => true
      case _ => a == b
    }
  }

  // type level lambdas
  object n2dtFun {
    def apply(f: NatIdentifier => DataType): NatToDataLambda = {
      val x = NatIdentifier(freshName("n2dt"), isExplicit = true)
      NatToDataLambda(x, f(x))
    }

    def apply(r: rise.arithmetic.Range)(f: NatIdentifier => DataType): NatToDataLambda = {
      val x = NatIdentifier(freshName("n2dt"), r, isExplicit = true)
      NatToDataLambda(x, f(x))
    }

    def apply(upperBound: Nat)(f: NatIdentifier => DataType): NatToDataLambda = {
      apply(RangeAdd(0, upperBound, 1))(f)
    }
  }

  object n2nFun {
    def apply(f: NatIdentifier => Nat): NatToNatLambda = {
      val x = NatIdentifier(freshName("n2n"), isExplicit = true)
      NatToNatLambda(x, f(x))
    }

    def apply(r: rise.arithmetic.Range)(f: NatIdentifier => Nat): NatToNatLambda = {
      val x = NatIdentifier(freshName("n2n"), r, isExplicit = true)
      NatToNatLambda(x, f(x))
    }

    def apply(upperBound: Nat)(f: NatIdentifier => Nat): NatToNatLambda = {
      apply(RangeAdd(0, upperBound, 1))(f)
    }
  }

  // dependent function types
  object nFunT {
    def apply(f: NatIdentifier => Type): Type = {
      val x = NatIdentifier(freshName("n"), isExplicit = true)
      DepFunType[NatKind, Type](x, f(x))
    }
  }

  object dtFunT {
    def apply(f: DataTypeIdentifier => Type): Type = {
      val x = DataTypeIdentifier(freshName("dt"), isExplicit = true)
      DepFunType[DataKind, Type](x, f(x))
    }
  }

  object n2nFunT {
    def apply(f: NatToNat => Type): Type = {
      val x = NatToNatIdentifier(freshName("n2n"), isExplicit = true)
      DepFunType[NatToNatKind, Type](x, f(x))
    }
  }

  object n2dtFunT {
    def apply(f: NatToData => Type): Type = {
      val x = NatToDataIdentifier(freshName("n2dt"), isExplicit = true)
      DepFunType[NatToDataKind, Type](x, f(x))
    }
  }

  object aFunT {
    def apply(f: AddressSpaceIdentifier => Type): Type = {
      val x = AddressSpaceIdentifier(freshName("a"), isExplicit = true)
      DepFunType[AddressSpaceKind, Type](x, f(x))
    }
  }

  // types with implicit type parameters
  def implN[A](f: NatIdentifier => A): A = {
    f(NatIdentifier(freshName("n")))
  }


  def implT[A](f: TypeIdentifier => A): A = {
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

  def implA[A](f: AddressSpaceIdentifier => A): A = {
    f(AddressSpaceIdentifier(freshName("w")))
  }

  def freshTypeIdentifier: Type = implT(identity)

  implicit final class TypeConstructors(private val r: Type) extends AnyVal {
    @inline def ->:(t: Type): FunType[Type, Type] = FunType(t, r)
  }

  implicit final class TupleTypeConstructors(private val a: DataType) extends AnyVal {
    @inline def x(b: DataType): PairType = PairType(a, b)
  }

  final case class ArrayTypeConstructorHelper(ns: Seq[Nat]) {
    @inline def `.`(n: Nat): ArrayTypeConstructorHelper = ArrayTypeConstructorHelper(ns :+ n)
    @inline def `.`(dt: DataType): ArrayType = {
      val nsr = ns.reverse
      nsr.tail.foldLeft(ArrayType(nsr.head, dt))( (t, n) => ArrayType(n, t) )
    }
  }

  implicit final class ArrayTypeConstructors(private val n: Nat) extends AnyVal {
    @inline def `.`(m: Nat): ArrayTypeConstructorHelper = ArrayTypeConstructorHelper(Seq(n, m))
    @inline def `.`(dt: DataType): ArrayType = ArrayType(n, dt)
  }

  implicit final class ArrayTypeConstructorsFromInt(private val n: Int) extends AnyVal {
    @inline def `.`(m: Nat): ArrayTypeConstructorHelper = ArrayTypeConstructorHelper(Seq(Cst(n), m))
    @inline def `.`(dt: DataType): ArrayType = ArrayType(Cst(n), dt)
  }
}
