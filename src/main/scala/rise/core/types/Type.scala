package rise.core.types

import arithexpr.arithmetic.RangeAdd
import rise.core._
import util.PatternMatching

object alphaEquiv {
  type Env = List[(Kind.Identifier , Kind.Identifier)]

  def equivNat(env : Env) : Nat => Nat => Boolean = a => b => {
    val natEnv = env.collect {case (i : NatIdentifier, n : Nat) => (i , n)}
    // substitutes elements on the left with elements on the right
    substitute.natsInNat(natEnv.toMap, a) == b
  }

  def makeExplicit[K <: Kind.Identifier] : K => K = {
    case t : DataTypeIdentifier => t.asExplicit.asInstanceOf[K]
    case t => t
  }

  def equiv : Type => Type => Boolean = equiv(Nil)

  /** Alpha equivalence on types
    * Datatype identifier explicitness is ignored.
    * Short circuits in the event of syntactic equality.
    * @param env Pairs of identifiers to be considered equal.
    */
  def equiv(env : Env) : Type => Type => Boolean = a => b => {
    val and = PatternMatching.matchWithDefault(b, false)
    a == b || (a match {
      // Base cases
      case TypePlaceholder => and { case TypePlaceholder => true }
      case NatType => and { case NatType => true }
      case sa: ScalarType => and { case sb: ScalarType => sa == sb }

      // Base cases -> identifier lookup
      case na : TypeIdentifier => and { case nb : TypeIdentifier => na == nb || env.contains((na, nb)) }
      case na : DataTypeIdentifier => and { case nb : DataTypeIdentifier => na.asExplicit == nb.asExplicit || env.contains((na.asExplicit, nb.asExplicit)) }

      // Base cases -> identifier lookup in nat expressions
      case IndexType(sa) => and { case IndexType(sb) => equivNat(env)(sa)(sb) }
      case DepArrayType(sa, da) => and { case DepArrayType(sb, db) => equivNat(env)(sa)(sb) && da == db }

      case NatToDataApply(fa, na) => and { case NatToDataApply(fb, nb) =>
        val and = PatternMatching.matchWithDefault(fb, false)
        equivNat(env)(na)(nb) && (fa match {
          case na : NatToDataIdentifier => and { case nb : NatToDataIdentifier => na == nb || env.contains((na, nb)) }
          case NatToDataLambda(xa, ba) => and { case NatToDataLambda(xb, bb) => equiv((xa , xb) :: env)(ba)(bb) }
        })
       }

      // Recursive cases
      case FunType(sa, ta) => and { case FunType(sb, tb) => equiv(env)(sa)(sb) && equiv(env)(ta)(tb) }
      case PairType(la, ra) => and { case PairType(lb, rb) => equiv(env)(la)(lb) && equiv(env)(ra)(rb) }
      case VectorType(sa, da) => and { case VectorType(sb, db) => equivNat(env)(sa)(sb) && equiv(env)(da)(db) }
      case ArrayType(sa, da) => and { case ArrayType(sb, db) => equivNat(env)(sa)(sb) && equiv(env)(da)(db) }

      // Recursive cases -> binding tracking
      case DepFunType(xa, ta) => and { case DepFunType(xb, tb) =>
        xa.getClass == xb.getClass && equiv((makeExplicit(xa), makeExplicit(xb)) :: env)(ta)(tb) }
      case DepPairType(xa, ta) => and { case DepPairType(xb, tb) =>
        xa.getClass == xb.getClass && equiv((makeExplicit(xa), makeExplicit(xb)) :: env)(ta)(tb) }
    })
  }

  val hash : Type => Int = {
    case TypePlaceholder => 5
    case TypeIdentifier(n) => 7 * n.hashCode()
    case FunType(inT, outT) => 11 * inT.hashCode() + 13 * outT.hashCode() + 1
    case DepFunType(_, t) => 17 * t.hashCode()
    case dataType: DataType => 19 * dataType.hashCode()
  }
}


sealed trait Type {
  def =~~=(b: Type): Boolean = alphaEquiv.equiv(this)(b)
  def =~=(b: Type): Boolean = (this, b) match {
    case (TypePlaceholder, _) => true
    case (_, TypePlaceholder) => true
    case _ => alphaEquiv.equiv(this)(b)
  }
}

object TypePlaceholder extends Type {
  override def toString: String = "?"
}

final case class TypeIdentifier(name: String)
    extends Type
    with Kind.Identifier {
  override def toString: String = "_" + name
}

final case class FunType[T <: Type, U <: Type](inT: T, outT: U)
    extends Type {
  override def toString: String = s"($inT -> $outT)"
}

final case class DepFunType[K <: Kind: KindName, T <: Type](
    x: K#I with Kind.Explicitness,
    t: T
) extends Type {
  override def toString: String =
    s"(${x.name}: ${implicitly[KindName[K]].get} -> $t)"
}

// == Data types ==============================================================

sealed trait DataType extends Type

final case class DataTypeIdentifier(name: String,
                                    override val isExplicit: Boolean = false
                                   ) extends DataType
  with Kind.Identifier
  with Kind.Explicitness {
  override def toString: String = if (isExplicit) name else "_" + name
  override def asExplicit: DataTypeIdentifier = this.copy(isExplicit = true)
  override def asImplicit: DataTypeIdentifier = this.copy(isExplicit = false)
  override def hashCode(): Int = this.name.hashCode()
}

sealed trait ScalarType extends DataType

object bool extends ScalarType { override def toString: String = "bool" }

object int extends ScalarType { override def toString: String = "int" }

object i8  extends ScalarType { override def toString: String = "i8"  }
object i16 extends ScalarType { override def toString: String = "i16" }
object i32 extends ScalarType { override def toString: String = "i32" }
object i64 extends ScalarType { override def toString: String = "i64" }

object u8  extends ScalarType { override def toString: String = "u8"  }
object u16 extends ScalarType { override def toString: String = "u16" }
object u32 extends ScalarType { override def toString: String = "u32" }
object u64 extends ScalarType { override def toString: String = "u64" }

object f16 extends ScalarType { override def toString: String = "f16" }
object f32 extends ScalarType { override def toString: String = "f32" }
object f64 extends ScalarType { override def toString: String = "f64" }

object NatType extends DataType { override def toString: String = "nat" }

// TODO: enforce ScalarType
sealed case class VectorType(size: Nat, elemType: DataType) extends DataType {
  override def toString: String = s"<$size>$elemType"
}

object vec {
  def apply(size: Nat, elemType: DataType): VectorType =
    VectorType(size, elemType)
}

final case class IndexType(size: Nat) extends DataType {
  override def toString: String = s"idx[$size]"
}

final case class PairType(dt1: DataType, dt2: DataType) extends DataType {
  override def toString: String = s"($dt1, $dt2)"
}


final case class DepPairType[K <: Kind: KindName](
                            x: K#I,
                            t: DataType
                           ) extends DataType {
  type Kind = K

  // Note(federico): for pattern-matching purposes, if we ever need to
  // recover the kind name from a pattern-match over just DataType
  val kindName: KindName[K] = implicitly[KindName[K]]

  override def toString: String =
    s"(${x.name}: ${kindName.get} ** $t)"

  override def hashCode(): Int = super.hashCode()
}


final class NatToDataApply(val f: NatToData, val n: Nat) extends DataType {
  override def toString: String = s"$f($n)"
}

object NatToDataApply {
  def apply(f: NatToData, n: Nat): DataType = f match {
    case l: NatToDataLambda     => l.apply(n)
    case i: NatToDataIdentifier => new NatToDataApply(i, n)
  }

  def unapply(arg: NatToDataApply): Option[(NatToData, Nat)] =
    Some((arg.f, arg.n))
}

final case class ArrayType(size: Nat, elemType: DataType) extends DataType {
  override def toString: String = s"$size.$elemType"
}

final case class DepArrayType(size: Nat, fdt: NatToData) extends DataType {
  override def toString: String = s"$size..$fdt"
}

object DepArrayType {
  def apply(size: Nat, f: Nat => DataType): DepArrayType = {
    val n = NatIdentifier(freshName("n"), RangeAdd(0, size, 1), isExplicit = true)
    DepArrayType(size, NatToDataLambda(n, f(n)))
  }
}
