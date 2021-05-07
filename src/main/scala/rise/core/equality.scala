package rise.core

import rise.core.DSL._
import rise.core.semantics._
import rise.core.types._
import util.PatternMatching

object equality {
  case class Env[T](unwrap: Map[T, T]) {
    def add(x: T, y: T): Env[T] = Env(unwrap.updated(x, y))
    def check(x: T, y: T): Boolean = x == y || unwrap.get(x).contains(y)
  }
  object Env {
    def apply[T](): Env[T] = Env(Map())
  }

  val equivNat: Env[Kind.Identifier] => Nat => Nat => Boolean = env => a => b => {
    val natEnv = env.unwrap.collect { case (i: NatIdentifier, n: Nat) => (i, n) }
    // substitutes elements on the left with elements on the right
    substitute.natsInNat(natEnv, a) == b
  }

  trait TypeEq {
    final type Eq[K <: Kind] = K#T => K#T => Boolean
    def apply[K <: Kind] : Eq[K] = equiv[K](Env())
    def equiv[K <: Kind] : Env[Kind.Identifier] => Eq[K]
    def hash[K <: Kind] : K#T => Int
  }

  object typeErasure extends TypeEq {
    override def hash[K <: Kind]: K#T => Int = _ => 0
    override def equiv[K <: Kind]: Env[Kind.Identifier] => Eq[K] = _ => _ => _ => true
  }

  object typePartialAlphaEq extends TypeEq {
    override def hash[K <: Kind]: K#T => Int = _ => 0
    override def equiv[K <: Kind]: Env[Kind.Identifier] => Eq[K] = env => a => b => (a, b) match {
      case (a : Type, b : Type) => (a, b) match {
        case (TypePlaceholder, _) => true
        case (_, TypePlaceholder) => true
        case _ => typeAlphaEq.equiv[TypeKind](env)(a)(b)
      }
      case _ => typeAlphaEq.equiv(env)(a)(b)
    }
  }

  object typeAlphaEq extends TypeEq {
    /** Alpha equivalence on types.
      * Kind equality is checked on dependent functions and pairs.
      */
    override def equiv[K <: Kind]: Env[Kind.Identifier] => Eq[K] = env => a => b => {
      val and = PatternMatching.matchWithDefault(b, false)
      a match {
        case a : Nat => and {case b : Nat => equivNat(env)(a)(b)}
        case a : Type => and {case b : Type => equivType(env)(a)(b) }
        case ia: Kind.Identifier => and { case ib: Kind.Identifier => env.check(ia, ib) }
        case a: AddressSpace => and { case b: AddressSpace => (a : AddressSpace) == (b : AddressSpace) }
        case NatToNatLambda(na, ba) => and { case NatToNatLambda(nb, bb) => equivNat(env.add(na, nb))(ba)(bb) }
        case NatToDataLambda(na, ba) => and { case NatToDataLambda(nb, bb) => equiv[DataKind](env.add(na, nb))(ba)(bb) }
        case NatCollectionFromArray(a) => and { case NatCollectionFromArray(b) => a == b } // FIXME: should use exprEq
      }
    }
    val equivType: Env[Kind.Identifier] => Type => Type => Boolean = env => a => b => {
      val and = PatternMatching.matchWithDefault(b, false)
      a match {
        // Base cases
        case TypePlaceholder => and { case TypePlaceholder => true }
        case NatType => and { case NatType => true }
        case sa: ScalarType => and { case sb: ScalarType => sa == sb }

        // Base cases -> identifier lookup
        case na: TypeIdentifier => and { case nb: TypeIdentifier => env.check(na, nb) }
        case na: DataTypeIdentifier => and { case nb: DataTypeIdentifier => env.check(na, nb)
        }

        // Base cases -> identifier lookup in nat expressions
        case IndexType(sa) => and { case IndexType(sb) => equivNat(env)(sa)(sb) }
        case DepArrayType(sa, da) => and { case DepArrayType(sb, db) =>
          equivNat(env)(sa)(sb) && equiv[NatToDataKind](env)(da)(db) }

        // Should we move this into its own equality check?
        case NatToDataApply(fa, na) => and { case NatToDataApply(fb, nb) =>
          val and = PatternMatching.matchWithDefault(fb, false)
          equivNat(env)(na)(nb) && (fa match {
            case na: NatToDataIdentifier => and { case nb: NatToDataIdentifier => env.check(na, nb) }
            case NatToDataLambda(xa, ba) => and { case NatToDataLambda(xb, bb) => equivType(env.add(xa, xb))(ba)(bb) }
          })
        }

        // Recursive cases
        case FunType(sa, ta) => and { case FunType(sb, tb) => equivType(env)(sa)(sb) && equivType(env)(ta)(tb) }
        case PairType(la, ra) => and { case PairType(lb, rb) => equivType(env)(la)(lb) && equivType(env)(ra)(rb) }
        case VectorType(sa, da) => and { case VectorType(sb, db) => equivNat(env)(sa)(sb) && equivType(env)(da)(db) }
        case ArrayType(sa, da) => and { case ArrayType(sb, db) => equivNat(env)(sa)(sb) && equivType(env)(da)(db) }
        case FragmentType(ra, ca, da, dta, fka, mla) => and { case FragmentType(rb, cb, db, dtb, fkb, mlb) =>
          equivNat(env)(ra)(rb) && equivNat(env)(ca)(cb) && equivNat(env)(da)(db) &&
          equivType(env)(dta)(dtb) && fka == fkb && mla == mlb
        }

        // Recursive cases -> binding tracking
        case DepFunType(xa, ta) => and { case DepFunType(xb, tb) =>
          xa.getClass == xb.getClass && equivType(env.add(xa, xb))(ta)(tb)
        }
        case DepPairType(xa, ta) => and { case DepPairType(xb, tb) =>
          xa.getClass == xb.getClass && equivType(env.add(xa, xb))(ta)(tb)
        }
      }
    }

    /** Alpha renaming respecting hash function on types.
      * All identifiers are considered equal and therefore ignored.
      */
    override def hash[K <: Kind]: K#T => Int = {
      case t: Type => hashType(t)
      case _: Kind.Identifier => 7
      case a: AddressSpace => a.hashCode()
      case NatToNatLambda(na, ba) => hash[NatKind](ba)
      case NatToDataLambda(na, ba) => hashType(ba)
      case NatCollectionFromArray(a) => 17
    }
    val hashType: Type => Int = {
      case TypePlaceholder => 5
      case TypeIdentifier(_) => 7
      case DataTypeIdentifier(_) => 11
      case FunType(inT, outT) => 13 * hashType(inT) + 17 * hashType(outT)
      case DepFunType(_, t) => 19 * hashType(t)
      case st: ScalarType => 29 * st.hashCode()
      case NatType => 23
      case VectorType(size, elemType) => 31 * hash[NatKind](size) + 37 * hashType(elemType)
      case IndexType(size) => 41 * hash[NatKind](size)
      case PairType(dt1, dt2) => 43 * hashType(dt1) + 47 * hashType(dt2)
      case DepPairType(x, t) => 53 * hashType(t)
      case NatToDataApply(f, n) => 59 * hash[NatToDataKind](f) + 61 * hash[NatKind](n)
      case ArrayType(size, elemType) => 67 * hash[NatKind](size) + 71 * hashType(elemType)
      case DepArrayType(size, fdt) => 73 * hash[NatKind](size) + 79 * hash[NatToDataKind](fdt)
      case FragmentType(r, c, d, dt, fk, ml) => 83 * hash[NatKind](r) + 89 * hash[NatKind](c) + 97 * hash[NatKind](d) +
        101 * hashType(dt) + 103*fk.hashCode()
    }
  }

  trait ExprEq {
    final type Eq = Expr => Expr => Boolean
    def apply : Eq = equiv(Env())(Env())
    def typeEq : TypeEq
    val equiv: Env[Kind.Identifier] => Env[String] => Eq
    val hash: Expr => Int
  }

  case class exprAlphaEq(typeEq : TypeEq) extends ExprEq {
    /** Alpha equivalence on expressions.
      * Parametrised by an equality relation on type.
      * Kind equality is checked on dependent functions and pairs.
      */
    override val equiv: Env[Kind.Identifier] => Env[String] => Eq = typeEnv => exprEnv => a => b => {
      val and = PatternMatching.matchWithDefault(b, false) // Make the match exhaustive
      typeEq.equiv[TypeKind](typeEnv)(a.t)(b.t) && (a match {
        case Identifier(na) => and { case Identifier(nb) => exprEnv.check(na, nb)}
        case Literal(da) => and { case Literal(db) => equivData(typeEnv)(da)(db) }
        case App(fa, ea) => and { case App(fb, eb) =>
          equiv(typeEnv)(exprEnv)(fa)(fb) && equiv(typeEnv)(exprEnv)(ea)(eb) }
        case DepApp(fa, xa) => and { case DepApp(fb, xb) =>
          typeEq.equiv(typeEnv)(xa)(xb) && equiv(typeEnv)(exprEnv)(fa)(fb)}
        case Lambda(xa, ta) => and { case Lambda(xb, tb) =>
          typeEq.equiv[TypeKind](typeEnv)(xa.t)(xb.t) && equiv(typeEnv)(exprEnv.add(xa.name, xb.name))(ta)(tb) }
        case DepLambda(xa, ea) => and { case DepLambda(xb, eb) =>
          xa.getClass == xb.getClass && equiv(typeEnv.add(xa, xb))(exprEnv)(ea)(eb) }
        case Opaque(e1, t1) => and { case Opaque(e2, t2) =>
          equiv(typeEnv)(exprEnv)(e1)(e2) && typeEq.equiv[TypeKind](typeEnv)(t1)(t2) }
        case TypeAnnotation(e1, t1) => and { case TypeAnnotation(e2, t2) =>
          equiv(typeEnv)(exprEnv)(e1)(e2) && typeEq.equiv[TypeKind](typeEnv)(t1)(t2) }
        // TODO: TopLevel
        case a: Primitive => and { case b: Primitive => a.primEq(b) }
      })
    }

    val equivData: Env[Kind.Identifier] => Data => Data => Boolean = typeEnv => a => b => {
      val and = PatternMatching.matchWithDefault(b, false) // Make the match exhaustive
      a match {
        case NatData(n1) => and { case NatData(n2) =>
          equivNat(typeEnv)(n1)(n2) }
        case IndexData(i1, n1) => and { case IndexData(i2, n2) =>
          equivNat(typeEnv)(i1)(i2) && equivNat(typeEnv)(n1)(n2) }
        case d1: ScalarData => and { case d2 : ScalarData => d1 == d2 }
        case VectorData(v1) => and { case VectorData(v2) => v1 == v2 }
        case ArrayData(as1) => and { case ArrayData(as2) =>
          (as1 zip as2).forall { case (a1, a2) => equivData(typeEnv)(a1)(a2) } }
        case PairData(l1, r1) => and { case PairData(l2, r2) =>
          equivData(typeEnv)(l1)(l2) && equivData(typeEnv)(r1)(r2) }
      }
    }

    /** Alpha renaming respecting hash function on expressions.
      * Parametrised by a hash function on types.
      * All identifiers are considered equal and therefore ignored.
      */
    override val hash: Expr => Int = {
      case i: Identifier => 5 + typeEq.hash[TypeKind](i.t)
      case Lambda(x, e) => 7 * hash(e) + typeEq.hash[TypeKind](x.t) + typeEq.hash[TypeKind](e.t)
      case App(f, e) => 11 * hash(f) + 13 * hash(e) + typeEq.hash[TypeKind](f.t) + typeEq.hash[TypeKind](e.t)
      case DepLambda(x, e) => 17 * hash(e) + typeEq.hash[TypeKind](e.t)
      case DepApp(f, _) => 19 * hash(f) + typeEq.hash[TypeKind](f.t)
      case l@Literal(_: ScalarData | _: VectorData) => l.d.hashCode()
      case Literal(_: NatData) => 91
      case Literal(_: IndexData) => 93
      case Literal(_: ArrayData) => 95
      case Literal(_: PairData) => 97
      case Opaque(e, t) => 101*hash(e) + 103*typeEq.hash[TypeKind](t)
      case TypeAnnotation(e, t) => 107*hash(e) + 109*typeEq.hash[TypeKind](t)
      case p: Primitive => 131*p.name.hashCode() + 137*typeEq.hash[TypeKind](p.t)
    }
  }
}