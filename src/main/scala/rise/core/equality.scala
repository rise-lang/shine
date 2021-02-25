package rise.core

import rise.core.types._
import util.PatternMatching

object equality {
  case class Env[T](unwrap: Map[T, T]) {
    def add(x: T, y: T): Env[T] = Env(unwrap.updated(x, y))
    def check(x: T, y: T): Boolean = unwrap.get(x).contains(y)
  }
  object Env {
    def apply[T](): Env[T] = Env(Map())
  }

  // TODO: move to utils?
  private def makeExplicit[K <: Kind.Identifier]: K => K = {
    case t: DataTypeIdentifier => t.asExplicit.asInstanceOf[K]
    case t => t
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

  object typeUnificationAlphaEq extends TypeEq {
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
      * Datatype identifier explicitness is ignored.
      * Short circuits in the event of syntactic equality.
      * Kind equality is checked on dependent functions and pairs.
      */
    override def equiv[K <: Kind]: Env[Kind.Identifier] => Eq[K] = env => a => b => {
      val and = PatternMatching.matchWithDefault(b, false)
      a == b || (a match {
        case a : Type => and {case b : Type => equivType(env)(a)(b) }
        case ia: Kind.Identifier => and { case ib: Kind.Identifier => env.check(makeExplicit(ia), makeExplicit(ib)) }
        case a: AddressSpace => and { case b: AddressSpace => (a : AddressSpace) == (b : AddressSpace) }
        case NatToNatLambda(na, ba) => and { case NatToNatLambda(nb, bb) => equivNat(env.add(na, nb))(ba)(bb) }
        case NatToDataLambda(na, ba) => and { case NatToDataLambda(nb, bb) => equiv[DataKind](env.add(na, nb))(ba)(bb) }
        case NatCollectionFromArray(a) => and { case NatCollectionFromArray(b) => a == b } // FIXME: should use exprEq
      })
    }
    val equivType: Env[Kind.Identifier] => Type => Type => Boolean = env => a => b => {
      val and = PatternMatching.matchWithDefault(b, false)
      a == b || (a match {
        // Base cases
        case TypePlaceholder => and { case TypePlaceholder => true }
        case NatType => and { case NatType => true }
        case sa: ScalarType => and { case sb: ScalarType => sa == sb }

        // Base cases -> identifier lookup
        case na: TypeIdentifier => and { case nb: TypeIdentifier => env.check(na, nb) }
        case na: DataTypeIdentifier => and { case nb: DataTypeIdentifier =>
          na.asExplicit == nb.asExplicit || env.check(na.asExplicit, nb.asExplicit)
        }

        // Base cases -> identifier lookup in nat expressions
        case IndexType(sa) => and { case IndexType(sb) => equivNat(env)(sa)(sb) }
        case DepArrayType(sa, da) => and { case DepArrayType(sb, db) =>
          equivNat(env)(sa)(sb) && equiv[NatToDataKind](env)(da)(db) } // FIXME: record sa == sb in env

        // Should we move this into its own equality check?
        case NatToDataApply(fa, na) => and { case NatToDataApply(fb, nb) =>
          val and = PatternMatching.matchWithDefault(fb, false)
          equivNat(env)(na)(nb) && (fa match {
            case na: NatToDataIdentifier => and { case nb: NatToDataIdentifier => na == nb || env.check(na, nb) }
            case NatToDataLambda(xa, ba) => and { case NatToDataLambda(xb, bb) => equivType(env.add(xa, xb))(ba)(bb) }
          })
        }

        // Recursive cases
        case FunType(sa, ta) => and { case FunType(sb, tb) => equivType(env)(sa)(sb) && equivType(env)(ta)(tb) }
        case PairType(la, ra) => and { case PairType(lb, rb) => equivType(env)(la)(lb) && equivType(env)(ra)(rb) }
        case VectorType(sa, da) => and { case VectorType(sb, db) => equivNat(env)(sa)(sb) && equivType(env)(da)(db) }
        case ArrayType(sa, da) => and { case ArrayType(sb, db) => equivNat(env)(sa)(sb) && equivType(env)(da)(db) }

        // Recursive cases -> binding tracking
        case DepFunType(xa, ta) => and { case DepFunType(xb, tb) =>
          xa.getClass == xb.getClass && equivType(env.add(makeExplicit(xa), makeExplicit(xb)))(ta)(tb)
        }
        case DepPairType(xa, ta) => and { case DepPairType(xb, tb) =>
          xa.getClass == xb.getClass && equivType(env.add(makeExplicit(xa), makeExplicit(xb)))(ta)(tb)
        }
      })
    }

    /** Alpha renaming respecting hash function on types.
      * All identifiers are considered equal and therefore ignored.
      */
    override def hash[K <: Kind]: K#T => Int = {
      case t: Type => hashType(t)
      case _: Kind.Identifier => 7
      case a: AddressSpace => a.hashCode()
      case NatToNatLambda(na, ba) => ba.hashCode()
      case NatToDataLambda(na, ba) => ba.hashCode()
      case NatCollectionFromArray(a) => a.hashCode()
    }
    val hashType: Type => Int = {
      case TypePlaceholder => 5
      case TypeIdentifier(_) => 7
      case DataTypeIdentifier(_, _) => 11
      case FunType(inT, outT) => 13 * inT.hashCode() + 17 * outT.hashCode()
      case DepFunType(_, t) => 19 * t.hashCode()
      case st: ScalarType => 29 * st.hashCode()
      case NatType => 23
      case VectorType(size, elemType) => 31 * size.hashCode() + 37 * elemType.hashCode()
      case IndexType(size) => 41 * size.hashCode()
      case PairType(dt1, dt2) => 43 * dt1.hashCode() + 47 * dt2.hashCode()
      case DepPairType(x, t) => 53 * t.hashCode()
      case NatToDataApply(f, n) => 59 * f.hashCode() + 61 * n.hashCode()
      case ArrayType(size, elemType) => 67 * size.hashCode() + 71 * elemType.hashCode()
      case DepArrayType(size, fdt) => 73 * size.hashCode() + 79 * fdt.hashCode()
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
      * Datatype identifier explicitness is ignored.
      * Kind equality is checked on dependent functions and pairs.
      */
    override val equiv: Env[Kind.Identifier] => Env[String] => Eq = typeEnv => exprEnv => a => b => {
      val and = PatternMatching.matchWithDefault(b, false) // Make the match exhaustive
      typeEq.equiv[TypeKind](typeEnv)(a.t)(b.t) && (a match {
        case Identifier(na) => and { case Identifier(nb) => na == nb || exprEnv.check(na, nb)}
        case Literal(da) => and { case Literal(db) => da == db }
        case a: Primitive => and { case b: Primitive => a.name == b.name }
        case App(fa, ea) => and { case App(fb, eb) =>
          equiv(typeEnv)(exprEnv)(fa)(fb) && equiv(typeEnv)(exprEnv)(ea)(eb) }
        case DepApp(fa, xa) => and { case DepApp(fb, xb) =>
          xa == xb && equiv(typeEnv)(exprEnv)(fa)(fb)} // FIXME: xa == xb is not alpha equivalence }
        case Lambda(xa, ta) => and { case Lambda(xb, tb) =>
          typeEq.equiv[TypeKind](typeEnv)(xa.t)(xb.t) && equiv(typeEnv)(exprEnv.add(xa.name, xb.name))(ta)(tb) }
        case DepLambda(xa, ea) => and { case DepLambda(xb, eb) =>
          xa.getClass == xb.getClass && equiv(typeEnv.add(makeExplicit(xa), makeExplicit(xb)))(exprEnv)(ea)(eb) }
      })
    }

    /** Alpha renaming respecting hash function on expressions.
      * Parametrised by a hash function on types.
      * All identifiers are considered equal and therefore ignored.
      */
    override val hash: Expr => Int = {
      case i: Identifier => 5 + typeEq.hash[TypeKind](i.t)
      case Lambda(x, e) => 7 * e.hashCode() + typeEq.hash[TypeKind](x.t) + typeEq.hash[TypeKind](e.t)
      case App(f, e) => 11 * f.hashCode() + 13 * e.hashCode() + typeEq.hash[TypeKind](f.t) + typeEq.hash[TypeKind](e.t)
      case DepLambda(x, e) => 17 * e.hashCode() + typeEq.hash[TypeKind](e.t)
      case DepApp(f, _) => 19 * f.hashCode() + typeEq.hash[TypeKind](f.t)
      case l : Literal => typeEq.hash[TypeKind](l.t)
      case p: Primitive => p.name.hashCode() + typeEq.hash[TypeKind](p.t)
    }
  }
}