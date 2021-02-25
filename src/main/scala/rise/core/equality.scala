package rise.core

import rise.core.types._
import util.PatternMatching

object equality {
  case class Env[T](unwrap : Map[T, T]) {
    def add(x : T, y : T): Env[T] = Env(unwrap.updated(x, y))
    def check(x : T, y : T) : Boolean = unwrap.get(x).contains(y)
  }
  object Env {
    def apply[T]() : Env[T] = Env(Map())
  }

  type TypeEnv = Env[Kind.Identifier]
  type TypeEq = TypeEnv => Type => Type => Boolean
  type ExprEnv = Env[String]
  type ExprEq = TypeEq => TypeEnv => ExprEnv => Expr => Expr => Boolean

  // TODO: move to utils?
  private def makeExplicit[K <: Kind.Identifier]: K => K = {
    case t: DataTypeIdentifier => t.asExplicit.asInstanceOf[K]
    case t => t
  }
  val equivNat: TypeEnv => Nat => Nat => Boolean = env => a => b => {
    val natEnv = env.unwrap.collect { case (i: NatIdentifier, n: Nat) => (i, n) }
    // substitutes elements on the left with elements on the right
    substitute.natsInNat(natEnv, a) == b
  }

  object typeEq {
    object typeErasure {
      def apply(a : Type, b : Type) : Boolean = equiv(Env())(a)(b)
      val equiv : TypeEq = _ => _ => _ => true
      val hash: Type => Int = _ => 0
    }
    
    object unificationAlphaEquivalence {
      def apply(a : Type, b : Type) : Boolean = equiv(Env())(a)(b)
      val equiv : TypeEq = env => a => b => (a, b) match {
        case (TypePlaceholder, _) => true
        case (_, TypePlaceholder) => true
        case _ => alphaEquivalence.equiv(env)(a)(b)
      }
      val hash: Type => Int = _ => 0
    }

    object alphaEquivalence {
      def apply(a : Type, b : Type) : Boolean = equiv(Env())(a)(b)

      /** Alpha equivalence on types.
        * Datatype identifier explicitness is ignored.
        * Short circuits in the event of syntactic equality.
        * Kind equality is checked on dependent functions and pairs.
        */
      val equiv: TypeEq = env => a => b => {
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
          case DepArrayType(sa, da) => and { case DepArrayType(sb, db) => da == db && equivNat(env)(sa)(sb) }

          // Should we move this into its own equality check?
          case NatToDataApply(fa, na) => and { case NatToDataApply(fb, nb) =>
            val and = PatternMatching.matchWithDefault(fb, false)
            equivNat(env)(na)(nb) && (fa match {
              case na: NatToDataIdentifier => and { case nb: NatToDataIdentifier => env.check(na, nb) }
              case NatToDataLambda(xa, ba) => and { case NatToDataLambda(xb, bb) => equiv(env.add(xa, xb))(ba)(bb) }
            })
          }

          // Recursive cases
          case FunType(sa, ta) => and { case FunType(sb, tb) => equiv(env)(sa)(sb) && equiv(env)(ta)(tb) }
          case PairType(la, ra) => and { case PairType(lb, rb) => equiv(env)(la)(lb) && equiv(env)(ra)(rb) }
          case VectorType(sa, da) => and { case VectorType(sb, db) => equivNat(env)(sa)(sb) && equiv(env)(da)(db) }
          case ArrayType(sa, da) => and { case ArrayType(sb, db) => equivNat(env)(sa)(sb) && equiv(env)(da)(db) }

          // Recursive cases -> binding tracking
          case DepFunType(xa, ta) => and { case DepFunType(xb, tb) =>
            xa.getClass == xb.getClass && equiv(env.add(makeExplicit(xa), makeExplicit(xb)))(ta)(tb)
          }
          case DepPairType(xa, ta) => and { case DepPairType(xb, tb) =>
            xa.getClass == xb.getClass && equiv(env.add(makeExplicit(xa), makeExplicit(xb)))(ta)(tb)
          }
        })
      }

      /** Alpha renaming respecting hash function on types.
        * All identifiers are considered equal and therefore ignored.
        */
      val hash: Type => Int = {
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
  }

  object exprEq {
    /** Alpha equivalence on expressions.
      * Parametrised by an equality relation on type.
      * Datatype identifier explicitness is ignored.
      * Kind equality is checked on dependent functions and pairs.
      */
    object alphaEquivalence {
      def apply(typeEq : TypeEq, a : Expr, b : Expr) : Boolean = equiv(typeEq)(Env())(Env())(a)(b)

      val equiv : ExprEq = typeEq => typeEnv => exprEnv => a => b => {
        val and = PatternMatching.matchWithDefault(b, false) // Make the match exhaustive
        typeEq(typeEnv)(a.t)(b.t) && (a match {
          case Identifier(na) => and { case Identifier(nb) => na == nb || exprEnv.check(na, nb)}
          case Literal(da) => and { case Literal(db) => da == db }
          case a: Primitive => and { case b: Primitive => a.name == b.name }
          case App(fa, ea) => and { case App(fb, eb) =>
            equiv(typeEq)(typeEnv)(exprEnv)(fa)(fb) && equiv(typeEq)(typeEnv)(exprEnv)(ea)(eb) }
          case DepApp(fa, xa) => and { case DepApp(fb, xb) =>
            xa == xb && equiv(typeEq)(typeEnv)(exprEnv)(fa)(fb)} // FIXME: xa == xb is not alpha equivalence
          case Lambda(xa, ta) => and { case Lambda(xb, tb) =>
            typeEq(typeEnv)(xa.t)(xb.t) && equiv(typeEq)(typeEnv)(exprEnv.add(xa.name, xb.name))(ta)(tb) }
          case DepLambda(xa, ea) => and { case DepLambda(xb, eb) =>
            xa.getClass == xb.getClass && equiv(typeEq)(typeEnv.add(makeExplicit(xa), makeExplicit(xb)))(exprEnv)(ea)(eb) }
        })
      }

      /** Alpha renaming respecting hash function on expressions.
        * Parametrised by a hash function on types.
        * All identifiers are considered equal and therefore ignored.
        */
      val hash: (Type => Int) => Expr => Int = typeHash => {
        case i: Identifier => 5 + typeHash(i.t)
        case Lambda(x, e) => 7 * e.hashCode() + typeHash(x.t) + typeHash(e.t)
        case App(f, e) => 11 * f.hashCode() + 13 * e.hashCode() + typeHash(f.t) + typeHash(e.t)
        case DepLambda(x, e) => 17 * e.hashCode() + typeHash(e.t)
        case DepApp(f, _) => 19 * f.hashCode() + typeHash(f.t)
        case l : Literal => typeHash(l.t)
        case p: Primitive => p.name.hashCode() + typeHash(p.t)
      }
    }
  }
}
