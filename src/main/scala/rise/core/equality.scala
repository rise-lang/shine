package rise.core

import rise.core.semantics.{ArrayData, IndexData, NatData, PairData, ScalarData, VectorData}
import rise.core.types._
import util.PatternMatching

object equality {
  type TypeEnv = List[(Kind.Identifier, Kind.Identifier)]

  object typeEq {
    object unificationAlphaEquivalence {
      val equiv: TypeEnv => Type => Type => Boolean = env => a => b => (a, b) match {
        case (TypePlaceholder, _) => true
        case (_, TypePlaceholder) => true
        case _ => alphaEquivalence.equiv(env)(a)(b)
      }
      val hash: Type => Int = _ => 0
    }
    object alphaEquivalence {
      // TODO: move to utils?
      private def makeExplicit[K <: Kind.Identifier]: K => K = {
        case t: DataTypeIdentifier => t.asExplicit.asInstanceOf[K]
        case t => t
      }

      val equivNat: TypeEnv => Nat => Nat => Boolean = env => a => b => {
        val natEnv = env.collect { case (i: NatIdentifier, n: Nat) => (i, n) }
        // substitutes elements on the left with elements on the right
        substitute.natsInNat(natEnv.toMap, a) == b
      }

      /** Alpha equivalence on types
        * Datatype identifier explicitness is ignored.
        * Short circuits in the event of syntactic equality.
        * Kind equality is checked on dependent functions and pairs.
        */
      val equiv: TypeEnv => Type => Type => Boolean = env => a => b => {
        val and = PatternMatching.matchWithDefault(b, false)
        a == b || (a match {
          // Base cases
          case TypePlaceholder => and { case TypePlaceholder => true }
          case NatType => and { case NatType => true }
          case sa: ScalarType => and { case sb: ScalarType => sa == sb }

          // Base cases -> identifier lookup
          case na: TypeIdentifier => and { case nb: TypeIdentifier => env.contains((na, nb)) }
          case na: DataTypeIdentifier => and { case nb: DataTypeIdentifier =>
            na.asExplicit == nb.asExplicit || env.contains((na.asExplicit, nb.asExplicit))
          }

          // Base cases -> identifier lookup in nat expressions
          case IndexType(sa) => and { case IndexType(sb) => equivNat(env)(sa)(sb) }
          case DepArrayType(sa, da) => and { case DepArrayType(sb, db) => equivNat(env)(sa)(sb) && da == db }

          // Should we move this into its own equality check?
          case NatToDataApply(fa, na) => and { case NatToDataApply(fb, nb) =>
            val and = PatternMatching.matchWithDefault(fb, false)
            equivNat(env)(na)(nb) && (fa match {
              case na: NatToDataIdentifier => and { case nb: NatToDataIdentifier => env.contains((na, nb)) }
              case NatToDataLambda(xa, ba) => and { case NatToDataLambda(xb, bb) => equiv((xa, xb) :: env)(ba)(bb) }
            })
          }

          // Recursive cases
          case FunType(sa, ta) => and { case FunType(sb, tb) => equiv(env)(sa)(sb) && equiv(env)(ta)(tb) }
          case PairType(la, ra) => and { case PairType(lb, rb) => equiv(env)(la)(lb) && equiv(env)(ra)(rb) }
          case VectorType(sa, da) => and { case VectorType(sb, db) => equivNat(env)(sa)(sb) && equiv(env)(da)(db) }
          case ArrayType(sa, da) => and { case ArrayType(sb, db) => equivNat(env)(sa)(sb) && equiv(env)(da)(db) }

          // Recursive cases -> binding tracking
          case DepFunType(xa, ta) => and { case DepFunType(xb, tb) =>
            xa.getClass == xb.getClass && equiv((makeExplicit(xa), makeExplicit(xb)) :: env)(ta)(tb)
          }
          case DepPairType(xa, ta) => and { case DepPairType(xb, tb) =>
            xa.getClass == xb.getClass && equiv((makeExplicit(xa), makeExplicit(xb)) :: env)(ta)(tb)
          }
        })
      }

      val hash: Type => Int = {
        case TypePlaceholder => 5
        case TypeIdentifier(n) => 7 * n.hashCode()
        case FunType(inT, outT) => 11 * inT.hashCode() + 13 * outT.hashCode() + 1
        case DepFunType(_, t) => 17 * t.hashCode()
        case dataType: DataType => 19 * dataType.hashCode()
      }
    }
  }

  object exprEq {
    type TypeEq = Type => Type => Boolean

    object alphaEquivalence {
      val equiv: TypeEq => Expr => Expr => Boolean = typeEq => a => b => {
        // Make the match exhaustive
        val and = PatternMatching.matchWithDefault(b, false)
        typeEq(a.t)(b.t) && (a match {
          case Identifier(na) => and { case Identifier(nb) => na == nb }
          case Literal(da) => and { case Literal(db) => da == db }
          case a: Primitive => and { case b: Primitive => a.name == b.name }
          // Application compares structurally
          case App(fa, ea) => and { case App(fb, eb) => equiv(typeEq)(fa)(fb) && equiv(typeEq)(ea)(eb) }
          case DepApp(fa, xa) => and { case DepApp(fb, xb) => equiv(typeEq)(fa)(fb) && xa == xb }
          // Abstraction compares after substitution
          case Lambda(xa, ta) => and { case other@Lambda(xb, _) =>
            typeEq(xa.t)(xb.t) && equiv(typeEq)(ta)(typedLifting.liftFunExpr(other).value(xa))
          }
          case DepLambda(xa, ea) => and { case other@DepLambda(xb, _) =>
            val and = PatternMatching.matchWithDefault(xb, false)
            xa match {
              case n: NatIdentifier => and { case _: NatIdentifier =>
                equiv(typeEq)(ea)(typedLifting.liftDepFunExpr[NatKind](other).value(n))
              }
              case dt: DataTypeIdentifier => and { case _: DataTypeIdentifier =>
                equiv(typeEq)(ea)(typedLifting.liftDepFunExpr[DataKind](other).value(dt))
              }
              case addr: AddressSpaceIdentifier => and { case _: AddressSpaceIdentifier =>
                equiv(typeEq)(ea)(typedLifting.liftDepFunExpr[AddressSpaceKind](other).value(addr))
              }
              case n2n: NatToNatIdentifier => and { case _: NatToNatIdentifier =>
                equiv(typeEq)(ea)(typedLifting.liftDepFunExpr[NatToNatKind](other).value(n2n))
              }
              case n2d: NatToDataIdentifier => and { case _: NatToDataIdentifier =>
                equiv(typeEq)(ea)(typedLifting.liftDepFunExpr[NatToDataKind](other).value(n2d))
              }
            }
          }
        })
      }

      val hash: Expr => Int = {
        case _: Identifier => 17
        case Lambda(_, e) => 3 * e.hashCode() + 1
        case App(f, e) => 5 * f.hashCode() + -7 * e.hashCode() + 2
        case DepLambda(_, e) => 4 * e.hashCode() + 3
        case DepApp(f, _) => 6 * f.hashCode() + 4
        case l@Literal(_: ScalarData | _: VectorData) => l.d.hashCode()
        case Literal(_: NatData) => 91
        case Literal(_: IndexData) => 93
        case Literal(_: ArrayData) => 95
        case Literal(_: PairData) => 97
        case p: Primitive => p.getClass.hashCode()
      }
    }
  }
}
