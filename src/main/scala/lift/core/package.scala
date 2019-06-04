package lift

import lift.arithmetic._

package object core {
  object freshName {
    private var counter = 0

    def apply(prefix: String): String = {
      counter += 1
      prefix + counter
    }
  }

  type Nat = ArithExpr
  type NatIdentifier = NamedVar with types.Kind.Identifier

  object NatIdentifier {
    def apply(name: String): NatIdentifier = new NamedVar(name) with types.Kind.Identifier
    def apply(name: String, range: Range): NatIdentifier = new NamedVar(name, range) with types.Kind.Identifier
    def apply(nv: NamedVar): NatIdentifier = new NamedVar(nv.name, nv.range) with types.Kind.Identifier
  }

//  case class NatNatTypeFunction private (x:NatIdentifier, body:Nat) {
//    //NatNatTypeFunction have an interesting comparison behavior, as we do not define
//    //equality for them as simple syntactic equality: we just want to make sure their bodies
//    //are equal up-to renaming of the binder.
//
//    //However, just updating equals is not sufficient, as many data structures, such as HashMaps,
//    //use hashCodes as proxy for equality. In order to make sure this property is respected, we ignore
//    //the identifier variable, and just take the hash of the body evaluated at a known point
//    override def hashCode(): Int = this(NamedVar("comparisonDummy")).hashCode()
//
//    def apply(n: Nat): Nat = ArithExpr.substitute(body, Map((x, n)))
//
//    override def toString: String = s"($x:nat) -> $body"
//
//    override def equals(obj: Any): Boolean = {
//      obj match {
//        case other:NatNatTypeFunction => body == other(x)
//        case _ => false
//      }
//    }
//  }

//  object NatNatTypeFunction {
//    def apply(upperBound:Nat, f:NatIdentifier => Nat):NatNatTypeFunction = {
//      val x = NamedVar(freshName("n"), RangeAdd(0, upperBound, 1))
//      NatNatTypeFunction(x, f(x))
//    }
//
//    def apply(upperBound:Nat, id:NatIdentifier, body:Nat):NatNatTypeFunction = {
//      val x = NamedVar(freshName("n"), RangeAdd(0, upperBound, 1))
//      NatNatTypeFunction(x, x => ArithExpr.substitute(body, Map((id, x))))
//    }
//  }

//  case class NatDataTypeFunction private (x:NatIdentifier, body:DataType) {
//    //See hash code of NatNatTypeFunction
//    override def hashCode(): Int = this(NamedVar("ComparisonDummy")).hashCode()
//
//    def apply(n:Nat): DataType =
//      substitute(n, `for`=x, `in`=body).asInstanceOf[DataType]
//
//    override def toString: String = s"($x:nat) -> $body"
//
//    override def equals(obj: Any): Boolean = {
//      obj match {
//        case other:NatDataTypeFunction =>
//          val subbedOther = other(x)
//          val eq = body == subbedOther
//          eq
//        case _ => false
//      }
//    }
//  }
//
//  object NatDataTypeFunction {
//    def apply(upperBound:Nat, f:NatIdentifier => DataType):NatDataTypeFunction = {
//      val x = NamedVar(freshName("n"), RangeAdd(0, upperBound, 1))
//      NatDataTypeFunction(x, f(x))
//    }
//
//    def apply(upperBound:Nat, id:NatIdentifier, body:DataType):NatDataTypeFunction = {
//      val x = NamedVar(freshName("n"), RangeAdd(0, upperBound, 1))
//      NatDataTypeFunction(x, x =>
//        substitute(x, `for`=id, `in`=body).asInstanceOf[DataType])
//    }
//  }

}
