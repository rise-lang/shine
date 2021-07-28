package rise.core

import rise.core.types._

object showScala {
  private val kindIdent : Kind.Identifier => String = {
    case NatKind.Identifier(n) => s"""NatIdentifier("${n.name}", ${n.range})"""
    case DataKind.Identifier(n) => s"""DataTypeIdentifier("$n")"""
    case x => throw new Exception(s"missing rule for $x")
  }

  def nat(n: Nat): String = {
    import arithexpr.arithmetic._
    n match {
      case n: NatIdentifier => s"""NatIdentifier("${n.name}", ${n.range})"""
      case Prod(factors) => factors.map(nat).mkString("(", " * ", ")")
      case Sum(terms) => terms.map(nat).mkString("(", " + ", ")")
      case Cst(c) => s"Cst($c)"
      case _ => throw new Exception(s"missing rule for $n")
    }
  }

  def data(d: semantics.Data): String = {
    import semantics._
    d match {
      case BoolData(b) => s"BoolData($b)"
      case IntData(i) => s"IntData($i)"
      case FloatData(f) => s"FloatData(${f}f)"
      case DoubleData(d) => s"DoubleData($d)"
      case NatData(n) => s"NataData(${nat(n)})"
      case IndexData(i, n) => s"IndexData(${nat(i)}, ${nat(n)})"
      case VectorData(v) => v.map(data).mkString("VectorData(Seq(", ",", "))")
      case ArrayData(a) => a.map(data).mkString("ArrayData(Seq(", ",", "))")
      case PairData(p1, p2) => s"PairData(${data(p1)}, ${data(p2)})"
    }
  }

  def `type`(t: Type): String = { // scalastyle:ignore
    t match {
      case TypePlaceholder => "TypePlaceholder"
      case TypeIdentifier(n) => s"""TypeIdentifier("$n")"""
      case FunType(inT, outT) =>
        s"FunType(${`type`(inT)}, ${`type`(outT)})"
      case DepFunType(k, x, t) =>
        s"DepFunType(${kindIdent(Kind.toIdentifier(k, x))}, ${`type`(t)})"
      case DataTypeIdentifier(n) => s"""DataTypeIdentifier("$n")"""
      case ArrayType(n, e) =>
        s"ArrayType(${nat(n)}, ${`type`(e)})"
      case PairType(p1, p2) =>
        s"PairType(${`type`(p1)}, ${`type`(p2)})"
      case NatType => "NatType"
      case s: ScalarType => s.toString
      case IndexType(n) => s"IndexType(${nat(n)})"
      case VectorType(n, e) =>
        s"VectorType(${nat(n)}, ${`type`(e)})"
      case _ => throw new Exception(s"missing rule for $t")
    }
  }

  def expr(e: Expr): String = {
    e match {
      case Identifier(name) => s"""Identifier("$name")(${`type`(e.t)})"""
      case p: Primitive => s"${p.name}.primitive"
      case TypeAnnotation(e, t) => s"TypeAnnotation(${expr(e)}, ${`type`(t)})"
      case TypeAssertion(e, t) => s"TypeAssertion(${expr(e)}, ${`type`(t)})"
      case Opaque(e, t) => s"Opaque(${expr(e)}, ${`type`(t)})"
      case Literal(d) => s"Literal(${data(d)})"
      case App(f, a) => s"App(${expr(f)}, ${expr(a)})(${`type`(e.t)})"
      case Lambda(x, b) => s"Lambda(${expr(x)}, ${expr(b)})(${`type`(e.t)})"
      case DepApp(NatKind, f, v: Nat) =>
        s"DepApp(NatKind, ${expr(f)}, $v)(${`type`(e.t)})"
      case DepApp(AddressSpaceKind, f, v: AddressSpace) =>
        s"DepApp(AddressSpaceKind, ${expr(f)}, $v)(${`type`(e.t)})"
      case DepApp(_, _, _) => ???
      case DepLambda(k, x, b) => s"DepLambda(${kindIdent(Kind.toIdentifier(k, x))}, ${expr(b)})(${`type`(e.t)})"
    }
  }
}
