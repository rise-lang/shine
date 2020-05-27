package rise.core

import rise.core.types._

object showScala {
  private def kindIdent[K <: Kind](x: K#I): String = {
    x match {
      case n: NatIdentifier =>
        s"""NatIdentifier("${n.name}", ${n.range}, ${n.isExplicit})"""
      case DataTypeIdentifier(n, isE) =>
        s"""DataTypeIdentifier("$n", $isE)"""
      case _ => throw new Exception(s"missing rule for $x")
    }
  }

  def nat(n: Nat): String = {
    import arithexpr.arithmetic._
    n match {
      case n: NatIdentifier =>
        s"""NatIdentifier("${n.name}", ${n.range}, ${n.isExplicit})"""
      case n: NamedVar =>
        s"""NamedVar("${n.name}", ${n.range})"""
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
      case DepFunType(x, t) =>
        s"DepFunType(${kindIdent(x)}, ${`type`(t)})"
      case DataTypeIdentifier(n, isE) => s"""DataTypeIdentifier("$n", $isE)"""
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
      case Identifier(name) => s"""Identifier("${name}")(${`type`(e.t)})"""
      case p: Primitive => p.name
      case Literal(d) => s"Literal(${data(d)})"
      case App(f, e) => s"app(${expr(f)}, ${expr(e)})"
      case Lambda(x, b) => s"lambda(${expr(x)}, ${expr(b)})"
      case DepApp(f, v: Nat) =>
        s"depApp[NatKind](${expr(f)}, ${v})"
      case DepApp(f, v: AddressSpace) =>
        s"depApp[AddressSpaceKind](${expr(f)}, ${v})"
      case DepApp(f, v) => ???
      case DepLambda(x, b) => s"depLambda(${kindIdent(x)}, ${expr(b)})"
    }
  }
}
