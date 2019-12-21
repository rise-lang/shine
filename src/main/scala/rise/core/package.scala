package rise

import arithexpr.arithmetic._
import rise.core.types._

package object core {
  object freshName {
    private var counter = 0

    def apply(prefix: String): String = {
      counter += 1
      prefix + counter
    }
  }

  type Nat = ArithExpr
  //type NatIdentifier = NamedVar with types.Kind.Identifier with types.Kind.Binder

  def toEvaluableString(e: Expr): String = {
    e match {
      case Identifier(name) => s"""Identifier("id$name")""" // id prefix prevents name clashes with freshName
      case Lambda(x, e) => s"Lambda(${toEvaluableString(x)}, ${toEvaluableString(e)})"
      case App(f, e) => s"Apply(${toEvaluableString(f)}, ${toEvaluableString(e)})"
      case DepLambda(x, e) => x match {
        case n: NatIdentifier => s"""DepLambda[NatKind](NatIdentifier("id$n"), ${toEvaluableString(e)})"""
        case dt: DataTypeIdentifier => s"""DepLambda[DataKind]("id$dt", ${toEvaluableString(e)})"""
      }
      case DepApp(f, x) => x match {
        case n: Nat => s"DepApply[NatKind](${toEvaluableString(f)}, $n)"
        case dt: DataType => s"DepApply[DataKind](${toEvaluableString(f)}, $dt)"
      }
      case Literal(d) => s"Literal($d)"
      case ff: ForeignFunction => ff.toString
      case p: Primitive => p.toString
    }
  }
}
