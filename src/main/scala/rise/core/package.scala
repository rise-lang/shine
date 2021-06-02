package rise

import rise.core.types._

import scala.language.implicitConversions

package object core {
  object freshName {
    private var counter = 0

    def apply(prefix: String): String = {
      counter += 1
      prefix + counter
    }
  }

  def toEvaluableString(e: Expr): String = {
    e match {
      case Identifier(name) =>
        // id prefix prevents name clashes with freshName
        s"""Identifier("id$name")"""
      case Lambda(x, e) =>
        s"Lambda(${toEvaluableString(x)}, ${toEvaluableString(e)})"
      case App(f, e) =>
        s"Apply(${toEvaluableString(f)}, ${toEvaluableString(e)})"
      case DepLambda(_, x, e) =>
        x match {
          case n: NatIdentifier =>
            s"""DepLambda[NatKind](NatIdentifier("id$n"),
               | ${toEvaluableString(e)})""".stripMargin
          case dt: DataTypeIdentifier =>
            s"""DepLambda[DataKind]("id$dt", ${toEvaluableString(e)})"""
        }
      case DepApp(_, f, x) =>
        x match {
          case n: Nat => s"DepApply[NatKind](${toEvaluableString(f)}, $n)"
          case dt: DataType =>
            s"DepApply[DataKind](${toEvaluableString(f)}, $dt)"
        }
      case Literal(d)           => s"Literal($d)"
      case TypeAnnotation(e, t) => s"TypeAnnotation(${toEvaluableString(e)}, $t)"
      case TypeAssertion(e, t)  => s"TypeAssertion(${toEvaluableString(e)}, $t)"
      case Opaque(e, t)         => s"Opaque(${toEvaluableString(e)}, $t)"
      case p: Primitive         => p.toString
    }
  }

  implicit def primitiveBuilderToPrimitive(pb: Builder
                                          ): DSL.ToBeTyped[Primitive] =
    pb.apply
}
