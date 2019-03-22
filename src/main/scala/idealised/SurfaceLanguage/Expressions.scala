package idealised.SurfaceLanguage

import idealised.SurfaceLanguage.Semantics.Data
import idealised.SurfaceLanguage.Types._
import idealised.traversal.{FromChildren, ToChildren}

sealed abstract class Expr extends ToChildren with FromChildren[Expr] {
  def t: Option[Type]
}

final case class IdentifierExpr(name: String,
                                override val t: Option[DataType] = None) extends Expr {
  override def toString: String = name

  override def children: Seq[Any] = Seq(t)

  override def rebuild: Seq[Any] => Expr = {
    case Seq(t: Option[DataType]) => IdentifierExpr(name, t)
  }
}

final case class LambdaExpr(param: IdentifierExpr, body: Expr, override val t: Option[Type] = None) extends Expr {
  override def toString: String = {
    val pName = param.name
    val pType = param.t match {
      case None => "NoType"
      case Some(dt) => dt.toString
    }
    s"λ $pName: $pType -> $body"
  }

  override def children: Seq[Any] = Seq(param, body, t)

  override def rebuild: Seq[Any] => Expr = {
    case Seq(param: IdentifierExpr, body: Expr, t: Option[Type]) => LambdaExpr(param, body, t)
  }
}

final case class ApplyExpr(fun: Expr, arg: Expr, override val t: Option[Type] = None) extends Expr {
  override def toString: String = s"($fun)($arg)"

  override def children: Seq[Any] = Seq(fun, arg, t)

  override def rebuild: Seq[Any] => Expr = {
    case Seq(fun: Expr, arg: Expr, t: Option[Type]) => ApplyExpr(fun, arg, t)
  }
}

final case class NatDependentLambdaExpr(x: NatIdentifier, body: Expr, override val t: Option[Type] = None) extends Expr {
  override def toString: String = s"Λ ($x : nat) -> $body"

  override def children: Seq[Any] = Seq(x, body, t)

  override def rebuild: Seq[Any] => Expr = {
    case Seq(x: NatIdentifier, body: Expr, t: Option[Type]) =>
      NatDependentLambdaExpr(x, body, t)
  }
}

final case class NatDependentApplyExpr(fun: Expr, arg: Nat, override val t: Option[Type] = None) extends Expr {
  override def toString: String = s"($fun)($arg)"

  override def children: Seq[Any] = Seq(fun, arg, t)

  override def rebuild: Seq[Any] => Expr = {
    case Seq(fun: Expr, arg: Nat, t: Option[Type]) =>
      NatDependentApplyExpr(fun, arg, t)
  }
}

final case class TypeDependentLambdaExpr(x: DataTypeIdentifier, body: Expr, override  val t: Option[Type] = None) extends Expr {
  override def toString: String = s"Λ ($x : dt) -> $body"

  override def children: Seq[Any] = Seq(x, body, t)

  override def rebuild: Seq[Any] => Expr = {
    case Seq(x: DataTypeIdentifier, body: Expr, t: Option[Type]) =>
      TypeDependentLambdaExpr(x, body, t)
  }
}

final case class TypeDependentApplyExpr(fun: Expr, arg: DataType, override val t: Option[Type] = None) extends Expr {
  override def toString: String = s"($fun)($arg)"

  override def children: Seq[Any] = Seq(fun, arg, t)

  override def rebuild: Seq[Any] => Expr = {
    case Seq(fun: Expr, arg: DataType, t: Option[Type]) =>
      TypeDependentApplyExpr(fun, arg, t)
  }
}

final case class IfThenElseExpr(cond: Expr, thenE: Expr, elseE: Expr, override val t: Option[Type] = None) extends Expr {
  override def toString: String = s"if ($cond) then ($thenE) else ($elseE)"

  override def children: Seq[Any] = Seq(cond, thenE, elseE, t)

  override def rebuild: Seq[Any] => Expr = {
    case Seq(cond: Expr, thenE: Expr, elseE: Expr, t: Option[Type]) =>
      IfThenElseExpr(cond, thenE, elseE, t)
  }
}

final case class UnaryOpExpr(op: Operators.Unary.Value, e: Expr, override val t: Option[Type] = None) extends Expr {
  override def toString: String = s"$op $e"

  override def children: Seq[Any] = Seq(e, t)

  override def rebuild: Seq[Any] => Expr = {
    case Seq(e: Expr, t: Option[Type]) => UnaryOpExpr(op, e, t)
  }
}

final case class BinOpExpr(op: Operators.Binary.Value, lhs: Expr, rhs: Expr, override val t: Option[Type] = None) extends Expr {
  override def toString: String = s"$lhs $op $rhs"

  override def children: Seq[Any] = Seq(lhs, rhs, t)

  override def rebuild: Seq[Any] => Expr = {
    case Seq(lhs: Expr, rhs: Expr, t: Option[Type]) => BinOpExpr(op, lhs, rhs, t)
  }
}

final case class LiteralExpr(d: Data, override val t: Option[DataType] = None) extends Expr {
  import idealised.SurfaceLanguage.Semantics.IndexData

  override def toString: String = s"$d"

  override def children: Seq[Any] = {
    d match {
      case IndexData(i, it: IndexType) => Seq(i, it)
      case _ => Seq()
    }
  }

  override def rebuild: Seq[Any] => Expr = {
    case Seq(i: Nat, it: IndexType) =>
      LiteralExpr(IndexData(i, it), Some(it))
    case Seq() => this
  }
}

final case class NatExpr(n: Nat)
  extends Expr
{
  override val t = Some(NatType)

  override def toString: String = s"$n"

  override def children: Seq[Any] = Seq(n, t)

  override def rebuild: Seq[Any] => Expr = {
    case Seq(n: Nat, Some(NatType)) => NatExpr(n)
  }
}

abstract class PrimitiveExpr extends Expr {
  def inferType(subs: Types.TypeInference.SubstitutionMap): Expr
}

object Expr {
  // substitutes `phrase` for `for` in `in`, i.e. in [ phrase / for ]
  def substitute(expr: Expr, `for`: Expr, in: Expr): Expr = {
    object Visitor extends VisitAndRebuild.Visitor {
      import VisitAndRebuild.{Result, Stop, Continue}

      override def apply(e: Expr): Result[Expr] = {
        if (`for` == e) {
          Stop(expr)
        } else {
          Continue(e, this)
        }
      }
    }

    VisitAndRebuild(in, Visitor)
  }
}

object Operators {

  object Unary extends Enumeration {
    val NEG: Unary.Value = Value("-")
  }

  object Binary extends Enumeration {
    val ADD: Binary.Value = Value("+")
    val SUB: Binary.Value = Value("-")
    val MUL: Binary.Value = Value("*")
    val DIV: Binary.Value = Value("/")
    val MOD: Binary.Value = Value("%")
    val GT: Binary.Value = Value(">")
    val LT: Binary.Value = Value("<")
    val EQ: Binary.Value = Value("==")
  }

}