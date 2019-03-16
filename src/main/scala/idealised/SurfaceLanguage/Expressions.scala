package idealised.SurfaceLanguage

import idealised.SurfaceLanguage.Semantics.Data
import idealised.SurfaceLanguage.Types._

sealed abstract class Expr {
  def t: Option[Type]
}

final case class IdentifierExpr(name: String,
                                override val t: Option[DataType] = None) extends Expr {
  override def toString: String = name
}

final case class LambdaExpr(param: IdentifierExpr, body: Expr) extends Expr {
  override lazy val t: Option[Type] = (param.t, body.t) match {
    case (Some(pt), Some(bt)) => Some(FunctionType(pt, bt))
    case _ => None
  }

  override def toString: String = {
    val pName = param.name
    val pType = param.t match {
      case None => "NoType"
      case Some(dt) => dt.toString
    }
    s"λ $pName: $pType -> $body"
  }
}

final case class ApplyExpr(fun: Expr, arg: Expr) extends Expr {
  override lazy val t: Option[Type] = fun.t match {
    case Some(FunctionType(_, outT)) => Some(outT)
    case None => None
  }

  override def toString: String = s"($fun)($arg)"
}

final case class NatDependentLambdaExpr(x: NatIdentifier, body: Expr) extends Expr {
  override lazy val t: Option[Type] = body.t match {
    case Some(bodyT) => Some(NatDependentFunctionType(x, bodyT))
    case None => None
  }

  override def toString: String = s"Λ ($x : nat) -> $body"
}

final case class NatDependentApplyExpr(fun: Expr, arg: Nat) extends Expr {
  override lazy val t: Option[Type] = fun.t match {
    case Some(NatDependentFunctionType(_, bodyT)) => Some(bodyT)
    case None => None
  }

  override def toString: String = s"($fun)($arg)"
}

final case class TypeDependentLambdaExpr(x: DataTypeIdentifier, body: Expr) extends Expr {
  override lazy val t: Option[Type] = body.t match {
    case Some(bodyT) => Some(TypeDependentFunctionType(x, bodyT))
    case _ => None
  }

  override def toString: String = s"Λ ($x : dt) -> $body"
}

final case class TypeDependentApplyExpr(fun: Expr, arg: DataType) extends Expr {
  override lazy val t: Option[Type] = fun.t match {
    case Some(TypeDependentFunctionType(_, bodyT)) => Some(bodyT)
    case None => None
  }

  override def toString: String = s"($fun)($arg)"
}

final case class IfThenElseExpr(cond: Expr, thenE: Expr, elseE: Expr) extends Expr {
  override lazy val t: Option[Type] = (thenE.t, elseE.t) match {
    case (Some(tT), Some(eT)) if tT == eT => Some(tT)
    case _ => None
  }

  override def toString: String = s"if ($cond) then ($thenE) else ($elseE)"
}

final case class UnaryOpExpr(op: Operators.Unary.Value, e: Expr) extends Expr {
  assert(e.isInstanceOf[Expr])
  override lazy val t: Option[Type] = e.t

  override def toString: String = s"$op $e"
}

final case class BinOpExpr(op: Operators.Binary.Value, lhs: Expr, rhs: Expr) extends Expr {
  override lazy val t: Option[Type] = (lhs.t, rhs.t) match {
    case (Some(lhsT), Some(rhsT)) if lhsT == rhsT => Some(lhsT)
    case _ => None
  }

  override def toString: String = s"$lhs $op $rhs"
}

final case class LiteralExpr(d: Data) extends Expr {
  override lazy val t: Option[DataType] = Some(d.dataType)

  override def toString: String = s"$d"
}

abstract class PrimitiveExpr extends Expr {
  def inferType(subs: Types.TypeInference.SubstitutionMap): Expr

  def visitAndRebuild(f: VisitAndRebuild.Visitor): Expr
}

object Expr {
  // substitutes `phrase` for `for` in `in`, i.e. in [ phrase / for ]
  def substitute(expr: Expr, `for`: Expr, in: Expr): Expr = {
    object Visitor extends VisitAndRebuild.Visitor {
      override def apply(e: Expr): Result = {
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