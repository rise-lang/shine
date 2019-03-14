package idealised.SurfaceLanguage

import idealised.DPIA
import idealised.DPIA.error
import idealised.SurfaceLanguage.Semantics.Data
import idealised.SurfaceLanguage.Types._

sealed abstract class Expr {
  def t: Option[Type]

  // TODO: get out of here ... see ToDPIA for first (incomplete attempt)
  def convertToPhrase: DPIA.Phrases.Phrase[_ <: DPIA.Types.PhraseType]

  def toPhrase[PT <: DPIA.Types.PhraseType]: DPIA.Phrases.Phrase[PT] =
    convertToPhrase.asInstanceOf[DPIA.Phrases.Phrase[PT]]
}

final case class IdentifierExpr(name: String,
                                override val t: Option[DataType] = None)
  extends Expr
{
  override def convertToPhrase: DPIA.Phrases.Identifier[DPIA.Types.ExpType] = {
    t match {
      case Some(dt) => DPIA.Phrases.Identifier(name, DPIA.Types.ExpType(DPIA.Types.DataType(dt)))
      case None =>     error(s"Expected identifier to have type")
    }
  }

  override def toString: String = name
}

final case class LambdaExpr(param: IdentifierExpr, body: Expr)
  extends Expr
{
  override lazy val t: Option[Type] = (param.t, body.t) match {
    case (Some(pt), Some(bt)) => Some(FunctionType(pt, bt))
    case _ => None
  }

  override def convertToPhrase: DPIA.Phrases.Lambda[DPIA.Types.ExpType, _ <: DPIA.Types.PhraseType] = {
    DPIA.Phrases.Lambda(param.convertToPhrase, body.convertToPhrase)
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

final case class ApplyExpr(fun: Expr, arg: Expr)
  extends Expr
{
  assert(arg.isInstanceOf[Expr])

  override lazy val t: Option[Type] = fun.t match {
    case Some(FunctionType(_, outT)) => Some(outT)
    case None => None
  }

  override def convertToPhrase: DPIA.Phrases.Apply[DPIA.Types.ExpType, DPIA.Types.PhraseType] = {
    DPIA.Phrases.Apply[DPIA.Types.ExpType, DPIA.Types.PhraseType](
      fun.convertToPhrase.asInstanceOf[DPIA.Phrases.Phrase[DPIA.->[DPIA.Types.ExpType, DPIA.Types.PhraseType]]],
      arg.convertToPhrase.asInstanceOf[DPIA.Phrases.Phrase[DPIA.Types.ExpType]])
  }

  override def toString: String = s"($fun)($arg)"
}

final case class NatDependentLambdaExpr(x: NatIdentifier, body: Expr)
  extends Expr
{
  override lazy val t: Option[Type] = body.t match {
    case Some(bodyT) => Some(NatDependentFunctionType(x, bodyT))
    case None => None
  }

  override def convertToPhrase: DPIA.Phrases.NatDependentLambda[_ <: DPIA.Types.PhraseType] = {
    DPIA.Phrases.NatDependentLambda(x, body.convertToPhrase)
  }

  override def toString: String = s"Λ ($x : nat) -> $body"
}

final case class NatDependentApplyExpr(fun: Expr, arg: Nat)
  extends Expr
{
  override lazy val t: Option[Type] = fun.t match {
    case Some(NatDependentFunctionType(_, bodyT)) => Some(bodyT)
    case None => None
  }

  override def convertToPhrase: DPIA.Phrases.NatDependentApply[DPIA.Types.PhraseType] = {
    DPIA.Phrases.NatDependentApply(
      fun.convertToPhrase.asInstanceOf[DPIA.Phrases.Phrase[DPIA.Types.NatDependentFunctionType[DPIA.Types.PhraseType]]],
      arg)
  }

  override def toString: String = s"($fun)($arg)"
}

final case class TypeDependentLambdaExpr(x: DataTypeIdentifier, body: Expr)
  extends Expr
{
  override lazy val t: Option[Type] = body.t match {
    case Some(bodyT) => Some(TypeDependentFunctionType(x, bodyT))
    case _ => None
  }

  override def convertToPhrase: DPIA.Phrases.TypeDependentLambda[_ <: DPIA.Types.PhraseType] = {
    DPIA.Phrases.TypeDependentLambda(DPIA.Types.DataTypeIdentifier(x.name), body.convertToPhrase)
  }

  override def toString: String = s"Λ ($x : dt) -> $body"
}

final case class TypeDependentApplyExpr(fun: Expr, arg: DataType)
  extends Expr
{
  override lazy val t: Option[Type] = fun.t match {
    case Some(TypeDependentFunctionType(_, bodyT)) => Some(bodyT)
    case None => None
  }

  override def convertToPhrase: DPIA.Phrases.TypeDependentApply[DPIA.Types.PhraseType] = {
    DPIA.Phrases.TypeDependentApply(
      fun.convertToPhrase.asInstanceOf[DPIA.Phrases.Phrase[DPIA.Types.TypeDependentFunctionType[DPIA.Types.PhraseType]]],
      arg)
  }

  override def toString: String = s"($fun)($arg)"
}

final case class IfThenElseExpr(cond: Expr, thenE: Expr, elseE: Expr)
  extends Expr
{
  assert(cond.isInstanceOf[Expr])
  override lazy val t: Option[Type] = (thenE.t, elseE.t) match {
    case (Some(tT), Some(eT)) if tT == eT => Some(tT)
    case _ => None
  }

  override def convertToPhrase: DPIA.Phrases.IfThenElse[DPIA.Types.PhraseType] = {
    DPIA.Phrases.IfThenElse(cond.convertToPhrase.asInstanceOf[DPIA.Phrases.Phrase[DPIA.Types.ExpType]],
      thenE.convertToPhrase.asInstanceOf[DPIA.Phrases.Phrase[DPIA.Types.PhraseType]],
      elseE.convertToPhrase.asInstanceOf[DPIA.Phrases.Phrase[DPIA.Types.PhraseType]])
  }

  override def toString: String = s"if ($cond) then ($thenE) else ($elseE)"
}

final case class UnaryOpExpr(op: Operators.Unary.Value, e: Expr)
  extends Expr
{
  assert(e.isInstanceOf[Expr])
  override lazy val t: Option[Type] = e.t

  override def convertToPhrase: DPIA.Phrases.UnaryOp = {
    DPIA.Phrases.UnaryOp(op, e.convertToPhrase.asInstanceOf[DPIA.Phrases.Phrase[DPIA.Types.ExpType]])
  }

  override def toString: String = s"$op $e"
}

final case class BinOpExpr(op: Operators.Binary.Value, lhs: Expr, rhs: Expr)
  extends Expr
{
  assert(lhs.isInstanceOf[Expr])
  assert(rhs.isInstanceOf[Expr])
  override lazy val t: Option[Type] = (lhs.t, rhs.t) match {
    case (Some(lhsT), Some(rhsT)) if lhsT == rhsT => Some(lhsT)
    case _ => None
  }

  override def convertToPhrase: DPIA.Phrases.BinOp = {
    DPIA.Phrases.BinOp(op,
      lhs.convertToPhrase.asInstanceOf[DPIA.Phrases.Phrase[DPIA.Types.ExpType]],
      rhs.convertToPhrase.asInstanceOf[DPIA.Phrases.Phrase[DPIA.Types.ExpType]])
  }

  override def toString: String = s"$lhs $op $rhs"
}

final case class LiteralExpr(d: Data)
  extends Expr
{
  override lazy val t: Option[DataType] = Some(d.dataType)

  override def convertToPhrase: DPIA.Phrases.Literal = {
    DPIA.Phrases.Literal(DPIA.Semantics.OperationalSemantics.Data(d))
  }

  override def toString: String = s"$d"
}

abstract class PrimitiveExpr extends Expr {
  def inferType(subs: Types.TypeInference.SubstitutionMap): Expr

  def visitAndRebuild(f: VisitAndRebuild.Visitor): Expr

  override def convertToPhrase: idealised.DPIA.Phrases.Phrase[idealised.DPIA.Types.ExpType]
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