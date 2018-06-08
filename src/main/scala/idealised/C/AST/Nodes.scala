package idealised.C.AST

sealed trait Node

abstract class Decl(val name: String) extends Node

case class FunDecl(override val name: String,
                   returnType: Type,
                   params: Seq[ParamDecl],
                   body: Stmt) extends Decl(name)

case class VarDecl(override val name: String, t: Type, init: Option[Expr] = None) extends Decl(name)

case class ParamDecl(override val name: String, t: Type) extends Decl(name)

case class LabelDecl(override val name: String) extends Decl(name)

case class TypedefDecl(t: Type, override val name: String) extends Decl(name)


abstract class Stmt extends Node

case class Block(body: Seq[Stmt] = Seq()) extends Stmt {
  def +(s: Stmt): Block = Block(body :+ s)
  def add(s: Stmt): Block = this + s

  def ++ (s: Seq[Stmt]): Block = Block(body ++ s)
  def add(s: Seq[Stmt]): Block = this.++(s)
}

case class Stmts(fst: Stmt, snd: Stmt) extends Stmt

case class ForLoop(init: Stmt, cond: Expr, increment: Expr, body: Stmt) extends Stmt

case class WhileLoop(cond: Expr,body: Stmt) extends Stmt

case class IfThenElse(cond: Expr, trueBody: Stmt, falseBody: Option[Stmt]) extends Stmt

case class GOTO(label: String) extends Stmt

case class Break() extends Stmt

case class Continue() extends Stmt

case class Return() extends Stmt

case class DeclStmt(decl: Decl) extends Stmt

case class Comment(string: String) extends Stmt

case class Code(string: String) extends Stmt


abstract class Expr extends Stmt

case class Assignment(lvalue: Expr, rvalue: Expr) extends Expr

case class DeclRef(name: String) extends Expr

case class FunCall(fun: DeclRef, args: Seq[Expr]) extends Expr

case class ArraySubscript(v: Expr, index: Expr) extends Expr

case class UnaryExpr(op: UnaryOperator.Value, e: Expr) extends Expr

object UnaryOperator extends Enumeration {
  val ! : UnaryOperator.Value = Value("!")
  val - : UnaryOperator.Value = Value("-")
  val * : UnaryOperator.Value = Value("*")
}

case class BinaryExpr(lhs: Expr, op: BinaryOperator.Value, rhs: Expr) extends Expr

object BinaryOperator extends Enumeration {
  val + : BinaryOperator.Value = Value("+")
  val - : BinaryOperator.Value = Value("-")
  val * : BinaryOperator.Value = Value("*")
  val / : BinaryOperator.Value = Value("/")
  val < : BinaryOperator.Value = Value("<")
  val > : BinaryOperator.Value = Value(">")
  val <= : BinaryOperator.Value = Value("<=")
  val >= : BinaryOperator.Value = Value(">=")
  val != : BinaryOperator.Value = Value("!=")
  val == : BinaryOperator.Value = Value("==")
  val && : BinaryOperator.Value = Value("&&")
  val || : BinaryOperator.Value = Value("||")
}

case class Cast(t: Type, e: Expr) extends Expr

case class Literal(code: String) extends Expr

case class ArithmeticExpr(ae: lift.arithmetic.ArithExpr) extends Expr