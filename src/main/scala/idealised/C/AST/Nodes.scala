package idealised.C.AST

import idealised.C
import idealised.C.AST.Nodes.{VisitAndGenerateStmt, VisitAndRebuild}
import lift.arithmetic.ArithExpr

/*
* This implementation follows the design from the Lift repository by Adam Harries
* The overall design of the AST is as follows: We define an "overall" AST
* trait which we use to define types that are members of the/an AST. We
* extend that with kinds of node (e.g. Attributes, Declarations, Statements,
* Expressions etc) expressed as traits, which we further extend with
* specifics, e.g. function declarations, function calls, blocks, etc.
*
* It is that final extension where the "magic" of this design lies: by
* expressing leaf nodes as traits themselves, they can be extended by using
* inheritance, and "default" implementations can easily be defined.
*
* A very basic example (for a function declaration):
*
*   Node
*    ^
*    |
*    +
*    |
* Declaration
*    ^
*    |
*    |
*    |
* FunctionT
*    ^
*    |
*    +---------------------+
*    |                     |
* GenericFunction     OpenCLFunction
*
* */

sealed trait Node {
  def visitAndRebuild(v: Nodes.VisitAndRebuild.Visitor): Node
}

abstract class Decl(val name: String) extends Node

abstract class FunDecl(override val name: String,
                       val returnType: Type,
                       val params: Seq[ParamDecl],
                       val body: Stmt) extends Decl(name)

abstract class VarDecl(override val name: String,
                       val t: Type,
                       val init: Option[Expr] = None) extends Decl(name)

abstract class ParamDecl(override val name: String, val t: Type) extends Decl(name)

abstract class LabelDecl(override val name: String) extends Decl(name)

abstract class TypedefDecl(val t: Type, override val name: String) extends Decl(name)

abstract class StructTypeDecl(override val name: String,
                              val fields: Seq[VarDecl]) extends Decl(name)


abstract class Stmt extends Node {
  def visitAndBuildStmt(v:VisitAndGenerateStmt.Visitor):Stmt
}

abstract class Block(val body: Seq[Stmt] = Seq()) extends Stmt {
  def +(s: Stmt): Block
  def add(s: Stmt): Block

  def ++ (s: Seq[Stmt]): Block
  def add(s: Seq[Stmt]): Block
}

abstract class Stmts(val fst: Stmt, val snd: Stmt) extends Stmt

abstract class ForLoop(val init: DeclStmt, val cond: Expr, val increment: Expr, val body: Block) extends Stmt

abstract class WhileLoop(val cond: Expr, val body: Stmt) extends Stmt

abstract class IfThenElse(val cond: Expr, val trueBody: Stmt, val falseBody: Option[Stmt]) extends Stmt

abstract class GOTO(val label: String) extends Stmt

abstract class Break() extends Stmt

abstract class Continue() extends Stmt

abstract class Return(val x:Option[Expr]) extends Stmt

abstract class DeclStmt(val decl: Decl) extends Stmt

abstract class Comment(val string: String) extends Stmt

abstract class Code(val string: String) extends Stmt

abstract class ExprStmt(val expr: Expr) extends Stmt


abstract class Expr extends Node {
  def visitAndBuildStmt(v:VisitAndGenerateStmt.Visitor, cont:Expr => Stmt):Stmt
}

abstract class Assignment(val lvalue: Expr, val rvalue: Expr) extends Expr

abstract class DeclRef(val name: String) extends Expr

abstract class FunCall(val fun: DeclRef, val args: Seq[Expr]) extends Expr

abstract class ArraySubscript(val array: Expr, val index: Expr) extends Expr

abstract class StructMemberAccess(val struct: Expr, val member: DeclRef) extends Expr

abstract class UnaryExpr(val op: UnaryOperator.Value, val e: Expr) extends Expr

object UnaryOperator extends Enumeration {
  val ! : UnaryOperator.Value = Value("!")
  val - : UnaryOperator.Value = Value("-")
  val * : UnaryOperator.Value = Value("*")
  val & : UnaryOperator.Value = Value("&")
}

abstract class BinaryExpr(val lhs: Expr, val op: BinaryOperator.Value, val rhs: Expr) extends Expr

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
  val ^ : BinaryOperator.Value = Value("^")
  val % : BinaryOperator.Value = Value("%")
}

abstract class TernaryExpr(val cond: Expr, val thenE: Expr, val elseE: Expr) extends Expr

abstract class Cast(val t: Type, val e: Expr) extends Expr

abstract class Literal(val code: String) extends Expr

abstract class ArrayLiteral(val t: ArrayType, val inits: Seq[Expr]) extends Expr

abstract class RecordLiteral(val fst: Expr, val snd: Expr) extends Expr

abstract class ArithmeticExpr(val ae: lift.arithmetic.ArithExpr) extends Expr

// visitor

object Nodes {

  object VisitAndRebuild {
    class Visitor {
      def pre(n: Node): Result = Continue(n, this)
      def post(n: Node): Node = n
      def apply(t: Type): Type = t

      sealed trait Result
      case class Stop(n: Node) extends Result
      case class Continue(n: Node, v: Visitor) extends Result
    }

    def apply[N <: Node](n: N, v: Visitor): N = {
      v.pre(n) match {
        case res: v.Stop => res.n.asInstanceOf[N]
        case res: v.Continue => v.post(res.n.visitAndRebuild(res.v)).asInstanceOf[N]
      }
    }
  }

  object VisitAndGenerateStmt {
    class Visitor {
      def onExpr(e:Expr, cont:Expr => Stmt):Stmt = cont(e)
      def onStmt(stmt: Stmt):Stmt = stmt
    }

    def apply(e:Expr, v:Visitor, cont:Expr => Stmt):Stmt = {
      e.visitAndBuildStmt(v, e => v.onExpr(e, cont))
    }
    def apply(s:Stmt, v:Visitor):Stmt = v.onStmt(s.visitAndBuildStmt(v))
  }
}


// default constructors and extractors

object FunDecl {
  def apply(name: String, returnType: Type, params: Seq[ParamDecl], body: Stmt): FunDecl = DefaultImplementations.FunDecl(name, returnType, params, body)
  def unapply(arg: FunDecl): Option[(String, Type, Seq[ParamDecl], Stmt)] = Some((arg.name, arg.returnType, arg.params, arg.body))
}

object VarDecl {
  def apply(name: String, t: Type, init: Option[Expr] = None): VarDecl = DefaultImplementations.VarDecl(name, t, init)
  def unapply(arg: VarDecl): Option[(String, Type, Option[Expr])] = Some((arg.name, arg.t, arg.init))
}

object ParamDecl {
  def apply(name: String, t: Type): ParamDecl = DefaultImplementations.ParamDecl(name, t)
  def unapply(arg: ParamDecl): Option[(String, Type)] = Some((arg.name, arg.t))
}

object LabelDecl {
  def apply(name: String): LabelDecl = DefaultImplementations.LabelDecl(name)
  def unapply(arg: LabelDecl): Option[String] = Some(arg.name)
}

object TypedefDecl {
  def apply(t: Type, name: String): TypedefDecl  = DefaultImplementations.TypedefDecl(t, name)
  def unapply(arg: TypedefDecl): Option[(Type, String)] = Some((arg.t, arg.name))
}

object StructTypeDecl {
  def apply(name: String, fields: Seq[VarDecl]): StructTypeDecl = DefaultImplementations.StructTypeDecl(name, fields)
  def unapply(arg: StructTypeDecl): Option[(String, Seq[VarDecl])] = Some((arg.name, arg.fields))
}

object Block {
  def apply(body: Seq[Stmt] = Seq()): Block = DefaultImplementations.Block(body)
  def unapply(arg: Block): Option[Seq[Stmt]] = Some(arg.body)
}

object Stmts {
  def apply(fst: Stmt, snd: Stmt): Stmts = DefaultImplementations.Stmts(fst, snd)
  def unapply(arg: Stmts): Option[(Stmt, Stmt)] = Some((arg.fst, arg.snd))
}

object ForLoop {
  def apply(init: DeclStmt, cond: Expr, increment: Expr, body: Block): ForLoop = DefaultImplementations.ForLoop(init, cond, increment, body)
  def unapply(arg: ForLoop): Option[(Stmt, Expr, Expr, Stmt)] = Some((arg.init, arg.cond, arg.increment, arg.body))
}

object WhileLoop {
  def apply(cond: Expr, body: Stmt): WhileLoop = DefaultImplementations.WhileLoop(cond, body)
  def unapply(arg: WhileLoop): Option[(Expr, Stmt)] = Some((arg.cond, arg.body))
}

object IfThenElse {
  def apply(cond: Expr, trueBody: Stmt, falseBody: Option[Stmt]): IfThenElse = DefaultImplementations.IfThenElse(cond, trueBody, falseBody)
  def unapply(arg: IfThenElse): Option[(Expr, Stmt, Option[Stmt])] = Some((arg.cond, arg.trueBody, arg.falseBody))
}

object GOTO {
  def apply(label: String): GOTO = DefaultImplementations.GOTO(label)
  def unapply(arg: GOTO): Option[String] = Some(arg.label)
}

object Break {
  def apply(): Break = DefaultImplementations.Break()
}

object Continue {
  def apply(): Continue = DefaultImplementations.Continue()
}

object Return {
  def apply(): Return = DefaultImplementations.Return(None)
  def apply(x:Expr): Return = DefaultImplementations.Return(Some(x))
}

object DeclStmt {
  def apply(decl: Decl): DeclStmt = DefaultImplementations.DeclStmt(decl)
  def unapply(arg: DeclStmt): Option[Decl] = Some(arg.decl)
}

object Comment {
  def apply(string: String): Comment = DefaultImplementations.Comment(string)
  def unapply(arg: Comment): Option[String] = Some(arg.string)
}

object Code {
  def apply(string: String): Code = DefaultImplementations.Code(string)
  def unapply(arg: Code): Option[String] = Some(arg.string)
}

object ExprStmt {
  def apply(expr: Expr): ExprStmt = DefaultImplementations.ExprStmt(expr)
  def unapply(arg: ExprStmt): Option[Expr] = Some(arg.expr)
}

object Assignment {
  def apply(lvalue: Expr, rvalue: Expr): Assignment = DefaultImplementations.Assignment(lvalue, rvalue)
  def unapply(arg: Assignment): Option[(Expr, Expr)] = Some((arg.lvalue, arg.rvalue))
}

object DeclRef {
  def apply(name: String): DeclRef = DefaultImplementations.DeclRef(name)
  def unapply(arg: DeclRef): Option[String] = Some(arg.name)
}

object FunCall {
  def apply(fun: DeclRef, args: Seq[Expr]): FunCall = DefaultImplementations.FunCall(fun, args)
  def unapply(arg: FunCall): Option[(DeclRef, Seq[Expr])] = Some((arg.fun, arg.args))
}

object ArraySubscript {
  def apply(array: Expr, index: Expr): ArraySubscript = DefaultImplementations.ArraySubscript(array, index)
  def unapply(arg: ArraySubscript): Option[(Expr, Expr)] = Some((arg.array, arg.index))
}

object StructMemberAccess {
  def apply(struct: Expr, member: DeclRef): StructMemberAccess = DefaultImplementations.StructMemberAccess(struct, member)
  def unapply(arg: StructMemberAccess): Option[(Expr, DeclRef)] = Some((arg.struct, arg.member))
}

object UnaryExpr {
  def apply(op: UnaryOperator.Value, e: Expr): UnaryExpr = DefaultImplementations.UnaryExpr(op, e)
  def unapply(arg: UnaryExpr): Option[(UnaryOperator.Value, Expr)] = Some((arg.op, arg.e))
}

object BinaryExpr {
  def apply(lhs: Expr, op: BinaryOperator.Value, rhs: Expr): BinaryExpr = DefaultImplementations.BinaryExpr(lhs, op, rhs)
  def unapply(arg: BinaryExpr): Option[(Expr, BinaryOperator.Value, Expr)] = Some((arg.lhs, arg.op, arg.rhs))
}

object TernaryExpr {
  def apply(cond: Expr, thenE: Expr, elseE: Expr): TernaryExpr = DefaultImplementations.TernaryExpr(cond, thenE, elseE)
  def unapply(arg: TernaryExpr): Option[(Expr, Expr, Expr)] = Some((arg.cond, arg.thenE, arg.elseE))
}

object Cast {
  def apply(t: Type, e: Expr): Cast = DefaultImplementations.Cast(t, e)
  def unapply(arg: Cast): Option[(Type, Expr)] = Some((arg.t, arg.e))
}

object Literal {
  def apply(code: String): Literal = DefaultImplementations.Literal(code)
  def unapply(arg: Literal): Option[String] = Some(arg.code)
}

object ArrayLiteral {
  def apply(t: ArrayType, inits: Seq[Expr]): ArrayLiteral = DefaultImplementations.ArrayLiteral(t, inits)
  def unapply(arg: ArrayLiteral): Option[(ArrayType, Seq[Expr])] = Some((arg.t, arg.inits))
}

object RecordLiteral {
  def apply(fst: Expr, snd: Expr): RecordLiteral = DefaultImplementations.RecordLiteral(fst, snd)
  def unapply(arg: RecordLiteral): Option[(Expr, Expr)] = Some((arg.fst, arg.snd))
}

object ArithmeticExpr {
  def apply(ae: lift.arithmetic.ArithExpr): ArithmeticExpr = DefaultImplementations.ArithmeticExpr(ae)
  def unapply(arg: ArithmeticExpr): Option[ArithExpr] = Some(arg.ae)
}

// default implementations

object DefaultImplementations {

  case class FunDecl(override val name: String,
                     override val returnType: Type,
                     override val params: Seq[C.AST.ParamDecl],
                     override val body: Stmt)
    extends C.AST.FunDecl(name, returnType, params, body)
  {
    override def visitAndRebuild(v: VisitAndRebuild.Visitor): FunDecl =
      FunDecl(name, v(returnType), params.map(VisitAndRebuild(_, v)), VisitAndRebuild(body, v))
  }

  case class VarDecl(override val name: String,
                     override val t: Type,
                     override val init: Option[Expr] = None)
    extends C.AST.VarDecl(name, t, init)
  {
    override def visitAndRebuild(v: VisitAndRebuild.Visitor): VarDecl = VarDecl(name, v(t), init.map(VisitAndRebuild(_, v)))
  }

  case class ParamDecl(override val name: String,
                       override val t: Type)
    extends C.AST.ParamDecl(name, t)
  {
    override def visitAndRebuild(v: VisitAndRebuild.Visitor): ParamDecl = ParamDecl(name, v(t))
  }

  case class LabelDecl(override val name: String)
    extends C.AST.LabelDecl(name)
  {
    override def visitAndRebuild(v: VisitAndRebuild.Visitor): LabelDecl.this.type = this
  }

  case class TypedefDecl(override val t: Type,
                         override val name: String)
    extends C.AST.TypedefDecl(t, name)
  {
    override def visitAndRebuild(v: VisitAndRebuild.Visitor): TypedefDecl = TypedefDecl(v(t), name)
  }

  case class StructTypeDecl(override val name: String,
                            override val fields: Seq[C.AST.VarDecl])
    extends C.AST.StructTypeDecl(name, fields)
  {
    override def visitAndRebuild(v: VisitAndRebuild.Visitor): StructTypeDecl =
      StructTypeDecl(name, fields.map(VisitAndRebuild(_, v)))
  }

  case class Block(override val body: Seq[Stmt] = Seq())
    extends C.AST.Block(body)
  {
    def +(s: Stmt): Block = Block(body :+ s)
    def add(s: Stmt): Block = this + s

    def ++ (s: Seq[Stmt]): Block = Block(body ++ s)
    def add(s: Seq[Stmt]): Block = this.++(s)

    override def visitAndRebuild(v: VisitAndRebuild.Visitor): Block = Block(body.map(VisitAndRebuild(_, v)))

    override def visitAndBuildStmt(v: VisitAndGenerateStmt.Visitor): Block = {
      //We cannot simply map, as later blocks may be dependent on the contents previous blocks.
      //Instead, we must merge everything into one resulting block
      body.foldLeft(Block(Seq()))((currentBlock, stmt) => {
        val rebuilt = VisitAndGenerateStmt(stmt, v)
        rebuilt match {
          case Block(stmts) => Block(currentBlock.body ++ stmts)
          case other => Block(currentBlock.body :+ other)
        }
      })
    }
  }

  case class Stmts(override val fst: Stmt, override val snd: Stmt)
    extends C.AST.Stmts(fst, snd)
  {
    override def visitAndRebuild(v: VisitAndRebuild.Visitor): Stmts = Stmts(VisitAndRebuild(fst, v), VisitAndRebuild(snd, v))

    override def visitAndBuildStmt(v: VisitAndGenerateStmt.Visitor): Stmt = Stmts(VisitAndGenerateStmt(fst, v), VisitAndGenerateStmt(snd, v))
  }

  case class ForLoop(override val init: C.AST.DeclStmt,
                     override val cond: Expr,
                     override val increment: Expr,
                     override val body: C.AST.Block)
    extends C.AST.ForLoop(init, cond, increment, body)
  {
    override def visitAndRebuild(v: VisitAndRebuild.Visitor): ForLoop =
      ForLoop(VisitAndRebuild(init, v), VisitAndRebuild(cond, v), VisitAndRebuild(increment, v), VisitAndRebuild(body, v))

    override def visitAndBuildStmt(v: VisitAndGenerateStmt.Visitor): Stmt =
      VisitAndGenerateStmt(cond, v, condE => VisitAndGenerateStmt(increment, v, incrementE => ForLoop(
        VisitAndGenerateStmt(init, v).asInstanceOf[C.AST.DeclStmt],
        condE,
        incrementE,
        VisitAndGenerateStmt(body, v).asInstanceOf[Block]
      )))
  }

  case class WhileLoop(override val cond: Expr, override val body: Stmt)
    extends C.AST.WhileLoop(cond, body)
  {
    override def visitAndRebuild(v: VisitAndRebuild.Visitor):
      WhileLoop = WhileLoop(VisitAndRebuild(cond, v), VisitAndRebuild(body, v))

    override def visitAndBuildStmt(v: VisitAndGenerateStmt.Visitor): Stmt =
      VisitAndGenerateStmt(cond, v, condE => WhileLoop(condE, VisitAndGenerateStmt(body, v)))
  }

  case class IfThenElse(override val cond: Expr,
                        override val trueBody: Stmt,
                        override val falseBody: Option[Stmt])
    extends C.AST.IfThenElse(cond, trueBody, falseBody)
  {
    override def visitAndRebuild(v: VisitAndRebuild.Visitor): IfThenElse =
      IfThenElse(VisitAndRebuild(cond, v), VisitAndRebuild(trueBody, v), falseBody.map(VisitAndRebuild(_, v)))

    override def visitAndBuildStmt(v: VisitAndGenerateStmt.Visitor): Stmt =
      VisitAndGenerateStmt(cond, v, condE => IfThenElse(condE, VisitAndGenerateStmt(trueBody, v), falseBody.map(VisitAndGenerateStmt(_, v))))
  }

  case class GOTO(override val label: String)
    extends C.AST.GOTO(label)
  {
    override def visitAndRebuild(v: VisitAndRebuild.Visitor): GOTO.this.type = this

    override def visitAndBuildStmt(v: VisitAndGenerateStmt.Visitor): Stmt = this
  }

  case class Break() extends C.AST.Break {
    override def visitAndRebuild(v: VisitAndRebuild.Visitor): Break.this.type = this

    override def visitAndBuildStmt(v: VisitAndGenerateStmt.Visitor): Stmt = this
  }

  case class Continue() extends C.AST.Continue {
    override def visitAndRebuild(v: VisitAndRebuild.Visitor): Continue.this.type = this

    override def visitAndBuildStmt(v: VisitAndGenerateStmt.Visitor): Stmt = this
  }

  case class Return(override val x:Option[Expr]) extends C.AST.Return(x) {
    override def visitAndRebuild(v: VisitAndRebuild.Visitor): Return = Return(x.map(VisitAndRebuild(_, v)))

    override def visitAndBuildStmt(v: VisitAndGenerateStmt.Visitor): Stmt = this
  }

  case class DeclStmt(override val decl: Decl) extends C.AST.DeclStmt(decl) {
    override def visitAndRebuild(v: VisitAndRebuild.Visitor): DeclStmt = DeclStmt(VisitAndRebuild(decl, v))

    override def visitAndBuildStmt(v: VisitAndGenerateStmt.Visitor): Stmt = decl match {
      case VarDecl(name, t, Some(init)) => VisitAndGenerateStmt(init, v, initE => DeclStmt(VarDecl(name, t, Some(initE))))
      case _ => this
    }
  }

  case class Comment(override val string: String) extends C.AST.Comment(string) {
    override def visitAndRebuild(v: VisitAndRebuild.Visitor): Comment.this.type = this

    override def visitAndBuildStmt(v: VisitAndGenerateStmt.Visitor): Stmt = this
  }

  case class Code(override val string: String) extends C.AST.Code(string) {
    override def visitAndRebuild(v: VisitAndRebuild.Visitor): Code.this.type = this

    override def visitAndBuildStmt(v: VisitAndGenerateStmt.Visitor): Stmt = this
  }

  case class ExprStmt(override val expr: Expr) extends C.AST.ExprStmt(expr) {
    override def visitAndRebuild(v: VisitAndRebuild.Visitor): ExprStmt = ExprStmt(VisitAndRebuild(expr, v))

    override def visitAndBuildStmt(v: VisitAndGenerateStmt.Visitor): Stmt =
      VisitAndGenerateStmt(expr, v, exprE =>
        ExprStmt(exprE))
  }


  case class Assignment(override val lvalue: Expr, override val rvalue: Expr)
    extends C.AST.Assignment(lvalue, rvalue) {
    override def visitAndRebuild(v: VisitAndRebuild.Visitor): Assignment =
      Assignment(VisitAndRebuild(lvalue, v), VisitAndRebuild(rvalue, v))

    override def visitAndBuildStmt(v: VisitAndGenerateStmt.Visitor, cont: Expr => Stmt): Stmt =
      VisitAndGenerateStmt(lvalue, v, lvalueE =>
        VisitAndGenerateStmt(rvalue, v, rvalueE =>
          cont(Assignment(lvalueE, rvalueE))))
  }

  case class DeclRef(override val name: String) extends C.AST.DeclRef(name) {
    override def visitAndRebuild(v: VisitAndRebuild.Visitor): DeclRef.this.type = this

    override def visitAndBuildStmt(v: VisitAndGenerateStmt.Visitor, cont: Expr => Stmt): Stmt = cont(this)
  }

  case class FunCall(override val fun: C.AST.DeclRef, override val args: Seq[Expr])
    extends C.AST.FunCall(fun, args)
  {
    override def visitAndRebuild(v: VisitAndRebuild.Visitor): FunCall =
      FunCall(VisitAndRebuild(fun, v), args.map(VisitAndRebuild(_, v)))

    override def visitAndBuildStmt(v: VisitAndGenerateStmt.Visitor, cont: Expr => Stmt): Stmt = {
      def rec(worklist:Iterable[Expr], accum:Seq[Expr]):Stmt = worklist.headOption match {
        case None => cont(FunCall(fun, accum))
        case Some(head) => VisitAndGenerateStmt(head, v, headE => rec(worklist.tail, accum :+ headE))
      }

      rec(args, Seq())
    }
  }

  case class ArraySubscript(override val array: Expr, override val index: Expr)
    extends C.AST.ArraySubscript(array, index)
  {
    override def visitAndRebuild(v: VisitAndRebuild.Visitor): ArraySubscript =
      ArraySubscript(VisitAndRebuild(array, v), VisitAndRebuild(index, v))

    override def visitAndBuildStmt(v: VisitAndGenerateStmt.Visitor, cont: Expr => Stmt): Stmt =
      VisitAndGenerateStmt(array, v, arrayE => VisitAndGenerateStmt(index, v, indexE => cont(ArraySubscript(arrayE, indexE))))
  }

  case class StructMemberAccess(override val struct: Expr, override val member: C.AST.DeclRef)
    extends C.AST.StructMemberAccess(struct, member)
  {
    override def visitAndRebuild(v: VisitAndRebuild.Visitor): StructMemberAccess =
      StructMemberAccess(VisitAndRebuild(struct, v), VisitAndRebuild(member, v))

    override def visitAndBuildStmt(v: VisitAndGenerateStmt.Visitor, cont: Expr => Stmt): Stmt =
      VisitAndGenerateStmt(struct, v, structE => cont(StructMemberAccess(structE, member)))
  }

  case class UnaryExpr(override val op: UnaryOperator.Value, override val e: Expr)
    extends C.AST.UnaryExpr(op, e)
  {
    override def visitAndRebuild(v: VisitAndRebuild.Visitor): UnaryExpr = UnaryExpr(op, VisitAndRebuild(e, v))

    override def visitAndBuildStmt(v: VisitAndGenerateStmt.Visitor, cont: Expr => Stmt): Stmt =
      VisitAndGenerateStmt(e, v, eE => cont(UnaryExpr(op, eE)))
  }

  case class BinaryExpr(override val lhs: Expr, override val op: BinaryOperator.Value, override val rhs: Expr)
    extends C.AST.BinaryExpr(lhs, op, rhs)
  {
    override def visitAndRebuild(v: VisitAndRebuild.Visitor): BinaryExpr =
      BinaryExpr(VisitAndRebuild(lhs, v), op, VisitAndRebuild(rhs, v))

    override def visitAndBuildStmt(v: VisitAndGenerateStmt.Visitor, cont: Expr => Stmt): Stmt =
      VisitAndGenerateStmt(lhs, v, lhsE =>
        VisitAndGenerateStmt(rhs, v, rhsE =>
          cont(BinaryExpr(lhsE, op, rhsE))))
  }

  case class TernaryExpr(override val cond: Expr, override val thenE: Expr, override val elseE: Expr)
    extends C.AST.TernaryExpr(cond, thenE, elseE)
  {
    override def visitAndRebuild(v: VisitAndRebuild.Visitor): TernaryExpr =
      TernaryExpr(VisitAndRebuild(cond, v), VisitAndRebuild(thenE, v), VisitAndRebuild(elseE, v))

    override def visitAndBuildStmt(v: VisitAndGenerateStmt.Visitor, cont: Expr => Stmt): Stmt =
      VisitAndGenerateStmt(cond, v, condE =>
        VisitAndGenerateStmt(thenE, v, thenEE =>
          VisitAndGenerateStmt(elseE, v, elseEE =>
            cont(TernaryExpr(condE, thenEE, elseEE)))))
  }

  case class Cast(override val t: Type, override val e: Expr) extends C.AST.Cast(t, e) {
    override def visitAndRebuild(v: VisitAndRebuild.Visitor): Cast = Cast(v(t), VisitAndRebuild(e, v))

    override def visitAndBuildStmt(v: VisitAndGenerateStmt.Visitor, cont: Expr => Stmt): Stmt =
      VisitAndGenerateStmt(e, v, eE => cont(Cast(t, eE)))
  }

  case class Literal(override val code: String) extends C.AST.Literal(code) {
    override def visitAndRebuild(v: VisitAndRebuild.Visitor): Literal.this.type = this

    override def visitAndBuildStmt(v: VisitAndGenerateStmt.Visitor, cont: Expr => Stmt): Stmt = cont(this)
  }

  case class ArrayLiteral(override val t: ArrayType, override val inits: Seq[Expr]) extends C.AST.ArrayLiteral(t, inits) {
    override def visitAndRebuild(v: VisitAndRebuild.Visitor): ArrayLiteral =
      ArrayLiteral(v(t).asInstanceOf[ArrayType], inits.map(VisitAndRebuild(_, v)))

    override def visitAndBuildStmt(v: VisitAndGenerateStmt.Visitor, cont: Expr => Stmt): Stmt = {
     def rec(toProcess:Seq[Expr], accum:Seq[Expr]):Stmt = {
       toProcess.headOption match {
         case None => cont(ArrayLiteral(t, accum))
         case Some(expr) => VisitAndGenerateStmt(expr, v, exprE => rec(toProcess.tail, accum :+ exprE))
       }
     }

      rec(inits, Seq())
    }
  }

  case class RecordLiteral(override val fst: Expr, override val snd: Expr) extends C.AST.RecordLiteral(fst, snd) {
    override def visitAndRebuild(v: VisitAndRebuild.Visitor): RecordLiteral =
      RecordLiteral(VisitAndRebuild(fst, v), VisitAndRebuild(snd, v))

    override def visitAndBuildStmt(v: VisitAndGenerateStmt.Visitor, cont: Expr => Stmt): Stmt =
      VisitAndGenerateStmt(fst, v, fstE => VisitAndGenerateStmt(snd, v, sndE => cont(RecordLiteral(fstE, sndE))))
  }

  case class ArithmeticExpr(override val ae: lift.arithmetic.ArithExpr) extends C.AST.ArithmeticExpr(ae) {
    override def visitAndRebuild(v: VisitAndRebuild.Visitor): ArithmeticExpr.this.type = this

    override def visitAndBuildStmt(v: VisitAndGenerateStmt.Visitor, cont: Expr => Stmt): Stmt = cont(this)
  }
}