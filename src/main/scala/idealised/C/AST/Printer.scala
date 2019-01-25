package idealised.C.AST

import lift.arithmetic._

trait Printer {
  def printNode(n: Node): String

  def printDecl(d: Decl): Unit

  def printExpr(e: Expr): Unit

  def printStmt(s: Stmt): Unit

  def typeName(t: Type): String

  def toString(e: ArithExpr) : String

  protected val sb: StringBuilder = new StringBuilder
  protected var indent: Int = 0

  protected def print(s: String): Unit = {
    sb ++= s
  }

  protected  def println(s: String): Unit = {
    sb ++= s + "\n" + tab()
  }

  protected  val tabSize = 2

  protected  def tab(): String = {
    lazy val whiteSpace: String = " " * tabSize
    whiteSpace * indent
  }

  protected  def moveCursorBack(size: Int): Unit = {
    for (_ <- 1 to size) {
      if (sb.last.isWhitespace) { sb.deleteCharAt(sb.size - 1) }
    }
  }
}

object Printer {
  def apply(n: Node): String = (new CPrinter).printNode(n)
}

class CPrinter extends Printer {

  override def printNode(n: Node): String = {

    n match {
      case d: Decl => printDecl(d)
      case e: Expr => printExpr(e)
      case s: Stmt => printStmt(s)
    }

    sb.toString()
  }

  override def printDecl(d: Decl): Unit = d match {
    case f: FunDecl => printFunDecl(f)
    case v: VarDecl => printVarDecl(v)
    case p: ParamDecl => printParamDecl(p)
    case l: LabelDecl => printLabelDecl(l)
    case t: TypedefDecl => printTypedefDecl(t)
  }

  override def printExpr(e: Expr): Unit = e match {
    case a: Assignment => printAssignment(a)
    case d: DeclRef => printDeclRef(d)
    case f: FunCall => printFunCall(f)
    case s: StructMemberAccess => printStructMemberAccess(s)
    case a: ArraySubscript => printArraySubscript(a)
    case u: UnaryExpr => printUnaryExpr(u)
    case b: BinaryExpr => printBinaryExpr(b)
    case t: TernaryExpr => printTernaryExpr(t)
    case c: Cast => printCast(c)
    case l: Literal => printLiteral(l)
    case a: ArithmeticExpr => printArithmeticExpr(a)
  }

  override def printStmt(s: Stmt): Unit = s match {
    case s: Stmts => printStmts(s)
    case b: Block => printBlock(b)
    case f: ForLoop => printForLoop(f)
    case w: WhileLoop => printWhileLoop(w)
    case i: IfThenElse => printIfThenElse(i)
    case g: GOTO => printGOTO(g)
    case b: Break => printBreak(b)
    case c: Continue => printContinue(c)
    case r: Return => printReturn(r)
    case d: DeclStmt => printDeclStmt(d)
    case c: Comment => printComment(c)
    case c: Code => printCode(c)
    case e: Expr =>
      printExpr(e)
      print(";")
  }

  // Decls
  private def printFunDecl(f: FunDecl): Unit = {
    print(typeName(f.returnType))
    print(s" ${f.name}(")
    f.params.foreach(p => {
      printDecl(p)
      if (!p.eq(f.params.last)) print(", ")
    })
    print(")")

    printStmt(f.body)
  }

  private def printVarDecl(v: VarDecl): Unit = {
    if (v.t.const) print("const ")
    v.t match {
      case b: BasicType => print(s"${b.name} ${v.name}")
      case s: StructType => print(s"struct ${s.name} ${v.name}")
      case u: UnionType => ???
      case a: ArrayType =>
        // float name[s];
        print(s"${a.getBaseType} ${v.name}[${ a.getSizes match {
          case None => ""
          case Some(s) => s
        } }]")
      case p: PointerType => print(s"${p.valueType}* ${v.name}")
    }
    v.init match {
      case None =>
      case Some(init) =>
        print(" = ")
        printExpr(init)
    }
  }

  private def printParamDecl(p: ParamDecl): Unit = {
    if (p.t.const) print("const ")
    p.t match {
      case b: BasicType => print(s"${b.name} ${p.name}")
      case s: StructType => print(s"struct ${s.name} ${p.name}")
      case u: UnionType => ???
      case a: ArrayType =>
        print(s"${a.getBaseType} ${p.name}[${ a.getSizes match {
          case None => ""
          case Some(s) => s
        } }]")
      case pt: PointerType => print(s"${pt.valueType}* ${p.name}")
    }
  }

  private def printLabelDecl(l: LabelDecl): Unit = {
    println(l.name + ": ;")
  }

  private def printTypedefDecl(t: TypedefDecl): Unit = ???

  // Smts
  private def printStmts(s: Stmts): Unit = {
    printStmt(s.fst)
    println("")
    printStmt(s.snd)
  }

  private def printBlock(b: Block): Unit = {
    indent += 1
    println("{")
    b.body.foreach( (s: Stmt) => {
      printStmt(s)
      println("")
    })
    indent -= 1
    moveCursorBack(tabSize)
    println("}")
  }

  private def printForLoop(f: ForLoop): Unit = {
    print("for (")
    printDeclStmt(f.init)
    printExpr(f.cond)
    print(";")
    printExpr(f.increment)
    print(") ")
    printBlock(f.body)
  }

  private def printWhileLoop(w: WhileLoop): Unit = {
    print("while (")
    printExpr(w.cond)
    print(") ")
    printStmt(w.body)
  }

  private def printIfThenElse(i: IfThenElse): Unit = {
    print("if (")
    printExpr(i.cond)
    print(") ")
    printStmt(i.trueBody)

    i.falseBody match {
      case Some(falseBody) =>
        print(" else ")
        printStmt(falseBody)
      case None =>
    }
  }

  private def printGOTO(g: GOTO): Unit = {
    println("goto " + g.label + ";")
  }

  private def printBreak(b: Break): Unit = {
    println("break;")
  }

  private def printContinue(c: Continue): Unit = {
    println("continue;")
  }

  private def printReturn(r: Return): Unit = {
    println("return;")
  }

  private def printDeclStmt(d: DeclStmt): Unit = {
    printDecl(d.decl)
    print(";")
  }

  private def printComment(c: Comment): Unit = {
    print(s"/* ${c.string} */")
  }

  private def printCode(c: Code): Unit = {
    print(c.string)
  }

  // Exprs
  private def printAssignment(a: Assignment): Unit = {
    printExpr(a.lvalue)
    print(" = ")
    printExpr(a.rvalue)
  }

  private def printDeclRef(d: DeclRef): Unit = {
    print(d.name)
  }

  private def printFunCall(f: FunCall): Unit = {
    printDeclRef(f.fun)
    print("(")
    f.args.foreach(a => {
      printExpr(a)
      if (!a.eq(f.args.last)) print(", ")
    })
    print(")")
  }

  private def printArraySubscript(a: ArraySubscript): Unit = {
    printExpr(a.array)
    print("[")
    printExpr(a.index)
    print("]")
  }

  private def printStructMemberAccess(s: StructMemberAccess): Unit = {
    printExpr(s.struct)
    print(".")
    printDeclRef(s.member)
  }

  private def printUnaryExpr(u: UnaryExpr): Unit = {
    print("(")
    print(u.op.toString)
    print("(")
    printExpr(u.e)
    print("))")
  }

  private def printBinaryExpr(b: BinaryExpr): Unit = {
    print("(")
    printExpr(b.lhs)
    print(" ")
    print(b.op.toString)
    print(" ")
    printExpr(b.rhs)
    print(")")
  }

  private def printTernaryExpr(t: TernaryExpr): Unit = {
    print("(")
    printExpr(t.cond)
    print(") ? (")
    printExpr(t.thenE)
    print(") : (")
    printExpr(t.elseE)
    print(")")
  }

  private def printCast(c: Cast): Unit = {
    print("(")
    print(s"(${c.t})")
    printExpr(c.e)
    print(")")
  }

  private def printLiteral(l: Literal): Unit = {
    print(l.code)
  }

  private def printArithmeticExpr(a: ArithmeticExpr): Unit = {
    print(toString(a.ae))
  }

  // Types
  def typeName(t: Type): String = t.toString

  def toString(e: ArithExpr) : String = {
    e match {
      case Cst(c) => c.toString
      case Pow(b, ex) =>
        ex match {
          case Cst(2) => s"(${toString(b)} * ${toString(b)})"
          case _ => "(int)pow((float)" + toString (b) + ", " + toString (ex) + ")"
        }
      case Log(b, x) => "(int)log"+b+"((float)"+toString(x)+")"
      case Prod(es) => "(" + es.foldLeft("1")( (s: String, e: ArithExpr) => {
        s + (e match {
          case Pow(b, Cst(-1)) => " / (" + toString(b) + ")"
          case _ => " * " + toString(e)
        })
      } ).drop(4) + ")" // drop(4) removes the initial "1 * "
      case Sum(es) => "(" + es.map(toString).reduce( _ + " + " + _  ) + ")"
      case Mod(a,n) => "(" + toString(a) + " % " + toString(n) + ")"
      case v: Var => v.toString
      case IntDiv(n, d) => "(" + toString(n) + " / " + toString(d) + ")"
      case lu: Lookup => "lookup" + lu.id + "(" + toString(lu.index) + ")"
      case i: lift.arithmetic.IfThenElse =>
        s"( (${toString(i.test.lhs)} ${i.test.op} ${toString(i.test.rhs)}) ? " +
          s"${toString(i.t)} : ${toString(i.e)} )"
      case aeFun:ArithExprFunction => aeFun.name
      case node =>
        println("Cannot print:")
        println(node.toString)
        ???
    }
  }


}
