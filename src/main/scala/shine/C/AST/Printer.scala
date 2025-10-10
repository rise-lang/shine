package shine.C.AST

import arithexpr.arithmetic._
import shine.DPIA.NatFunCall

trait Printer {
  def printNode(n: Node): String

  def printDecl(d: Decl): Unit

  def printExpr(e: Expr, parenthesize: Boolean): Unit

  def printStmt(s: Stmt): Unit

  def typeName(t: Type): String

  def printArithExpr(e: ArithExpr, parenthesize: Boolean) : String

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
  def declFun(f: FunDecl): String =
    (new CPrinter).declareFunSig(f)
}

class CPrinter extends Printer {

  override def printNode(n: Node): String = {

    n match {
      case d: Decl => printDecl(d)
      case e: Expr => printExpr(e, parenthesize = false)
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
    case s: StructTypeDecl => printStructTypeDecl(s)
  }

  def declareFunSig(f: FunDecl): String = {
    printFunSig(f)
    print(";")
    sb.toString()
  }

  override def printExpr(e: Expr, parenthesize: Boolean): Unit = e match {
    case a: Assignment =>
      printMaybe(parenthesize)(
        printAssignment(a)
      )
    case d: DeclRef => printDeclRef(d)
    case f: FunCall => printFunCall(f)
    case s: StructMemberAccess => printStructMemberAccess(s)
    case a: ArraySubscript => printArraySubscript(a)
    case u: UnaryExpr =>
      printMaybe(parenthesize)(
        printUnaryExpr(u)
      )
    case b: BinaryExpr =>
      printMaybe(parenthesize)(
        printBinaryExpr(b)
      )
    case t: TernaryExpr =>
      printMaybe(parenthesize)(
        printTernaryExpr(t)
      )
    case c: Cast =>
      printMaybe(parenthesize)(
        printCast(c)
      )
    case l: Literal => printLiteral(l)
    case al: ArrayLiteral => printArrayLiteral(al)
    case rl: RecordLiteral => printRecordLiteral(rl)
    case a: ArithmeticExpr => printArithmeticExpr(a, parenthesize)
  }

  override def printStmt(s: Stmt): Unit = s match {
    case s: Stmts => printStmts(s)
    case b: Block => printBlock(b)
    case f: ForLoop => printForLoop(f)
    case w: WhileLoop => printWhileLoop(w)
    case i: shine.C.AST.IfThenElse => printIfThenElse(i)
    case g: GOTO => printGOTO(g)
    case b: Break => printBreak(b)
    case c: Continue => printContinue(c)
    case r: Return => printReturn(r)
    case d: DeclStmt => printDeclStmt(d)
    case c: Comment => printComment(c)
    case c: Code => printCode(c)
    case e: ExprStmt =>
      printExpr(e.expr, parenthesize = false)
      print(";")
  }

  def printFunSig(f: FunDecl): Unit = {
    print(typeName(f.returnType))
    print(s" ${f.name}(")
    f.params.foreach(p => {
      printDecl(p)
      if (!p.eq(f.params.last)) print(", ")
    })
    print(")")
  }

  // Decls
  private def printFunDecl(f: FunDecl): Unit = {
    printFunSig(f)
    printStmt(f.body)
  }

  private def printVarDecl(v: VarDecl): Unit = {
    if (v.t.const) print("const ")
    v.t match {
      case b: BasicType => print(s"${b.name} ${v.name}")
      case o: OpaqueType => print(s"${o.name} ${v.name}")
      case s: StructType => print(s"struct ${s.name} ${v.name}")
      case _: UnionType | _:FragmentType => ???
      case a: ArrayType =>
        // float name[s];
        print(s"${typeName(a.getBaseType)} ${v.name}[${ a.getSizes match {
          case None => ""
          case Some(s) => printArithExpr(s, parenthesize = false)
        } }]")
      case p: PointerType => print(s"${typeName(p.valueType)}* ${v.name}")
    }
    v.init match {
      case None =>
      case Some(init) =>
        print(" = ")
        printExpr(init, parenthesize = false)
    }
  }

  private def printParamDecl(p: ParamDecl): Unit = {
    if (p.t.const) print("const ")
    p.t match {
      case b: BasicType => print(s"${b.name} ${p.name}")
      case o: OpaqueType => print(s"${o.name} ${p.name}")
      case s: StructType => print(s"struct ${s.name} ${p.name}")
      case _: UnionType => ???
      case a: ArrayType => print(s"${typeName(a.getBaseType)} ${p.name}[${ a.getSizes match {
        case None => ""
        case Some(s) => printArithExpr(s, parenthesize = false)}
      }]")
      case pt: PointerType => print(s"${typeName(pt.valueType)}* ${p.name}")
      case _: FragmentType =>
        throw new Exception("FragmentTypes are not supported in C")
    }
  }

  private def printLabelDecl(l: LabelDecl): Unit = {
    println(l.name + ": ;")
  }

  private def printTypedefDecl(t: TypedefDecl): Unit = {
    print("typedef ")
    print(typeName(t.t))
    print(" ")
    print(t.name)
    println(";")
  }

  private def printStructTypeDecl(decl: StructTypeDecl): Unit = {
    print(decl.name) // struct name
    println(" {")
    decl.fields.foreach(field => {
      print("  ")
      printDeclStmt(DeclStmt(field))
      println("")
    })
    println("};")
  }

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
    print(" ")
    printExpr(f.cond, parenthesize = false)
    print("; ")
    printExpr(f.increment, parenthesize = false)
    print(") ")
    printBlock(f.body)
  }

  private def printWhileLoop(w: WhileLoop): Unit = {
    print("while (")
    printExpr(w.cond, parenthesize = false)
    print(") ")
    printStmt(w.body)
  }

  private def printIfThenElse(i: shine.C.AST.IfThenElse): Unit = {
    print("if (")
    printExpr(i.cond, parenthesize = false)
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
    print("return ")
    r.x.foreach(printExpr(_, parenthesize = false))
    println(";")
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
    printExpr(a.lvalue, parenthesize = false)
    print(" = ")
    printExpr(a.rvalue, parenthesize = false)
  }

  private def printDeclRef(d: DeclRef): Unit = {
    print(d.name)
  }

  private def printFunCall(f: FunCall): Unit = {
    printDeclRef(f.fun)
    print("(")
    if (f.args.nonEmpty) {
      f.args.take(f.args.length - 1).foreach(a => {
        printExpr(a, parenthesize = false)
        print(", ")
      })
      printExpr(f.args.last, parenthesize = false)
    }
    print(")")
  }

  private def printArraySubscript(a: ArraySubscript): Unit = {
    printExpr(a.array, parenthesize = true)
    print("[")
    printExpr(a.index, parenthesize = false)
    print("]")
  }

  private def printStructMemberAccess(s: StructMemberAccess): Unit = {
    printExpr(s.struct, parenthesize = true)
    print(".")
    printDeclRef(s.member)
  }

  private def printUnaryExpr(u: UnaryExpr): Unit = {
    print(u.op.toString)
    printExpr(u.e, parenthesize = true)
  }

  private def printBinaryExpr(b: BinaryExpr): Unit = {
    printExpr(b.lhs, parenthesize = true)
    print(s" ${b.op.toString} ")
    printExpr(b.rhs, parenthesize = true)
  }

  private def printTernaryExpr(t: TernaryExpr): Unit = {
    printExpr(t.cond, parenthesize = true)
    print(" ? ")
    printExpr(t.thenE, parenthesize = true)
    print(" : ")
    printExpr(t.elseE, parenthesize = true)
  }

  private def printCast(c: Cast): Unit = {
    print(s"(${typeName(c.t)})")
    printExpr(c.e, parenthesize = true)
  }

  private def printLiteral(l: Literal): Unit = {
    print(l.code)
  }

  private def printArrayLiteral(al: ArrayLiteral): Unit = {
    print("(")
    if (al.t.const) { print("const ") }
    print(s"${typeName(al.t.getBaseType)}[${ al.t.getSizes match {
      case None => ""
      case Some(s) => printArithExpr(s, parenthesize = false)
    } }]")
    print("){")
    var first = true
    al.inits.foreach { e =>
      if (first) { first = false  }
      else { print(", ") }
      printExpr(e, parenthesize = false)
    }
    print("}")
  }

  private def printRecordLiteral(rl: RecordLiteral): Unit = {
    print(s"(${typeName(rl.t)})")
    print("{ ")
    printExpr(rl.fst, parenthesize = false)
    print(", ")
    printExpr(rl.snd, parenthesize = false)
    print(" }")
  }

  private def printArithmeticExpr(a: ArithmeticExpr, parenthesize: Boolean): Unit = {
    print(printArithExpr(a.ae, parenthesize))
  }

  private def printMaybe(parenthesize: Boolean)(p: => Unit): Unit = {
    if (parenthesize) print("(")
    p
    if (parenthesize) print(")")
  }


  // Types
  def typeName(t: Type): String = t.toString

  override def printArithExpr(e: ArithExpr, parenthesize: Boolean) : String = {
    e match {
      case Cst(c) => c.toString
      case Pow(b, ex) =>
        ex match {
          case Cst(2) =>
            maybe(parenthesize)(
              s"${printArithExpr(b, parenthesize = true)} * ${printArithExpr(b, parenthesize = true)}"
            )
          case _ =>
            s"(int)pow((float) ${printArithExpr(b, parenthesize = false)}, " +
              s"${printArithExpr(ex, parenthesize = false)})"
        }
      case Log(b, x) => s"(int)log$b((float)${printArithExpr(x, parenthesize = false)})"
      case Prod(es) => maybe(parenthesize)(es.foldLeft("1")((s, e) => {
          s + (e match {
            case Pow(b, Cst(-1)) => " / " + printArithExpr(b, parenthesize = true)
            case _ => " * " + printArithExpr(e, parenthesize = true)
          })
        } ).drop(4) // drop(4) removes the initial "1 * "
      )
      case Sum(es) =>
        maybe(parenthesize)(
          s"${es.map(printArithExpr(_, parenthesize = true)).reduce(_+" + "+_)}"
        )
      case Mod(a,n) =>
        maybe(parenthesize)(
          s"${printArithExpr(a, parenthesize = true)} % ${printArithExpr(n, parenthesize = true)}"
        )
      case v: Var => v.toString
      case IntDiv(n, d) =>
        maybe(parenthesize)(
          s"${printArithExpr(n, parenthesize = true)} / ${printArithExpr(d, parenthesize = true)}"
        )
      case lu: Lookup => "lookup" + lu.id + "(" + printArithExpr(lu.index, parenthesize = false) + ")"
      case i: arithexpr.arithmetic.IfThenElse =>
        maybe(parenthesize)(
          s"${printBoolExpr(i.test)} ? " +
          s"${printArithExpr(i.t, parenthesize = true)} : ${printArithExpr(i.e, parenthesize = true)}"
        )
      case natFunCall:NatFunCall => natFunCall.callAndParameterListString
      case sp: SteppedCase => printArithExpr(sp.intoIfChain(), parenthesize)
      case otherwise => throw new Exception(s"Don't know how to print $otherwise")
    }
  }

  private def printBoolExpr(boolExpr: BoolExpr):String = boolExpr match {
    case BoolExpr.True => "true"
    case BoolExpr.False => "false"
    case BoolExpr.ArithPredicate(lhs, rhs, op) =>
      s"${printArithExpr(lhs, parenthesize = true)} $op ${printArithExpr(rhs, parenthesize = true)}"
  }

  private def maybe(parenthesize: Boolean)(s: String): String =
    (if (parenthesize) "(" else "") + s + (if (parenthesize) ")" else "")


}
