package idealised.C.AST

import lift.arithmetic._
import ir.view.AccessVar

object Printer {
  def apply(n: Node): String = (new Printer)(n)
}

class Printer {
  def apply(n: Node): String = {
    print(n)
    sb.toString()
  }

  private val sb: StringBuilder = new StringBuilder
  private var indent: Int = 0

  private def print(s: String): Unit = {
    sb ++= s
  }

  private def println(s: String): Unit = {
    sb ++= s + "\n" + tab()
  }

  private val tabSize = 2

  private def tab() = {
    lazy val whiteSpace: String = " " * tabSize
    whiteSpace * indent
  }

  private def moveCursorBack(size: Int): Unit = {
    for (_ <- 1 to size) {
      if (sb.last.isWhitespace) { sb.deleteCharAt(sb.size - 1) }
    }
  }

  private def print(n: Node): Unit = n match {
    case d: Decl => print(d)
    case e: Expr => print(e)
    case s: Stmt => print(s)
  }

  private def print(d: Decl): Unit = d match {
    case f: FunDecl => print(f)
    case v: VarDecl => print(v)
    case p: ParamDecl => print(p)
    case l: LabelDecl => print(l)
    case t: TypedefDecl => print(t)
  }

  private def print(s: Stmt): Unit = s match {
    case s: Stmts => print(s)
    case b: Block => print(b)
    case f: ForLoop => print(f)
    case w: WhileLoop => print(w)
    case i: IfThenElse => print(i)
    case g: GOTO => print(g)
    case b: Break => print(b)
    case c: Continue => print(c)
    case r: Return => print(r)
    case d: DeclStmt => print(d)
    case c: Comment => print(c)
    case c: Code => print(c)
    case e: Expr =>
      print(e)
      print(";")
  }

  private def print(e: Expr): Unit = e match {
    case a: Assignment => print(a)
    case d: DeclRef => print(d)
    case f: FunCall => print(f)
    case s: StructMemberAccess => print(s)
    case a: ArraySubscript => print(a)
    case u: UnaryExpr => print(u)
    case b: BinaryExpr => print(b)
    case t: TernaryExpr => print(t)
    case c: Cast => print(c)
    case l: Literal => print(l)
    case a: ArithmeticExpr => print(a)
  }

  // Decls
  private def print(f: FunDecl): Unit = {
    print(typeName(f.returnType))
    print(s" ${f.name}(")
    f.params.foreach(p => {
      print(p)
      if (!p.eq(f.params.last)) print(", ")
    })
    print(")")

    print(f.body)
  }

  private def print(v: VarDecl): Unit = {
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
        print(init)
    }
  }

  private def print(p: ParamDecl): Unit = {
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

  private def print(l: LabelDecl): Unit = {
    println(l.name + ": ;")
  }

  private def print(t: TypedefDecl): Unit = ???

  // Smts
  private def print(s: Stmts): Unit = {
    print(s.fst)
    println("")
    print(s.snd)
  }

  private def print(b: Block): Unit = {
    indent += 1
    println("{")
    b.body.foreach( (s: Stmt) => {
      print(s)
      println("")
    })
    indent -= 1
    moveCursorBack(tabSize)
    println("}")
  }

  private def print(f: ForLoop): Unit = {
    print("for (")
    print(f.init)
    print(f.cond)
    print(";")
    print(f.increment)
    print(") ")
    print(f.body)
  }

  private def print(w: WhileLoop): Unit = {
    print("while (")
    print(w.cond)
    print(") ")
    print(w.body)
  }

  private def print(i: IfThenElse): Unit = {
    print("if (")
    print(i.cond)
    print(") ")
    print(i.trueBody)

    i.falseBody match {
      case Some(falseBody) =>
        print(" else ")
        print(falseBody)
      case None =>
    }
  }

  private def print(g: GOTO): Unit = {
    println("goto " + g.label + ";")
  }

  private def print(b: Break): Unit = {
    println("break;")
  }

  private def print(c: Continue): Unit = {
    println("continue;")
  }

  private def print(r: Return): Unit = {
    println("return;")
  }

  private def print(d: DeclStmt): Unit = {
    print(d.decl)
    print(";")
  }

  private def print(c: Comment): Unit = {
    print(s"/* ${c.string} */")
  }

  private def print(c: Code): Unit = {
    print(c.string)
  }

  // Exprs
  private def print(a: Assignment): Unit = {
    print(a.lvalue)
    print(" = ")
    print(a.rvalue)
  }

  private def print(d: DeclRef): Unit = {
    print(d.name)
  }

  private def print(f: FunCall): Unit = {
    print(f.fun)
    print("(")
    f.args.foreach(a => {
      print(a)
      if (!a.eq(f.args.last)) print(", ")
    })
    print(")")
  }

  private def print(a: ArraySubscript): Unit = {
    print(a.array)
    print("[")
    print(a.index)
    print("]")
  }

  private def print(s: StructMemberAccess): Unit = {
    print(s.struct)
    print(".")
    print(s.member)
  }

  private def print(u: UnaryExpr): Unit = {
    print("(")
    print(u.op.toString)
    print("(")
    print(u.e)
    print("))")
  }

  private def print(b: BinaryExpr): Unit = {
    print("(")
    print(b.lhs)
    print(" ")
    print(b.op.toString)
    print(" ")
    print(b.rhs)
    print(")")
  }

  private def print(t: TernaryExpr): Unit = {
    print("(")
    print(t.cond)
    print(") ? (")
    print(t.thenE)
    print(") : (")
    print(t.elseE)
    print(")")
  }

  private def print(c: Cast): Unit = {
    print("(")
    print(s"(${c.t})")
    print(c.e)
    print(")")
  }

  private def print(l: Literal): Unit = {
    print(l.code)
  }

  private def print(a: ArithmeticExpr): Unit = {
    print(toString(a.ae))
  }

  // Types
  private def typeName(t: Type): String = t.toString

  private def toString(e: ArithExpr) : String = {
    e match {
      case Cst(c) => c.toString
      case Pow(b, ex) =>
        "(int)pow((float)" + toString(b) + ", " + toString(ex) + ")"
      case Log(b, x) => "(int)log"+b+"((float)"+toString(x)+")"
      case Prod(es) => "(" + es.foldLeft("1")( (s: String, e: ArithExpr) => {
        s + (e match {
          case Pow(b, Cst(-1)) => " / (" + toString(b) + ")"
          case _ => " * " + toString(e)
        })
      } ).drop(4) + ")" // drop(4) removes the initial "1 * "
      case Sum(es) => "(" + es.map(toString).reduce( _ + " + " + _  ) + ")"
      case Mod(a,n) => "(" + toString(a) + " % " + toString(n) + ")"
//      case of: OclFunction => of.toOCLString
//      case ai: AccessVar => ai.array + "[" + toString(ai.idx.content) + "]"
      case v: Var => v.toString
      case IntDiv(n, d) => "(" + toString(n) + " / " + toString(d) + ")"
      case lu: Lookup => "lookup" + lu.id + "(" + toString(lu.index) + ")"
      case i: lift.arithmetic.IfThenElse =>
        s"( (${toString(i.test.lhs)} ${i.test.op} ${toString(i.test.rhs)}) ? " +
          s"${toString(i.t)} : ${toString(i.e)} )"
      case _ => ???
    }
  }


}
