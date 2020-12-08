package shine.OpenCL.AST

import arithexpr.arithmetic.ArithExpr
import shine.C.AST._
import shine.DPIA.Types.AddressSpaceIdentifier
import shine.OpenCL
import shine.OpenCL.{AddressSpace, BuiltInFunctionCall, NDRange}

object Printer {
  def apply(n: Node): String = (new Printer).printNode(n)
}

class Printer extends shine.C.AST.CPrinter {
  override def printDecl(d: Decl): Unit = d match {
    case k: OpenCL.AST.KernelDecl => printKernelDecl(k)
    case p: ParamDecl => printParamDecl(p)
    case v: OpenCL.AST.VarDecl => printVarDecl(v)
    case _ => super.printDecl(d)
  }

  override def printExpr(e: Expr): Unit = e match {
    case vs: VectorSubscript => printVectorSubscript(vs)
    case vl: VectorLiteral => printVectorLiteral(vl)
    case _ => super.printExpr(e)
  }

  override def printStmt(s: Stmt): Unit = s match {
    case b: Barrier =>
      print("barrier(")
        if (b.local) print("CLK_LOCAL_MEM_FENCE")
        if (b.local && b.global) print(" | ")
        if (b.global) print("CLK_GLOBAL_MEM_FENCE")
      print(");")
    case _ => super.printStmt(s)
  }

  override def typeName(t: Type): String = super.typeName(t)

  override def toString(e: ArithExpr): String = e match {
    case of: BuiltInFunctionCall => of.toString

    case _ => super.toString(e)
  }

  def printKernelDecl(k: KernelDecl): Unit = {
    print("__kernel")
    k.attribute match {
      case Some(RequiredWorkGroupSize(NDRange(x, y, z))) =>
        print(s" __attribute__ ((reqd_work_group_size($x, $y, $z)))")
      case None =>
    }
    println("")
    print(s"void ${k.name}(")
    k.params.foreach(p => {
      printDecl(p)
      if (!p.eq(k.params.last)) print(", ")
    })
    print(")")

    printStmt(k.body)
  }

  def printParamDecl(p: ParamDecl): Unit = {
    if (p.t.const) print("const ")
    p.t match {
      case b: BasicType => print(s"${b.name} ${p.name}")
      case s: StructType => print(s"struct ${s.name} ${p.name}")
      case _: UnionType => ???
      case _: ArrayType => throw new Exception("Arrays as parameters are not supported")
//        val addr = if (a.a == AddressSpace.Private) "" else s"${toString(a.a)} "
//        val size = a.getSizes match {
//          case None => ""
//          case Some(s) => s
//        }
//        print(s"$addr${a.getBaseType} ${p.name}[$size]")
      case pt: OpenCL.AST.PointerType => print(s"${toString(pt.a)} ${pt.valueType}* restrict ${p.name}")
      case _: shine.C.AST.PointerType => throw new Exception("Pointer without address space unsupported in OpenCL")
    }
  }

  private def printVarDecl(v: shine.OpenCL.AST.VarDecl): Unit = {
    if (v.addressSpace != AddressSpace.Private) print(s"${toString(v.addressSpace)} ")
    if (v.t.const) print("const ")
    v.t match {
      case b: BasicType => print(s"${b.name} ${v.name}")
      case s: StructType => print(s"struct ${s.name} ${v.name}")
      case a: ArrayType =>
        // float name[s];
        print(s"${a.getBaseType} ${v.name}[${ a.getSizes match {
          case None => ""
          case Some(s) => s
        } }]")
      case p: shine.OpenCL.AST.PointerType => print(s"${toString(p.a)} ${p.valueType}* ${v.name}")
      case _: shine.C.AST.PointerType => throw new Exception("This should not happen")
      case _: shine.C.AST.UnionType => ???
    }
    v.init match {
      case None =>
      case Some(init) =>
        print(" = ")
        printExpr(init)
    }
  }

  def toString(addressSpace: OpenCL.AddressSpace): String = addressSpace match {
    case AddressSpace.Global  => "global"
    case AddressSpace.Local   => "local"
    case AddressSpace.Private => "private"
    case AddressSpace.Constant => "constant"
    case AddressSpaceIdentifier(name) => name
  }

  def printVectorSubscript(vs: VectorSubscript): Unit = {
    printExpr(vs.vector)
    vs.index match {
      case ArithmeticExpr(arithexpr.arithmetic.Cst(c)) =>
        print(s".s$c")
      case Literal(str) =>
        print(s".s$str")
      case _ => throw new Exception(s"did not expect ${vs.index}")
    }
  }

  def printVectorLiteral(vl: VectorLiteral): Unit = {
    val VectorType(m, dt, _) = vl.t
    print(s"($dt$m)")
    print("(")
    printExpr(vl.values.head)
    for (v <- vl.values.tail) {
      print(", ")
      printExpr(v)
    }
    print(")")
  }
}
