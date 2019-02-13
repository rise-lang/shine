package idealised.OpenCL.AST

import idealised.C.AST._
import idealised.OpenCL
import idealised.OpenCL.{NDRange, BuiltInFunction}
import lift.arithmetic.ArithExpr

object Printer {
  def apply(n: Node): String = (new Printer).printNode(n)
}

class Printer extends idealised.C.AST.CPrinter {
  override def printDecl(d: Decl): Unit = d match {
    case k: OpenCL.AST.KernelDecl => printKernelDecl(k)
    case p: OpenCL.AST.ParamDecl => printParamDecl(p)
    case v: OpenCL.AST.VarDecl => printVarDecl(v)
    case _ => super.printDecl(d)
  }

  override def printExpr(e: Expr): Unit = super.printExpr(e)

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
    case of: BuiltInFunction => of.toOCLString

    case _ => super.toString(e)
  }

  private def printKernelDecl(k: KernelDecl): Unit = {
    k.attribute match {
      case Some(RequiredWorkGroupSize(NDRange(x, y, z))) =>
        println(s"__attribute__ ((reqd_work_group_size($x, $y, $z)))")
      case None =>
    }
    println("__kernel")
    print(s"void ${k.name}(")
    k.params.foreach(p => {
      printDecl(p)
      if (!p.eq(k.params.last)) print(", ")
    })
    print(")")

    printStmt(k.body)
  }

  private def printParamDecl(p: ParamDecl): Unit = {
    if (p.t.const) print("const ")
    p.t match {
      case b: BasicType => print(s"${b.name} ${p.name}")
      case s: StructType => print(s"struct ${s.name} ${p.name}")
      case _: UnionType => ???
      case a: ArrayType =>
        print(s"${a.getBaseType} ${p.name}[${ a.getSizes match {
          case None => ""
          case Some(s) => s
        } }]")
      case pt: PointerType => print(s"${toString(p.addressSpace)} ${pt.valueType}* restrict ${p.name}")
    }
  }

  private def printVarDecl(v: VarDecl): Unit = {
    if (v.t.const) print("const ")
    v.t match {
      case b: BasicType if v.addressSpace == OpenCL.PrivateMemory => print(s"${b.name} ${v.name}")
      case s: StructType if v.addressSpace == OpenCL.PrivateMemory => print(s"struct ${s.name} ${v.name}")
      case p: PointerType => print(s"${toString(v.addressSpace)} ${p.valueType}* ${v.name}")
    }
    v.init match {
      case None =>
      case Some(init) =>
        print(" = ")
        printExpr(init)
    }
  }

  def toString(addressSpace: OpenCL.AddressSpace): String = addressSpace match {
    case OpenCL.GlobalMemory  => "global"
    case OpenCL.LocalMemory   => "local"
    case OpenCL.PrivateMemory => "private"
  }
}
