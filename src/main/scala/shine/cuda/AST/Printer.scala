package shine.cuda.AST

import arithexpr.arithmetic.ArithExpr
import shine.C.AST._
import shine.DPIA.Types.AddressSpace
import shine.OpenCL
import shine.OpenCL.AST.KernelDecl
import shine.OpenCL.BuiltInFunctionCall

object Printer {
  def apply(n: Node): String = (new Printer).printNode(n)
}

class Printer extends shine.OpenCL.AST.Printer {
  override def printStmt(s: Stmt): Unit = s match {
    case _: SynchronizeThreads => print("__syncthreads();")
    case _: SynchronizeWarp => print("__syncwarp();")
    case _ => super.printStmt(s)
  }

  override def toString(e: ArithExpr): String = e match {
    case of: BuiltInFunctionCall =>
      of.name match {
        case "get_num_groups" => s"gridDim.${of.param.toChar}"
        case "get_local_size" => s"blockDim.${of.param.toChar}"
        case "get_local_id" => s"threadIdx.${of.param.toChar}"
        case "get_group_id" => s"blockIdx.${of.param.toChar}"
      }

    case _ => super.toString(e)
  }

  override def printKernelDecl(k: KernelDecl): Unit = {
    //TODO this include statement is not necessary for every CUDA-Kernel
    println("#include <mma.h>")

    print("extern \"C\" __global__")
    println("")
    print(s"void ${k.name}(")
    k.params.foreach(p => {
      printDecl(p)
      if (!p.eq(k.params.last)) print(", ")
    })
    print(")")

    printStmt(k.body)
  }

  override def printDecl(d: Decl): Unit = {
    d match {
      case v: shine.OpenCL.AST.VarDecl if v.t.isInstanceOf[ExternArrayType] =>
        print("extern ")
      case _ =>
    }

    super.printDecl(d)
  }


  override def printParamDecl(p: ParamDecl): Unit = {
    if (p.t.const) print("const ")
    p.t match {
      case b: BasicType => print(s"${b.name} ${p.name}")
      case s: StructType => print(s"struct ${s.name} ${p.name}")
      case _: UnionType => ???
      case _: ArrayType =>
        throw new Exception("Arrays as parameters are not supported")
      case pt: OpenCL.AST.PointerType =>
        val addrSpaceStr= toString(pt.a)
        val spaceAdj = if (addrSpaceStr.length == 0) "" else addrSpaceStr + " "
        print(s"$spaceAdj${pt.valueType}* __restrict__ ${p.name}")
      case _: shine.C.AST.PointerType =>
        throw new Exception(
          "Pointer without address space unsupported in OpenCL")
    }
  }

  override def printVarDecl(v: shine.OpenCL.AST.VarDecl): Unit = {
    v.t match {
      case f: FragmentType =>
        if (v.addressSpace != shine.cuda.AddressSpace.Private)
          throw new Exception("fragments only be stored in private memory!")

        print(s"${f.print} ${v.name}")
      case _ => super.printVarDecl(v)
    }
  }

  override def toString(
    addressSpace: OpenCL.AddressSpace
  ): String = addressSpace match {
    case AddressSpace.Global  => ""
    case AddressSpace.Local   => "__shared__"
    case AddressSpace.Private => ""
    case _ => ???
  }
}
