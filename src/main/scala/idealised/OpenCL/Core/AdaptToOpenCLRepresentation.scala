package idealised.OpenCL.Core

import idealised.Core.{ExpType, IdentPhrase}
import ir._
import opencl.generator.OpenCLAST._
import opencl.ir._

import scala.collection.mutable

// This pass performs the following adaptions to the OpenCL IR to overcome representation
// differences between LIFT and OpenCL:
//
//  - Parameters (ParDecl) in global or local memory which have a non-array type in LIFT are
//    represented as arrays of size 1 in OpenCL. Every VarRef to such a parameter is adjusted
//    accordingly.
//
object AdaptToOpenCLRepresentation {

  def apply(k: idealised.OpenCL.Kernel): Function = {
    (new AdaptToOpenCLRepresentation).adaptFunction(k)
  }

}

class AdaptToOpenCLRepresentation {
  val varRefReplacements = mutable.Map[String, VarRef]()

  def adaptFunction(k: idealised.OpenCL.Kernel): Function = {
    k.function.copy(params = k.function.params.map(adaptParamDecl(k.inputParams)), body = adaptBlock(k.function.body))
  }

  private def adaptParamDecl(inputParams: List[IdentPhrase[ExpType]])(paramDecl: ParamDecl): ParamDecl = {
    paramDecl.t match {
      case _: ScalarType =>
        paramDecl.addressSpace match {
          case GlobalMemory | LocalMemory =>
            val name = paramDecl.name
            // replace `name' with `name[0]'
            varRefReplacements(name) = VarRef(name, arrayIndex = ArithExpression(0))
            paramDecl.copy(t = ArrayType(paramDecl.t, 1))
          case _ => paramDecl
        }
      case _: ArrayType =>
        if (inputParams.map(_.name).contains(paramDecl.name)) {
          paramDecl.copy(const=true)
        } else {
          paramDecl
        }
      case _ => paramDecl
    }
  }

  private def adaptBlock(block: Block): Block = {
    Block(block.content.map(adaptBlockMember), block.global)
  }

  private def adaptNode(node: OclAstNode): OclAstNode = {
    node match {
      case d: Declaration => adaptDeclaration(d)
      case s: Statement => adaptStatement(s)
      case e: Expression => adaptExpression(e)
      case Comment(_) | OpenCLCode(_) | OpenCLExtension(_) | Skip() | RequiredWorkGroupSize(_) => node
      case null => null
    }
  }

  private def adaptBlockMember(node: OclAstNode with BlockMember): OclAstNode with BlockMember = {
    node match {
      case d: Declaration => adaptDeclaration(d)
      case s: Statement => adaptStatement(s)
      case Comment(_) | OpenCLCode(_) | OpenCLExtension(_) | Skip() => node
      case null => null
    }
  }

  private def adaptDeclaration(decl: Declaration): Declaration = {
    decl match {
      case f: Function => f.copy(params = f.params.map(adaptParamDecl(List())), body = adaptBlock(f.body))
      case v: VarDecl => v.copy(init = adaptNode(v.init))
      case p: ParamDecl => adaptParamDecl(List())(p)
      case l: Label => l
    }
  }

  private def adaptStatement(statement: Statement): Statement = {
    statement match {
      case b: Block => adaptBlock(b)
      case f: ForLoop => ForLoop(
        init = adaptDeclaration(f.init),
        cond = ExpressionStatement(adaptExpression(f.cond.e)),
        increment = adaptExpression(f.increment),
        body = adaptBlock(f.body)
      )
      case w: WhileLoop => WhileLoop(w.loopPredicate, adaptBlock(w.body))
      case i: IfThenElse => IfThenElse(
        adaptExpression(i.cond),
        adaptBlock(i.trueBody),
        adaptBlock(i.falseBody)
      )
      case ExpressionStatement(e) => ExpressionStatement(adaptExpression(e))
      case GOTO(_) | Barrier(_) | TypeDef(_) | TupleAlias(_, _) | Break() => statement
    }
  }

  private def adaptExpression(e: Expression): Expression = {
    e match {
      case f: FunctionCall => f.copy(args = f.args.map(adaptNode))
      case v: VarRef => adaptVarRef(v)
      case l: Load => l.copy(v = adaptVarRef(l.v))
      case s: Store => s.copy(v = adaptVarRef(s.v), value = adaptNode(s.value))
      case vl: VLoad => vl.copy(v = adaptVarRef(vl.v), offset = adaptExpression(vl.offset))
      case vs: VStore => vs.copy(
        v = adaptVarRef(vs.v),
        value = adaptNode(vs.value),
        offset = adaptExpression(vs.offset)
      )
      case a: AssignmentExpression => AssignmentExpression(adaptNode(a.to), adaptNode(a.value))
      case a: ArithExpression => a
      case b: BinaryExpression => b.copy(lhs = adaptExpression(b.lhs), rhs = adaptExpression(b.rhs))
      case u: UnaryExpression => u.copy(e = adaptExpression(u.e))
      case c: CondExpression => c.copy(lhs = adaptExpression(c.lhs), rhs = adaptExpression(c.rhs))
      case p: PointerCast => p.copy(v = adaptVarRef(p.v))
      case c: Cast => c.copy(v = adaptVarRef(c.v))
      case v: VectorLiteral => VectorLiteral(v.t, v.vs.map(adaptExpression):_*)
      case s: StructConstructor => s.copy(args = s.args.map(adaptNode))
      case l: Literal => l
      case o: OpenCLExpression => o
      case null => null
    }
  }

  private def adaptVarRef(v: VarRef): VarRef = {
    varRefReplacements.getOrElse(v.name, v.copy(arrayIndex = v.arrayIndex))
  }
}
