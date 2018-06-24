package idealised.DPIA.ImperativePrimitives

import idealised.DPIA.Compilation.CodeGenerator
import idealised.DPIA.DSL.identifier
import idealised.DPIA.Phrases._
import idealised.DPIA.Semantics.OperationalSemantics
import idealised.DPIA.Semantics.OperationalSemantics._
import idealised.DPIA.Types._
import idealised.DPIA._

import scala.xml.Elem

final case class NewDoubleBuffer(dt: DataType,
                                 in: Phrase[ExpType],
                                 out: Phrase[AccType],
                                 f: Phrase[ExpType x AccType x CommandType x CommandType -> CommandType])
  extends CommandPrimitive with GeneratableCommand {

  override val `type`: CommandType =
    (dt: DataType) -> /* (addressSpace: AddressSpace) -> */
      (in :: exp"[$dt]") ->
        (out :: acc"[$dt]") ->
          (f :: t"${PairType(PairType(VarType(dt), comm), comm)} -> comm") -> comm

  override def eval(s: Store): Store = ???

  override def codeGen[Environment, Path, Stmt, Expr, Decl](gen: CodeGenerator[Environment, Path, Stmt, Expr, Decl])(env: Environment): Stmt = {
    f match {
      case Lambda(ps, p) => gen.codeGenNewDoubleBuffer(dt, ps, p, env, gen)
      case _ => error(s"Expected lambda")
    }
  }

  override def visitAndRebuild(fun: VisitAndRebuild.Visitor): Phrase[CommandType] = {
    NewDoubleBuffer(fun(dt), VisitAndRebuild(in, fun), VisitAndRebuild(out, fun), VisitAndRebuild(f, fun))
  }

  override def prettyPrint: String = s"(new $${PrettyPhrasePrinter(f)})"

  override def xmlPrinter: Elem =
    <newDoubleBuffer dt={ToString(dt)}>
      <in>
        {Phrases.xmlPrinter(in)}
      </in>
      <out>
        {Phrases.xmlPrinter(out)}
      </out>
      <f>
        {Phrases.xmlPrinter(f)}
      </f>
    </newDoubleBuffer>
}
