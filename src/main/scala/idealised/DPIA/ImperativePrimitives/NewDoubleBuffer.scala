package idealised.DPIA.ImperativePrimitives

import idealised.DPIA.Compilation.CodeGenerator
import idealised.DPIA.Phrases._
import idealised.DPIA.Semantics.OperationalSemantics._
import idealised.DPIA.Types._
import idealised.DPIA._

import scala.xml.Elem

final case class NewDoubleBuffer(dt1: DataType,
                                 dt2: DataType,
                                 dt3: DataType,
                                 n: Nat,
                                 in: Phrase[ExpType],
                                 out: Phrase[AccType],
                                 f: Phrase[ExpType x AccType x CommandType x CommandType -> CommandType])
  extends CommandPrimitive with GeneratableCommand {

  override val `type`: CommandType =
    (dt1: DataType) -> (dt2: DataType) -> (dt3: DataType) -> (n: Nat) ->
      (in :: exp"[$dt1]") ->
        (out :: acc"[$dt2]") ->
          (f :: FunctionType(PairType(PairType(VarType(ArrayType(n, dt3)), comm), comm), comm) ) -> comm

  override def eval(s: Store): Store = ???

  override def codeGen[Environment, Path, Stmt, Expr, Decl, Ident](gen: CodeGenerator[Environment, Path, Stmt, Expr, Decl, Ident])(env: Environment): Stmt = {
    f match {
      case Lambda(ps, p) => gen.codeGenNewDoubleBuffer(ArrayType(n, dt3), in, out, ps, p, env, gen)
      case _ => error(s"Expected lambda")
    }
  }

  override def visitAndRebuild(fun: VisitAndRebuild.Visitor): Phrase[CommandType] = {
    NewDoubleBuffer(fun(dt1), fun(dt2), fun(dt3), fun(n), VisitAndRebuild(in, fun), VisitAndRebuild(out, fun), VisitAndRebuild(f, fun))
  }

  override def prettyPrint: String = s"(new $${PrettyPhrasePrinter(f)})"

  override def xmlPrinter: Elem =
    <newDoubleBuffer dt1={ToString(dt1)}>
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
