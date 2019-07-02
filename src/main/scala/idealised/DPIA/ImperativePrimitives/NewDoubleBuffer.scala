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
  extends CommandPrimitive {

  override val t: CommandType =
    (dt1: DataType) -> (dt2: DataType) -> (dt3: DataType) -> (n: Nat) ->
      (in :: exp"[$dt1, $read]") ->
        (out :: acc"[$dt2]") ->
          (f :: FunctionType(PairType(PairType(VarType(ArrayType(n, dt3)), comm), comm), comm) ) -> comm

  override def eval(s: Store): Store = ???

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
