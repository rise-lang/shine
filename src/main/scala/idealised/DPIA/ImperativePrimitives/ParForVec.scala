package idealised.DPIA.ImperativePrimitives

import idealised.DPIA.DSL._
import idealised.DPIA.Phrases._
import idealised.DPIA.Semantics.OperationalSemantics
import idealised.DPIA.Semantics.OperationalSemantics._
import idealised.DPIA.Types._
import idealised.DPIA._

import scala.xml.Elem

final case class ParForVec(n: Nat,
                           st: ScalarType,
                           out: Phrase[AccType],
                           body: Phrase[ExpType -> (AccType -> CommandType)])
  extends CommandPrimitive
{

  override val `type`: CommandType =
    (n: Nat) -> (st: ScalarType) ->
      (out :: acc"[${VectorType(n, st)}]") ->
        (body :: t"exp[idx($n)] -> acc[$st] -> comm") ->
          comm

  override def eval(s: Store): Store = {
    val nE = evalIndexExp(s, Literal(IndexData(n, IndexType(n))))
    val bodyE = OperationalSemantics.eval(s, body)(OperationalSemantics.BinaryFunctionEvaluator)

    (0 until nE.eval).foldLeft(s)((s1, i) => {
      OperationalSemantics.eval(s1,
        bodyE(Literal(i))(out `@` Literal(i)))
    })
  }

  override def visitAndRebuild(fun: VisitAndRebuild.Visitor): Phrase[CommandType] = {
    ParForVec(fun(n), fun(st), VisitAndRebuild(out, fun), VisitAndRebuild(body, fun))
  }

  override def prettyPrint: String =
    s"(${this.getClass.getSimpleName} $n ${PrettyPhrasePrinter(out)} ${PrettyPhrasePrinter(body)})"


  override def xmlPrinter: Elem =
    <parForVec n={ToString(n)} st={ToString(st)}>
      <output type={ToString(AccType(VectorType(n, st)))}>
        {Phrases.xmlPrinter(out)}
      </output>
      <body type={ToString(ExpType(IndexType(n)) -> (AccType(st) -> CommandType()))}>
        {Phrases.xmlPrinter(body)}
      </body>
    </parForVec>
}