package shine.DPIA.ImperativePrimitives

import shine.DPIA.DSL._
import shine.DPIA.FunctionalPrimitives.AsIndex
import shine.DPIA.Phrases._
import shine.DPIA.Semantics.OperationalSemantics
import shine.DPIA.Semantics.OperationalSemantics._
import shine.DPIA.Types._
import shine.DPIA.Types.DataType._
import shine.DPIA._

import scala.xml.Elem

final case class ForVec(n: Nat,
                        dt: ScalarType,
                        out: Phrase[AccType],
                        body: Phrase[ExpType ->: AccType ->: CommType])
  extends CommandPrimitive
{
  out :: accT(vec(n, dt))
  body :: expT(idx(n), read) ->: accT(dt) ->: comm

  override def eval(s: Store): Store = {
    val nE = evalIndexExp(s, AsIndex(n, Natural(n)))
    val bodyE = OperationalSemantics.eval(s, body)(OperationalSemantics.BinaryFunctionEvaluator)

    (0 until nE.eval).foldLeft(s)((s1, i) => {
      OperationalSemantics.eval(s1,
        bodyE(Literal(i))(out `@` Literal(i)))
    })
  }

  override def visitAndRebuild(fun: VisitAndRebuild.Visitor): Phrase[CommType] = {
    ForVec(fun.nat(n), fun.data(dt), VisitAndRebuild(out, fun), VisitAndRebuild(body, fun))
  }

  override def prettyPrint: String =
    s"(ForVec $n ${PrettyPhrasePrinter(out)} ${PrettyPhrasePrinter(body)})"


  override def xmlPrinter: Elem =
    <forVec n={ToString(n)} dt={ToString(dt)}>
      <output type={ToString(AccType(VectorType(n, dt)))}>
        {Phrases.xmlPrinter(out)}
      </output>
      <body type={ToString(ExpType(IndexType(n), read) ->: AccType(dt) ->: CommType())}>
        {Phrases.xmlPrinter(body)}
      </body>
    </forVec>
}
