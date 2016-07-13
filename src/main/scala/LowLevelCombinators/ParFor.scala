package LowLevelCombinators

import Core.OperationalSemantics._
import Core._
import DSL.typed._

import scala.xml.Elem

abstract class AbstractParFor(n: Nat,
                              dt: DataType,
                              out: Phrase[AccType],
                              body: Phrase[ExpType -> (AccType -> CommandType)])
  extends LowLevelCommCombinator {

  override def typeCheck(): Unit = {
    import TypeChecker._
    (n: Nat) -> (dt: DataType) ->
      (out `:` acc"[$n.$dt]") ->
      (body `:` t"exp[idx($n)] -> acc[$dt] -> comm") ->
      comm
  }

  override def eval(s: Store): Store = {
    val nE = evalIndexExp(s, LiteralPhrase(IndexData(n)))
    val bodyE = OperationalSemantics.eval(s, body)(OperationalSemantics.BinaryFunctionEvaluator)

    (0 until nE.eval).foldLeft(s)((s1, i) => {
      OperationalSemantics.eval(s1, bodyE(LiteralPhrase(i))(out `@` LiteralPhrase(i)))
    })
  }

  override def visitAndRebuild(fun: VisitAndRebuild.fun): Phrase[CommandType] = {
    makeParFor(fun(n), fun(dt), VisitAndRebuild(out, fun), VisitAndRebuild(body, fun))
  }

  override def prettyPrint: String =
    s"(${this.getClass.getSimpleName} $n ${PrettyPrinter(out)} ${PrettyPrinter(body)})"


  override def xmlPrinter: Elem =
    <parFor n={ToString(n)} dt={ToString(dt)}>
      <output type={ToString(AccType(ArrayType(n, dt)))}>
        {Core.xmlPrinter(out)}
      </output>
      <body type={ToString(ExpType(int) -> (AccType(dt) -> CommandType()))}>
        {Core.xmlPrinter(body)}
      </body>
    </parFor>.copy(label = {
      val name = this.getClass.getSimpleName
      Character.toLowerCase(name.charAt(0)) + name.substring(1)
    })

  def makeParFor: (Nat, DataType, Phrase[AccType], Phrase[ExpType -> (AccType -> CommandType)]) => AbstractParFor

}

final case class ParFor(n: Nat,
                        dt: DataType,
                        out: Phrase[AccType],
                        body: Phrase[ExpType -> (AccType -> CommandType)])
  extends AbstractParFor(n, dt, out, body) {
  override def makeParFor = ParFor
}
