package idealised.DPIA.ImperativePrimitives

import idealised.DPIA.Phrases._
import idealised.DPIA.Semantics.OperationalSemantics._
import idealised.DPIA.Types._
import idealised.DPIA._

import scala.xml.Elem

abstract class AbstractParForNat[T <: DataType](val n: Nat,
                                                val i: NatIdentifier,
                                                val dt: T,
                                                val out: Phrase[AccType],
                                                val body: Phrase[`(nat)->`[(AccType -> CommandType)]])
  extends CommandPrimitive {

  override lazy val `type`: CommandType = {
    (n: Nat) -> (dt: DataType) ->
      (out :: acc"[${DepArrayType(n, i, dt)}]") ->
      (body :: t"(${body.t.x}:nat) -> acc[$dt] -> comm") ->
      comm
  }
  override def eval(s: Store): Store = ???

  override def visitAndRebuild(fun: VisitAndRebuild.Visitor): Phrase[CommandType] = {
    makeParForNat(fun(n), fun(i).asInstanceOf[NatIdentifier], fun(dt), VisitAndRebuild(out, fun), VisitAndRebuild(body, fun))
  }

  override def prettyPrint: String =
    s"(${this.getClass.getSimpleName} $n ${PrettyPhrasePrinter(out)} ${PrettyPhrasePrinter(body)})"


  override def xmlPrinter: Elem =
    <parForNat n={ToString(n)} dt={ToString(dt)}>
      <output type={ToString(AccType(ArrayType(n, dt)))}>
        {Phrases.xmlPrinter(out)}
      </output>
      <body type={ToString(body.t.x -> (AccType(dt) -> CommandType()))}>
        {Phrases.xmlPrinter(body)}
      </body>
    </parForNat>.copy(label = {
      val name = this.getClass.getSimpleName
      Character.toLowerCase(name.charAt(0)) + name.substring(1)
    })

  def makeParForNat: (Nat, NatIdentifier, T, Phrase[AccType], Phrase[`(nat)->`[(AccType -> CommandType)]]) => AbstractParForNat[T]

}

final case class ParForNat(override val n: Nat,
                           override val i: NatIdentifier,
                           override val dt: DataType,
                           override val out: Phrase[AccType],
                           override val body: Phrase[`(nat)->`[(AccType -> CommandType)]])
  extends AbstractParForNat[DataType](n, i, dt, out, body) {
  override def makeParForNat = ParForNat
}
