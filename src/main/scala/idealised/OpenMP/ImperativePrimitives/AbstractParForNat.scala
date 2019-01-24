package idealised.OpenMP.ImperativePrimitives


import idealised.DPIA.Phrases._
import idealised.DPIA.Semantics.OperationalSemantics._
import idealised.DPIA.Types._
import idealised.DPIA._

import scala.xml.Elem

abstract class AbstractParForNat[T <: DataType](val n: Nat,
                                                val i: NatIdentifier,
                                                val dt: T,
                                                val out: Phrase[AccType],
                                                val body: Phrase[`(nat)->`[AccType -> CommandType]])
  extends CommandPrimitive {

  private def makeDt(x:Nat):DataType = DataType.substitute(x, `for`=i, `in`=dt)

  override lazy val `type`: CommandType = {
    (n: Nat) -> (dt: DataType) ->
      (out :: acc"[${DepArrayType(n, i, dt)}]") ->
      (body :: t"(${body.t.x}:nat) -> acc[${makeDt(body.t.x)}] -> comm") ->
      comm
  }
  override def eval(s: Store): Store = ???

  override def visitAndRebuild(fun: VisitAndRebuild.Visitor): Phrase[CommandType] = {
    makeParForNat(fun(n), fun(i).asInstanceOf[NatIdentifier], fun(dt), VisitAndRebuild(out, fun), VisitAndRebuild(body, fun))
  }

  override def prettyPrint: String =
    s"(parForNat $n $i $dt ${PrettyPhrasePrinter(out)} ${PrettyPhrasePrinter(body)})"


  override def xmlPrinter: Elem =
    <parForNat n={ToString(n)} dt={ToString(dt)}>
      <output type={ToString(AccType(ArrayType(n, dt)))}>
        {Phrases.xmlPrinter(out)}
      </output>
      <body type={ToString(body.t.x -> (AccType({makeDt(body.t.x)}) -> CommandType()))}>
        {Phrases.xmlPrinter(body)}
      </body>
    </parForNat>.copy(label = {
      val name = this.getClass.getSimpleName
      Character.toLowerCase(name.charAt(0)) + name.substring(1)
    })

  def makeParForNat: (Nat, NatIdentifier, T, Phrase[AccType], Phrase[`(nat)->`[AccType -> CommandType]]) => AbstractParForNat[T]

}
