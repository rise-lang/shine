package shine.DPIA.ImperativePrimitives

import shine.DPIA.Phrases._
import shine.DPIA.Semantics.OperationalSemantics.Store
import shine.DPIA.Types._
import shine.DPIA._

import scala.xml.Elem

final case class DMatchI(x: NatIdentifier,
                         elemT: DataType,
                         outT: DataType,
                         f: Phrase[`(nat)->:`[ExpType ->: CommType]],
                         input: Phrase[ExpType]) extends CommandPrimitive {
  override val t: CommType = comm

  override def eval(s: Store): Store = ???

  override def prettyPrint: String = s"${this.getClass.getSimpleName} (${PrettyPhrasePrinter(f)}) (${PrettyPhrasePrinter(input)})"

  override def xmlPrinter: Elem = <DMatchI x={ToString(x)} elemT={ToString(elemT)} outT={ToString(outT)}>
    <f type={ToString(f.t.x ->: ExpType(elemT, read) ->: ExpType(outT, write))}>
      {Phrases.xmlPrinter(f)}
    </f>
    <input type={ToString(ExpType(DepPairType(x, elemT), read))}>
      {Phrases.xmlPrinter(input)}
    </input>
  </DMatchI>.copy(label = {
    val name = this.getClass.getSimpleName
    s"${Character.toLowerCase(name.charAt(0))}${name.substring(1)}"
  })

  override def visitAndRebuild(v: VisitAndRebuild.Visitor): Phrase[CommType] =
    DMatchI(v.nat(x), v.data(elemT), v.data(outT), VisitAndRebuild(f, v), VisitAndRebuild(input, v))
}
