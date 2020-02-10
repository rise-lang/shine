package shine.OpenCL.ImperativePrimitives

import shine.DPIA.Phrases._
import shine.DPIA.Semantics.OperationalSemantics._
import shine.DPIA.Types._
import shine.DPIA.Types.DataType._
import shine.DPIA._

import scala.xml.Elem

final case class OpenCLNewDoubleBuffer(a: AddressSpace,
                                       dt1: DataType,
                                       dt2: DataType,
                                       dt3: DataType,
                                       n: Nat,
                                       in: Phrase[ExpType],
                                       out: Phrase[AccType],
                                       f: Phrase[(ExpType x AccType x CommType x CommType) ->: CommType])
  extends CommandPrimitive {

  in :: expT(dt1, read)
  out :: accT(dt2)
  f :: (((varT(n`.`dt3) x comm) x comm) ->: comm)

  override def eval(s: Store): Store = ???

  override def visitAndRebuild(fun: VisitAndRebuild.Visitor): Phrase[CommType] = {
    OpenCLNewDoubleBuffer(fun.addressSpace(a), fun.data(dt1), fun.data(dt2), fun.data(dt3), fun.nat(n), VisitAndRebuild(in, fun), VisitAndRebuild(out, fun), VisitAndRebuild(f, fun))
  }

  override def prettyPrint: String = s"(newDoubleBuffer $a ${PrettyPhrasePrinter(f)})"

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
