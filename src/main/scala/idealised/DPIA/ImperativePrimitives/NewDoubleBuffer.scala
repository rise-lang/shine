package idealised.DPIA.ImperativePrimitives

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
                                 f: Phrase[(ExpType x AccType x CommType x CommType) ->: CommType])
  extends CommandPrimitive {

  override val t: CommType =
    (dt1: DataType) ->: (dt2: DataType) ->: (dt3: DataType) ->: (n: Nat) ->:
      (in :: exp"[$dt1, $read]") ->:
        (out :: acc"[$dt2]") ->:
          (f :: (((varT"[$n.$dt3]" x comm) x comm) ->: comm) ) ->: comm

  override def eval(s: Store): Store = ???

  override def visitAndRebuild(fun: VisitAndRebuild.Visitor): Phrase[CommType] = {
    NewDoubleBuffer(fun.data(dt1), fun.data(dt2), fun.data(dt3), fun.nat(n), VisitAndRebuild(in, fun), VisitAndRebuild(out, fun), VisitAndRebuild(f, fun))
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
