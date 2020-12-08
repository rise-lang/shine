package shine.cuda.primitives.imperative

import shine.DPIA.DSL.{`new` => _}
import shine.DPIA.Phrases._
import shine.DPIA.Semantics.OperationalSemantics.{Store, _}
import shine.DPIA.Types._
import shine.DPIA.{Phrases, _}

import scala.xml.Elem

final case class GlobalToSharedAcc(dt: DataType,
                                   pipe: Phrase[ExpType],
                                   outputShared: Phrase[AccType]
                                  ) extends AccPrimitive {

  pipe :: expT(pipeline, read)
  outputShared :: accT(dt)
  override val t: AccType = accT(dt)

  override def visitAndRebuild(fun: VisitAndRebuild.Visitor): Phrase[AccType] = {
    GlobalToSharedAcc(fun.data(dt), VisitAndRebuild(pipe, fun), VisitAndRebuild(outputShared, fun))
  }

  override def prettyPrint: String =
    s"(GlobalToSharedAcc $pipe, ${PrettyPhrasePrinter(outputShared)})"

  override def xmlPrinter: Elem =
    <ToSharedAsyncAcc pipeline={ToString(pipe)}>
      {Phrases.xmlPrinter(outputShared)}
    </ToSharedAsyncAcc>

  override def eval(s: Store): AccIdentifier = ???
}


