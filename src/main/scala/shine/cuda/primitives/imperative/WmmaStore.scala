package shine.cuda.primitives.imperative

import shine.DPIA.Phrases._
import shine.DPIA.Semantics.OperationalSemantics.Store
import shine.DPIA.Types._
import shine.DPIA.{Nat, Phrases}

import scala.xml.Elem

final case class WmmaStore(m: Nat,
                           n: Nat,
                           k: Nat,
                           dataType: DataType,
                           fragment: Phrase[ExpType],
                           matrixTile: Phrase[AccType]
                          ) extends CommandPrimitive {

  fragment :: ExpType(Fragment(m, n, k, dataType), read)
  val fragArrayType = fragment.t.dataType.asInstanceOf[Fragment].matrixType
  matrixTile :: AccType(fragArrayType)

  override def eval(s: Store): Store = ???

  override def prettyPrint: String = {
    s"wmmaStore(${PrettyPhrasePrinter(fragment)}, ${PrettyPhrasePrinter(matrixTile)})"
  }

  override def xmlPrinter: Elem =
    <wmmaStore>
      <fragment>
        {Phrases.xmlPrinter(fragment)}
      </fragment>
      <matrixTile>
        {Phrases.xmlPrinter(matrixTile)}
      </matrixTile>
    </wmmaStore>

  override def visitAndRebuild(fun: VisitAndRebuild.Visitor): Phrase[CommType] = {
    WmmaStore(fun.nat(m), fun.nat(n), fun.nat(k), fun.data(dataType), VisitAndRebuild(fragment, fun),
      VisitAndRebuild(matrixTile, fun))
  }
}

