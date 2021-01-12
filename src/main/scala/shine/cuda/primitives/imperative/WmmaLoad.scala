package shine.cuda.primitives.imperative

import shine.DPIA.Phrases._
import shine.DPIA.Semantics.OperationalSemantics.Store
import shine.DPIA.Types._
import shine.DPIA.{Nat, Phrases}

import scala.xml.Elem

final case class WmmaLoad(m: Nat,
                          n: Nat,
                          k: Nat,
                          dataType: DataType,
                          matrixTile: Phrase[ExpType],
                          fragment: Phrase[AccType]
                         ) extends CommandPrimitive {

  val layout = fragment.t.dataType.asInstanceOf[Fragment].layout
  if (fragment.t.dataType != Fragment(m, n, k, dataType, FragmentType.AMatrix, layout)
    && fragment.t.dataType != Fragment(m, n, k, dataType, FragmentType.BMatrix, layout)
    && fragment.t.dataType != Fragment(m, n, k, dataType))
    throw new TypeException(s"Type error: found ${fragment.t.dataType} expected" +
      s"${Fragment(m, n, k, dataType, FragmentType.AMatrix, layout)} or" +
      s"${Fragment(m, n, k, dataType, FragmentType.BMatrix, layout)} or ${Fragment(m, n, k, dataType)}")

  matrixTile :: ExpType(fragment.t.dataType.asInstanceOf[Fragment].matrixType, read)

  override def eval(s: Store): Store = ???

  override def prettyPrint: String = {
    s"wmmaLoad(${PrettyPhrasePrinter(matrixTile)}, ${PrettyPhrasePrinter(fragment)})"
  }

  override def xmlPrinter: Elem =
    <wmmaLoad>
      <matrixTile>
        {Phrases.xmlPrinter(matrixTile)}
      </matrixTile>
      <fragment>
        {Phrases.xmlPrinter(fragment)}
      </fragment>
    </wmmaLoad>

  override def visitAndRebuild(fun: VisitAndRebuild.Visitor): Phrase[CommType] = {
    WmmaLoad(fun.nat(m), fun.nat(n), fun.nat(k), fun.data(dataType),
      VisitAndRebuild(matrixTile, fun), VisitAndRebuild(fragment, fun))
  }
}
