package shine.cuda.primitives.imperative

import shine.DPIA.Phrases._
import shine.DPIA.Semantics.OperationalSemantics.Store
import shine.DPIA.Types._
import shine.DPIA.{Nat, Phrases}

import scala.xml.Elem

final case class WmmaLoad(ldm: Nat,
                          m: Nat,
                          n: Nat,
                          k: Nat,
                          dataType: DataType,
                          layout: MatrixLayout,
                          matrixTile: Phrase[ExpType],
                          fragment: Phrase[AccType]
                         ) extends CommandPrimitive {

  if (fragment.t.dataType != WmmaAMatrix(m, n, k, dataType, layout)
    && fragment.t.dataType != WmmaBMatrix(m, n, k, dataType, layout)
    && fragment.t.dataType != WmmaAccumulator(m, n, k, dataType))
    throw new TypeException(s"Type error: found ${fragment.t.dataType} expected ${WmmaAMatrix(m, n, k, dataType, layout)}" +
        s"or ${WmmaBMatrix(m, n, k, dataType, layout)} or ${WmmaAccumulator(m, n, k, dataType)}")

  val fragArrayType = fragment.t.dataType.asInstanceOf[WmmaFragment].arrayType
  matrixTile :: ExpType(fragArrayType, read)

  override def eval(s: Store): Store = ???

  override def prettyPrint: String = {
    s"wmmaLoad($ldm, ${PrettyPhrasePrinter(matrixTile)}, ${PrettyPhrasePrinter(fragment)})"
  }

  override def xmlPrinter: Elem =
    <wmmaLoad ldm={ToString(ldm)}>
      <matrixTile>
        {Phrases.xmlPrinter(matrixTile)}
      </matrixTile>
      <fragment>
        {Phrases.xmlPrinter(fragment)}
      </fragment>
    </wmmaLoad>

  override def visitAndRebuild(fun: VisitAndRebuild.Visitor): Phrase[CommType] = {
    WmmaLoad(fun.nat(ldm), fun.nat(m), fun.nat(n), fun.nat(k), fun.data(dataType), layout,
      VisitAndRebuild(matrixTile, fun), VisitAndRebuild(fragment, fun))
  }
}
