package shine.cuda.primitives.functional

import shine.DPIA.Compilation.TranslationContext
import shine.DPIA.Compilation.TranslationToImperative.con
import shine.DPIA.DSL._
import shine.DPIA.Phrases._
import shine.DPIA.Semantics.OperationalSemantics
import shine.DPIA.Semantics.OperationalSemantics.Store
import shine.DPIA.Types._
import shine.DPIA.{->:, Nat, Phrases, VarType}
import shine.OpenCL.ImperativePrimitives.OpenCLNew
import shine.cuda.primitives.imperative.WmmaLoad

import scala.xml.Elem

case class ToFragment(ldm: Nat,
                      m: Nat,
                      n: Nat,
                      k: Nat,
                      dataType: DataType,
                      layout: MatrixLayout,
                      fragType: WmmaFragment,
                      matrix: Phrase[ExpType]) extends ExpPrimitive {

  matrix :: ExpType(fragType.arrayType, read)
  override val t: ExpType = ExpType(fragType, read)

  if (fragType != WmmaAMatrix(m, n, k, dataType, layout)
    && fragType != WmmaBMatrix(m, n, k, dataType, layout)
    && fragType != WmmaAccumulator(m, n, k, dataType))
    throw new TypeException(s"Type error: found ${fragType} expected ${WmmaAMatrix(m, n, k, dataType, layout)}" +
      s"or ${WmmaBMatrix(m, n, k, dataType, layout)} or ${WmmaAccumulator(m, n, k, dataType)}")

  override def visitAndRebuild(f: VisitAndRebuild.Visitor): Phrase[ExpType] = {
    ToFragment(f.nat(ldm), f.nat(m), f.nat(n), f.nat(k), f.data(dataType), layout, f.data(fragType),
      VisitAndRebuild(matrix, f))
  }

  override def acceptorTranslation(A: Phrase[AccType])
                                  (implicit context: TranslationContext): Phrase[CommType] = {
    con(matrix)(λ(ExpType(fragType.arrayType, read))(matrix =>
      WmmaLoad(ldm, m, n, k, dataType, layout, matrix, A)))
  }

  override def continuationTranslation(C: Phrase[ExpType ->: CommType])
                                      (implicit context: TranslationContext): Phrase[CommType] = {
    OpenCLNew(AddressSpace.Private, fragType,
      λ(VarType(fragType))(fragment =>
        acceptorTranslation(fragment.wr) `;`
          C(fragment.rd)))
  }

  override def eval(s: Store): OperationalSemantics.Data = ???

  override def prettyPrint: String = s"${this.getClass.getSimpleName}($ldm, $m, $n, $k, ${PrettyPhrasePrinter(matrix)})"

  override def xmlPrinter: Elem =
    <ToFragment ldm={ToString(ldm)} m={ToString(m)} n={ToString(n)} k={ToString(k)}>
      <matrixTile>
        {Phrases.xmlPrinter(matrix)}
      </matrixTile>
    </ToFragment>
}
