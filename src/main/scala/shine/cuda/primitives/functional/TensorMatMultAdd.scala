package shine.cuda.primitives.functional

import shine.DPIA.Compilation.TranslationContext
import shine.DPIA.Compilation.TranslationToImperative.con
import shine.DPIA.DSL._
import shine.DPIA.Phrases._
import shine.DPIA.Semantics.OperationalSemantics.{Data, Store}
import shine.DPIA.Types._
import shine.DPIA.{->:, Nat, Phrases, VarType}
import shine.OpenCL.ImperativePrimitives.OpenCLNew
import shine.cuda.ast.Wmma
import shine.cuda.primitives.imperative.WmmaMMA

import scala.xml.Elem

final case class TensorMatMultAdd(m: Nat,
                                  n: Nat,
                                  k: Nat,
                                  layoutA: MatrixLayout,
                                  layoutB: MatrixLayout,
                                  dataType: DataType,
                                  dataTypeAcc: DataType,
                                  aMatrix: Phrase[ExpType],
                                  bMatrix: Phrase[ExpType],
                                  cMatrix: Phrase[ExpType]) extends ExpPrimitive {
  Wmma.checkDimensionsAndTypes(m, n, k, dataType, dataTypeAcc)

  aMatrix :: ExpType(Fragment(m, n, k, dataType, FragmentType.AMatrix, layoutA), read)
  bMatrix :: ExpType(Fragment(m, n, k, dataType, FragmentType.BMatrix, layoutB), read)
  cMatrix :: ExpType(Fragment(m, n, k, dataTypeAcc), read)

  override val t: ExpType = ExpType(Fragment(m, n, k, dataTypeAcc), write)

  override def visitAndRebuild(f: VisitAndRebuild.Visitor): Phrase[ExpType] = {
    TensorMatMultAdd(f.nat(m), f.nat(n), f.nat(k), layoutA, layoutB,
      f.data(dataType), f.data(dataTypeAcc),
      VisitAndRebuild(aMatrix, f), VisitAndRebuild(bMatrix, f), VisitAndRebuild(cMatrix, f))
  }

  override def acceptorTranslation(A: Phrase[AccType])
                                  (implicit context: TranslationContext): Phrase[CommType] = {
    con(aMatrix)(λ(ExpType(Fragment(m, n, k, dataType, FragmentType.AMatrix, layoutA), read))(aMatrix =>
      con(bMatrix)(λ(ExpType(Fragment(m, n, k, dataType, FragmentType.BMatrix, layoutB), read))(bMatrix =>
        con(cMatrix)(λ(ExpType(Fragment(m, n, k, dataTypeAcc), read))(cMatrix =>
            WmmaMMA(m, n, k, layoutA, layoutB, dataType, dataTypeAcc, aMatrix, bMatrix, cMatrix, A)))))))
  }

  override def continuationTranslation(C: Phrase[ExpType ->: CommType])
                                      (implicit context: TranslationContext): Phrase[CommType] = ???

  override def eval(s: Store): Data = ???

  override def prettyPrint: String =
    s"TensorMatMultAdd(${PrettyPhrasePrinter(aMatrix)}, ${PrettyPhrasePrinter(bMatrix)}, ${PrettyPhrasePrinter {cMatrix}})"

  override def xmlPrinter: Elem =
    <wmmaMM n={ToString(n)} m={ToString(m)} k={ToString(k)} dt1={ToString(dataType)} dt2={ToString(dataTypeAcc)}>
      <aMatrix>
        {Phrases.xmlPrinter(aMatrix)}
      </aMatrix>
      <bMatrix>
        {Phrases.xmlPrinter(bMatrix)}
      </bMatrix>
      <cMatrix>
        {Phrases.xmlPrinter(cMatrix)}
      </cMatrix>
    </wmmaMM>
}
