package idealised.DPIA.FunctionalPrimitives

import idealised.DPIA.IntermediatePrimitives.AbstractMapI
import idealised.DPIA.Phrases._
import idealised.DPIA.Semantics.OperationalSemantics._
import idealised.DPIA.Types._
import idealised.DPIA._

import scala.xml.Elem


abstract class AbstractDepMap(n: Nat,
                              dt1: DataType,
                              dt2: DataType,
                              df: Phrase[`(nat)->`[ExpType -> ExpType]],
                              array: Phrase[ExpType])
  extends ExpPrimitive {


  def makeMap: (Nat, DataType, DataType, Phrase[`(nat)->`[ExpType -> ExpType]], Phrase[ExpType]) => AbstractDepMap

  def makeMapI: (Nat, DataType, DataType,
    Phrase[ExpType -> (AccType -> CommandType)], Phrase[ExpType], Phrase[AccType]) => AbstractMapI


  override val `type`: ExpType = {
    df match {
      case NatDependentLambda(i, _) =>
        //TODO: Do I need to substitute i in the result?
      (n: Nat) -> (dt1: DataType) -> (dt2: DataType) ->
      (df :: t"($i : nat) -> exp[$dt1] -> exp[$dt2]") ->
      (array :: exp"[$n.$dt1]") -> exp"[($i : nat) -> $n.$dt2]"
    }
  }

  override def visitAndRebuild(fun: VisitAndRebuild.Visitor): Phrase[ExpType] = {
    makeMap(fun(n), fun(dt1), fun(dt2), VisitAndRebuild(df, fun), VisitAndRebuild(array, fun))
  }

  override def eval(s: Store): Data = ???

  override def acceptorTranslation(A: Phrase[AccType]): Phrase[CommandType] = ???

  override def continuationTranslation(C: Phrase[ExpType -> CommandType]): Phrase[CommandType] = ???

  override def prettyPrint: String =
    s"${this.getClass.getSimpleName} (${PrettyPhrasePrinter(df)}) (${PrettyPhrasePrinter(array)})"

  override def xmlPrinter: Elem =
    <map n={ToString(n)} dt1={ToString(dt1)} dt2={ToString(dt2)}>
      <f type={ToString(ExpType(dt1) -> ExpType(dt2))}>
        {Phrases.xmlPrinter(df)}
      </f>
      <input type={ToString(ExpType(ArrayType(n, dt1)))}>
        {Phrases.xmlPrinter(array)}
      </input>
    </map>.copy(label = {
      val name = this.getClass.getSimpleName
      Character.toLowerCase(name.charAt(0)) + name.substring(1)
    })
}

final case class DepMap(n: Nat,
                     dt1: DataType,
                     dt2: DataType,
                     f: Phrase[`(nat)->`[ExpType -> ExpType]],
                     array: Phrase[ExpType])
  extends AbstractDepMap(n, dt1, dt2, f, array) {
  override def makeMap = DepMap

  override def makeMapI = ???
}