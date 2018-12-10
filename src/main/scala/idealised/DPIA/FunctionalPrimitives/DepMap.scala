package idealised.DPIA.FunctionalPrimitives

import idealised.DPIA.IntermediatePrimitives.AbstractMapI
import idealised.DPIA.Phrases._
import idealised.DPIA.Semantics.OperationalSemantics._
import idealised.DPIA.Types._
import idealised.DPIA._

import scala.xml.Elem


abstract class AbstractDepMap(n: Nat,
                              i1: Nat, dt1: DataType,
                              i2: Nat, dt2: DataType,
                              f: Phrase[`(nat)->`[ExpType -> ExpType]],
                              array: Phrase[ExpType])
  extends ExpPrimitive {


  def makeMap: (Nat, Nat, DataType, Nat, DataType, Phrase[`(nat)->`[ExpType -> ExpType]], Phrase[ExpType]) => AbstractDepMap

  def makeMapI: (Nat, DataType, DataType,
    Phrase[ExpType -> (AccType -> CommandType)], Phrase[ExpType], Phrase[AccType]) => AbstractMapI


  override val `type`: ExpType = {
    f match {
      case NatDependentLambda(k, _) =>
        (n: Nat) -> (i1: Nat) -> (dt1: DataType) -> (i2: Nat) -> (dt2: DataType) ->
          (f :: t"($k : nat) -> exp[${ DataType.substitute(k, `for`=i1, in=dt1) }] -> exp[${ DataType.substitute(k, `for`=i2, in=dt2) }]")
            (array :: exp"[${DepArrayType(n, DataType.substitute(_, `for`=i1, in=dt1))}]") ->
              exp"[${DepArrayType(n, DataType.substitute(_, `for`=i2, in=dt2))}]"

      case _ => throw new Exception("This should not happen")
    }
  }

  override def visitAndRebuild(fun: VisitAndRebuild.Visitor): Phrase[ExpType] = {
    makeMap(fun(n), fun(i1), fun(dt1), fun(i2), fun(dt2), VisitAndRebuild(f, fun), VisitAndRebuild(array, fun))
  }

  override def eval(s: Store): Data = ???

  override def acceptorTranslation(A: Phrase[AccType]): Phrase[CommandType] = { ???
//    import RewriteToImperative._
//
//    con(array)(λ(exp"[$n.$dt1]")(x =>
//      makeMapI(n, dt1, dt2, λ(exp"[$dt1]")(x => λ(acc"[$dt2]")(o => acc(f(x))(o))), x, A)))
  }

  override def continuationTranslation(C: Phrase[ExpType -> CommandType]): Phrase[CommandType] = ???

  override def prettyPrint: String =
    s"${this.getClass.getSimpleName} (${PrettyPhrasePrinter(f)}) (${PrettyPhrasePrinter(array)})"

  override def xmlPrinter: Elem = {
    val k = f match {
      case NatDependentLambda(k_, _) => k_
      case _ => throw new Exception("This should not happen")
    }
    <map n={ToString(n)} i1={ToString(i1)} dt1={ToString(dt1)} i2={ToString(i2)} dt2={ToString(dt2)}>
      <f type={ToString(k -> (ExpType(DataType.substitute(k, `for` = i1, in = dt1)) -> ExpType(DataType.substitute(k, `for` = i2, in = dt2))))}>
        {Phrases.xmlPrinter(f)}
      </f>
      <input type={ToString(ExpType(DepArrayType(n, DataType.substitute(_, `for` = i1, in = dt1))))}>
        {Phrases.xmlPrinter(array)}
      </input>
    </map>.copy(label = {
      val name = this.getClass.getSimpleName
      Character.toLowerCase(name.charAt(0)) + name.substring(1)
    })
  }
}

final case class DepMap(n: Nat,
                        i1: Nat, dt1: DataType,
                        i2: Nat, dt2: DataType,
                     f: Phrase[`(nat)->`[ExpType -> ExpType]],
                     array: Phrase[ExpType])
  extends AbstractDepMap(n, i1, dt1, i2, dt2, f, array) {
  override def makeMap = DepMap

  override def makeMapI = ???
}