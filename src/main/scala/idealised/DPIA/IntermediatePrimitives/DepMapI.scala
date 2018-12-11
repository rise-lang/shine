package idealised.DPIA.IntermediatePrimitives

import idealised.DPIA.Compilation.SubstituteImplementations
import idealised.DPIA.DSL._
import idealised.DPIA.Phrases._
import idealised.DPIA.Semantics.OperationalSemantics
import idealised.DPIA.Semantics.OperationalSemantics._
import idealised.DPIA.Types._
import idealised.DPIA._

import scala.xml.Elem

abstract class AbstractDepMapI(n: Nat,
                               i1: Nat, dt1: DataType,
                               i2: Nat, dt2: DataType,
                               f: Phrase[`(nat)->`[ExpType -> (AccType -> CommandType)]],
                               in: Phrase[ExpType],
                               out: Phrase[AccType])
  extends CommandPrimitive with Intermediate[CommandType] {

  private def makeDt1(x:Nat):DataType = DataType.substitute(x, `for`=i1, `in`=dt1)
  private def makeDt2(x:Nat):DataType = DataType.substitute(x, `for`=i2, `in`=dt2)

  override lazy val `type`: CommandType = {
    val k = f.t.x
    (n: Nat) -> (i1: Nat) -> (dt1: DataType) -> (i2: Nat) -> (dt2: DataType) ->
      (f :: t"($k:nat) -> exp[${makeDt1(k)}] -> acc[${makeDt2(k)}] -> comm") ->
      (in :: exp"[${DepArrayType(n, makeDt1)}]") ->
      (out :: acc"[${DepArrayType(n, makeDt2)}]") ->
      comm
  }

  override def eval(s: Store): Store = {
//    val fE = OperationalSemantics.eval(s, f)(BinaryFunctionEvaluator)
//    val n = in.t match {
//      case ExpType(ArrayType(len, _)) => len
//    }
//
//    (0 until n.eval).foldLeft(s)((sOld, i) => {
//      val comm = fE(in `@` Literal(i))(out `@` Literal(i))
//      OperationalSemantics.eval(sOld, comm)
//    })
    ???
  }

  override def visitAndRebuild(fun: VisitAndRebuild.Visitor): Phrase[CommandType] = {
    makeMapI(fun(n), fun(i1), fun(dt1), fun(i2), fun(dt2),
      VisitAndRebuild(f, fun),
      VisitAndRebuild(in, fun),
      VisitAndRebuild(out, fun))
  }

  def makeMapI: (Nat, Nat, DataType, Nat, DataType, Phrase[`(nat)->`[ExpType -> (AccType -> CommandType)]], Phrase[ExpType], Phrase[AccType]) => AbstractDepMapI

  override def prettyPrint =
    s"(${this.getClass.getSimpleName} ${PrettyPhrasePrinter(out)} ${PrettyPhrasePrinter(f)} ${PrettyPhrasePrinter(in)})"

  override def xmlPrinter: Elem =
    <depMapI n={ToString(n)} dt1={ToString(dt1)} dt2={ToString(dt2)}>
      <output type={ToString(AccType(ArrayType(n, dt2)))}>
        {Phrases.xmlPrinter(out)}
      </output>
      <f type={ToString(AccType(dt2) -> (ExpType(dt1) -> CommandType()))}>
        {Phrases.xmlPrinter(f)}
      </f>
      <input type={ToString(ExpType(ArrayType(n, dt1)))}>
        {Phrases.xmlPrinter(in)}
      </input>
    </depMapI>.copy(label = {
      val name = this.getClass.getSimpleName
      Character.toLowerCase(name.charAt(0)) + name.substring(1)
    })
}

final case class DepMapI(n: Nat,
                         i1: Nat, dt1: DataType,
                         i2: Nat, dt2: DataType,
                         f: Phrase[`(nat)->`[ExpType -> (AccType -> CommandType)]],
                         in: Phrase[ExpType],
                         out: Phrase[AccType])
  extends AbstractDepMapI(n, i1, dt1, i2, dt2, f, in, out) {

  override def makeMapI = DepMapI

  override def substituteImpl(env: SubstituteImplementations.Environment): Phrase[CommandType] = {
//    `for`(n, i =>
//      SubstituteImplementations(f(in `@` i)(out `@` i), env)
//    )
    ???
  }

}
