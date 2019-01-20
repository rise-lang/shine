package idealised.DPIA.IntermediatePrimitives

import idealised.DPIA.Compilation.SubstituteImplementations
import idealised.DPIA.DSL._
import idealised.DPIA.ImperativePrimitives.{For, ForNat}
import idealised.DPIA.Phrases._
import idealised.DPIA.Semantics.OperationalSemantics._
import idealised.DPIA.Types._
import idealised.DPIA._
import lift.arithmetic.{NamedVar, RangeAdd}

import scala.xml.Elem

abstract class AbstractDepMapI(n: Nat,
                               i1: NatIdentifier, dt1: DataType,
                               i2: NatIdentifier, dt2: DataType,
                               f: Phrase[`(nat)->`[ExpType -> (AccType -> CommandType)]],
                               in: Phrase[ExpType],
                               out: Phrase[AccType])
  extends CommandPrimitive with Intermediate[CommandType] {

  protected def makeDt1(x:Nat):DataType = DataType.substitute(x, `for`=i1, `in`=dt1)
  protected def makeDt2(x:Nat):DataType = DataType.substitute(x, `for`=i2, `in`=dt2)

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
    makeMapI(fun(n), fun(i1).asInstanceOf[NatIdentifier], fun(dt1), fun(i2).asInstanceOf[NatIdentifier], fun(dt2),
      VisitAndRebuild(f, fun),
      VisitAndRebuild(in, fun),
      VisitAndRebuild(out, fun))
  }

  def makeMapI: (Nat, NatIdentifier, DataType, NatIdentifier, DataType, Phrase[`(nat)->`[ExpType -> (AccType -> CommandType)]], Phrase[ExpType], Phrase[AccType]) => AbstractDepMapI

  override def prettyPrint =
    s"(${this.getClass.getSimpleName} ${PrettyPhrasePrinter(out)} ${PrettyPhrasePrinter(f)} ${PrettyPhrasePrinter(in)})"

  override def xmlPrinter: Elem =
    <depMapI n={ToString(n)} i1={ToString(i1)} dt1={ToString(dt1)} i2={ToString(i2)} dt2={ToString(dt2)}>
      <output type={ToString(out.t)}>
        {Phrases.xmlPrinter(out)}
      </output>
      <f type={ToString(f.t)}>
        {Phrases.xmlPrinter(f)}
      </f>
      <input type={ToString(in.t)}>
        {Phrases.xmlPrinter(in)}
      </input>
    </depMapI>.copy(label = {
      val name = this.getClass.getSimpleName
      Character.toLowerCase(name.charAt(0)) + name.substring(1)
    })
}

final case class DepMapI(n: Nat,
                         i1: NatIdentifier, dt1: DataType,
                         i2: NatIdentifier, dt2: DataType,
                         f: Phrase[`(nat)->`[ExpType -> (AccType -> CommandType)]],
                         in: Phrase[ExpType],
                         out: Phrase[AccType])
  extends AbstractDepMapI(n, i1, dt1, i2, dt2, f, in, out) {

  override def makeMapI = DepMapI

  override def substituteImpl(env: SubstituteImplementations.Environment): Phrase[CommandType] = {
    ForNat(n, _Λ_( i =>
      SubstituteImplementations(f(i)(in `@d` i)(out `@d` i), env)
      , RangeAdd(0, n, 1))
    )
  }
}
