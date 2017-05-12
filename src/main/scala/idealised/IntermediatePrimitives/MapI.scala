package idealised.IntermediatePrimitives

import idealised._
import idealised.Core._
import idealised.Core.OperationalSemantics._
import idealised.Compiling.SubstituteImplementations
import idealised.DSL.typed._

import scala.xml.Elem

abstract class AbstractMapI(n: Nat,
                            dt1: DataType,
                            dt2: DataType,
                            f: Phrase[ExpType -> (AccType -> CommandType)],
                            in: Phrase[ExpType],
                            out: Phrase[AccType])
  extends CommandPrimitive with Intermediate[CommandType] {

  override def typeCheck(): Unit = {
    import TypeChecker._
    (n: Nat) -> (dt1: DataType) -> (dt2: DataType) ->
      (f :: t"exp[$dt1] -> acc[$dt2] -> comm") ->
      (in :: exp"[$n.$dt1]") ->
      (out :: acc"[$n.$dt2]") ->
      comm
  }

  override def eval(s: Store): Store = {
    val fE = OperationalSemantics.eval(s, f)(BinaryFunctionEvaluator)
    val n = in.t match {
      case ExpType(ArrayType(len, _)) => len
    }

    (0 until n.eval).foldLeft(s)((sOld, i) => {
      val comm = fE(in `@` Literal(i, IndexType(n)))(out `@` Literal(i, IndexType(n)))
      OperationalSemantics.eval(sOld, comm)
    })
  }

  override def visitAndRebuild(fun: VisitAndRebuild.Visitor): Phrase[CommandType] = {
    makeMapI(fun(n), fun(dt1), fun(dt2),
      VisitAndRebuild(f, fun),
      VisitAndRebuild(in, fun),
      VisitAndRebuild(out, fun))
  }

  def makeMapI: (Nat, DataType, DataType, Phrase[ExpType -> (AccType -> CommandType)], Phrase[ExpType], Phrase[AccType]) => AbstractMapI

  override def prettyPrint =
    s"(${this.getClass.getSimpleName} ${PrettyPhrasePrinter(out)} ${PrettyPhrasePrinter(f)} ${PrettyPhrasePrinter(in)})"

  override def xmlPrinter: Elem =
    <map n={ToString(n)} dt1={ToString(dt1)} dt2={ToString(dt2)}>
      <output type={ToString(AccType(ArrayType(n, dt2)))}>
        {Core.xmlPrinter(out)}
      </output>
      <f type={ToString(AccType(dt2) -> (ExpType(dt1) -> CommandType()))}>
        {Core.xmlPrinter(f)}
      </f>
      <input type={ToString(ExpType(ArrayType(n, dt1)))}>
        {Core.xmlPrinter(in)}
      </input>
    </map>.copy(label = {
      val name = this.getClass.getSimpleName
      Character.toLowerCase(name.charAt(0)) + name.substring(1)
    })
}

final case class MapI(n: Nat,
                      dt1: DataType,
                      dt2: DataType,
                      f: Phrase[ExpType -> (AccType -> CommandType)],
                      in: Phrase[ExpType],
                      out: Phrase[AccType])
  extends AbstractMapI(n, dt1, dt2, f, in, out) {

  override def makeMapI = MapI

  override def substituteImpl(env: SubstituteImplementations.Environment): Phrase[CommandType] = {
    `parFor`(n, dt2, out, i => a =>
      SubstituteImplementations(f(in `@` i)(a), env)
    )
  }

}
