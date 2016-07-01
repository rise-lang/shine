package CommandPatterns

import Compiling.SubstituteImplementations
import Core.OperationalSemantics._
import Core._
import DSL._
import apart.arithmetic.ArithExpr

import scala.xml.Elem

abstract class AbstractMapI(n: ArithExpr,
                            dt1: DataType,
                            dt2: DataType,
                            out: Phrase[AccType],
                            f: Phrase[AccType -> (ExpType -> CommandType)],
                            in: Phrase[ExpType])
  extends IntermediateCommandPattern {

  override def typeCheck: Unit = {
    import TypeChecker._
    out checkType acc"[$n.$dt2]"
    f checkType t"acc[$dt2] -> exp[$dt1] -> comm"
    in checkType exp"[$n.$dt1]"
  }

  override def eval(s: Store): Store = {
    val fE = OperationalSemantics.eval(s, f)(BinaryFunctionEvaluator)
    val n = in.t match {
      case ExpType(ArrayType(len, _)) => len
    }

    (0 until n.eval).foldLeft(s)((sOld, i) => {
      val comm = fE(out `@` LiteralPhrase(i))(in `@` LiteralPhrase(i))
      OperationalSemantics.eval(sOld, comm)
    })
  }

  override def visitAndRebuild(fun: VisitAndRebuild.fun): Phrase[CommandType] = {
    makeMapI(fun(n), fun(dt1), fun(dt2),
      VisitAndRebuild(out, fun),
      VisitAndRebuild(f, fun),
      VisitAndRebuild(in, fun))
  }

  def makeMapI: (ArithExpr, DataType, DataType, Phrase[AccType], Phrase[AccType -> (ExpType -> CommandType)], Phrase[ExpType]) => AbstractMapI

  override def prettyPrint =
    s"(${this.getClass.getSimpleName} ${PrettyPrinter(out)} ${PrettyPrinter(f)} ${PrettyPrinter(in)})"

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

case class MapI(n: ArithExpr,
                dt1: DataType,
                dt2: DataType,
                out: Phrase[AccType],
                f: Phrase[AccType -> (ExpType -> CommandType)],
                in: Phrase[ExpType]) extends AbstractMapI(n, dt1, dt2, out, f, in) {

  override def makeMapI = MapI

  override def substituteImpl(env: SubstituteImplementations.Environment): Phrase[CommandType] = {
    `parFor`(n, dt2, out, i => o => {
      SubstituteImplementations(f(o)(in `@` i), env)
    })
  }

}
