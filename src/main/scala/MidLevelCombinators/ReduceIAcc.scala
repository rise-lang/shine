package MidLevelCombinators

import Compiling.SubstituteImplementations
import Core.OperationalSemantics._
import Core._
import DSL.typed._
import OpenCL.Core.PrivateMemory

import scala.xml.Elem

final case class ReduceIAcc(n: Nat,
                            dt1: DataType,
                            dt2: DataType,
                            out: Phrase[AccType],
                            f: Phrase[AccType -> (ExpType -> (ExpType -> CommandType))],
                            init: Phrase[ExpType],
                            in: Phrase[ExpType])
  extends MidLevelCombinator {

  override def typeCheck(): Unit = {
    import TypeChecker._
    (n: Nat) -> (dt1: DataType) -> (dt2: DataType) ->
      (out `:` acc"[$dt2]") ->
      (f `:` t"acc[$dt2] -> exp[$dt1] -> exp[$dt2] -> comm") ->
      (init `:` exp"[$dt2]") ->
      (in `:` exp"[$n.$dt1]") ->
      comm
  }

  override def visitAndRebuild(fun: VisitAndRebuild.fun): Phrase[CommandType] = {
    ReduceIAcc(fun(n), fun(dt1), fun(dt2),
      VisitAndRebuild(out, fun),
      VisitAndRebuild(f, fun),
      VisitAndRebuild(init, fun),
      VisitAndRebuild(in, fun))
  }

  override def eval(s: Store): Store = {
    val fE = OperationalSemantics.eval(s, f)(TrinaryFunctionEvaluator)
    val n = in.t match {
      case ExpType(ArrayType(len, _)) => len
    }

    (0 until n.eval).foldLeft(s)((sOld, i) => {
      val comm = fE(out)(in `@` LiteralPhrase(i, IndexType(n)))(init)
      OperationalSemantics.eval(sOld, comm)
    })
  }

  override def prettyPrint =
    s"(reduceIAcc ${PrettyPrinter(out)} ${PrettyPrinter(f)} ${PrettyPrinter(init)} ${PrettyPrinter(in)})"

  override def xmlPrinter: Elem =
    <reduceIAcc n={ToString(n)} dt1={ToString(dt1)} dt2={ToString(dt2)}>
      <output type={ToString(AccType(dt2))}>
        {Core.xmlPrinter(out)}
      </output>
      <f type={ToString(AccType(dt2) -> (ExpType(dt1) -> (ExpType(dt2) -> CommandType())))}>
        {Core.xmlPrinter(f)}
      </f>
      <init type={ToString(ExpType(dt2))}>
        {Core.xmlPrinter(init)}
      </init>
      <input type={ToString(ExpType(ArrayType(n, dt1)))}>
        {Core.xmlPrinter(in)}
      </input>
    </reduceIAcc>

  override def substituteImpl(env: SubstituteImplementations.Environment): Phrase[CommandType] = {
    `new`(init.t.dataType, PrivateMemory, accum => {
      (accum.wr `:=` init) `;`
        `for`(n, i => {
          SubstituteImplementations(f(accum.wr)(in `@` i)(accum.rd), env)
        }) `;`
        (out `:=` accum.rd)
    })
  }
}
