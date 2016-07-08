package MidLevelCombinators

import Compiling.SubstituteImplementations
import Core.OperationalSemantics._
import Core._
import DSL.typed._
import OpenCL.Core.PrivateMemory

import scala.xml.Elem

case class ReduceIExp(n: Nat,
                      dt1: DataType,
                      dt2: DataType,
                      out: Phrase[ExpType -> CommandType],
                      f: Phrase[AccType -> (ExpType -> (ExpType -> CommandType))],
                      init: Phrase[ExpType],
                      in: Phrase[ExpType])
  extends MidLevelCombinator {

  override def typeCheck(): Unit = {
    import TypeChecker._
    (n: Nat) -> (dt1: DataType) -> (dt2: DataType) ->
      (out `:` t"exp[$dt2] -> comm") ->
      (f `:` t"acc[$dt2] -> exp[$dt1] -> exp[$dt2] -> comm") ->
      (init `:` exp"[$dt2]") ->
      (in `:` exp"[$n.$dt1]") ->
      comm
  }

  override def visitAndRebuild(fun: VisitAndRebuild.fun): Phrase[CommandType] = {
    ReduceIExp(fun(n), fun(dt1), fun(dt2),
      VisitAndRebuild(out, fun),
      VisitAndRebuild(f, fun),
      VisitAndRebuild(init, fun),
      VisitAndRebuild(in, fun))
  }

  override def eval(s: Store): Store = {
    val outE = OperationalSemantics.eval(s, out)
    OperationalSemantics.eval(s, `new`(init.t.dataType, PrivateMemory, accum => {
      ReduceIAcc(n, dt1, dt2, π2(accum), f, init, in) `;`
        outE(π1(accum))
    }))
  }

  override def prettyPrint =
    s"(reduceIExp ${PrettyPrinter(out)} ${PrettyPrinter(f)} ${PrettyPrinter(init)} ${PrettyPrinter(in)})"

  override def xmlPrinter: Elem =
    <reduceIExp n={ToString(n)} dt1={ToString(dt1)} dt2={ToString(dt2)}>
      <output type={ToString(ExpType(dt2) -> CommandType())}>
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
    </reduceIExp>

  override def substituteImpl(env: SubstituteImplementations.Environment): Phrase[CommandType] = {
    `new`(init.t.dataType, PrivateMemory, accum => {
      (accum.wr `:=` init) `;`
        `for`(n, i => {
          SubstituteImplementations( f(accum.wr)(in `@` i)(accum.rd), env )
        }) `;`
        out(accum.rd)
    } )
  }

}
