package idealised.MidLevelPrimitives

import idealised._
import idealised.Core._
import idealised.Core.OperationalSemantics._
import idealised.Compiling.SubstituteImplementations
import idealised.DSL.typed._

import scala.xml.Elem

final case class ReduceIExp(n: Nat,
                            dt1: DataType,
                            dt2: DataType,
                            out: Phrase[ExpType -> CommandType],
                            f: Phrase[AccType -> (ExpType -> (ExpType -> CommandType))],
                            init: Phrase[ExpType],
                            in: Phrase[ExpType])
  extends CommandPrimitive with Intermediate[CommandType] {

  override def typeCheck(): Unit = {
    import TypeChecker._
    (n: Nat) -> (dt1: DataType) -> (dt2: DataType) ->
      (out :: t"exp[$dt2] -> comm") ->
      (f :: t"acc[$dt2] -> exp[$dt1] -> exp[$dt2] -> comm") ->
      (init :: exp"[$dt2]") ->
      (in :: exp"[$n.$dt1]") ->
      comm
  }

  override def visitAndRebuild(fun: VisitAndRebuild.Visitor): Phrase[CommandType] = {
    ReduceIExp(fun(n), fun(dt1), fun(dt2),
      VisitAndRebuild(out, fun),
      VisitAndRebuild(f, fun),
      VisitAndRebuild(init, fun),
      VisitAndRebuild(in, fun))
  }

  override def eval(s: Store): Store = {
    val outE = OperationalSemantics.eval(s, out)
    OperationalSemantics.eval(s, `new`(init.t.dataType, OpenCL.PrivateMemory, accum => {
      ReduceIAcc(n, dt1, dt2, π2(accum), f, init, in) `;`
        outE(π1(accum))
    }))
  }

  override def prettyPrint =
    s"(reduceIExp ${PrettyPhrasePrinter(out)} ${PrettyPhrasePrinter(f)} ${PrettyPhrasePrinter(init)} ${PrettyPhrasePrinter(in)})"

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
    `new`(dt2, OpenCL.PrivateMemory, accum =>
      (accum.wr `:=` init) `;`
        `for`(n, i =>
          SubstituteImplementations(f(accum.wr)(in `@` i)(accum.rd), env)
        ) `;`
        out(accum.rd)
    )
  }

}
