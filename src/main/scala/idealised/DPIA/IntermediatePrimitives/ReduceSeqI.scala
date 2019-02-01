package idealised.DPIA.IntermediatePrimitives

import idealised.DPIA.Compilation.SubstituteImplementations
import idealised.DPIA.DSL._
import idealised.DPIA.Phrases._
import idealised.DPIA.Semantics.OperationalSemantics
import idealised.DPIA.Semantics.OperationalSemantics._
import idealised.DPIA.Types._
import idealised.DPIA._

import scala.language.reflectiveCalls
import scala.xml.Elem

final case class ReduceSeqI(n: Nat,
                            dt1: DataType,
                            dt2: DataType,
                            f: Phrase[ExpType -> (ExpType -> (AccType -> CommandType))],
                            init: Phrase[ExpType],
                            in: Phrase[ExpType],
                            out: Phrase[ExpType -> CommandType])
  extends CommandPrimitive with Intermediate[CommandType] {

  override val `type`: CommandType =
    (n: Nat) -> (dt1: DataType) -> (dt2: DataType) ->
      (f :: t"exp[$dt1] -> exp[$dt2] -> acc[$dt2] -> comm") ->
        (init :: exp"[$dt2]") ->
          (in :: exp"[$n.$dt1]") ->
            (out :: t"exp[$dt2] -> comm") ->
              comm

  override def visitAndRebuild(fun: VisitAndRebuild.Visitor): Phrase[CommandType] = {
    ReduceSeqI(fun(n), fun(dt1), fun(dt2),
      VisitAndRebuild(f, fun),
      VisitAndRebuild(init, fun),
      VisitAndRebuild(in, fun),
        VisitAndRebuild(out, fun))
  }

  override def eval(s: Store): Store = {
    val outE = OperationalSemantics.eval(s, out)

    val fE = OperationalSemantics.eval(s, f)(TrinaryFunctionEvaluator)
    val n = in.t match {
      case ExpType(ArrayType(len, _)) => len
    }

    OperationalSemantics.eval(s, `new`(init.t.dataType, idealised.OpenCL.PrivateMemory, accum => {
      (accum.wr `:=` init) `;`
        `for`(n, i =>
          fE(in `@` i)(accum.rd)(accum.wr)
        ) `;`
        outE(π1(accum))
    }))
  }

  override def prettyPrint =
    s"(reduceIExp ${PrettyPhrasePrinter(out)} ${PrettyPhrasePrinter(f)} ${PrettyPhrasePrinter(init)} ${PrettyPhrasePrinter(in)})"

  override def xmlPrinter: Elem =
    <reduceIExp n={ToString(n)} dt1={ToString(dt1)} dt2={ToString(dt2)}>
      <output type={ToString(ExpType(dt2) -> CommandType())}>
        {Phrases.xmlPrinter(out)}
      </output>
      <f type={ToString(AccType(dt2) -> (ExpType(dt1) -> (ExpType(dt2) -> CommandType())))}>
        {Phrases.xmlPrinter(f)}
      </f>
      <init type={ToString(ExpType(dt2))}>
        {Phrases.xmlPrinter(init)}
      </init>
      <input type={ToString(ExpType(ArrayType(n, dt1)))}>
        {Phrases.xmlPrinter(in)}
      </input>
    </reduceIExp>

  override def substituteImpl(env: SubstituteImplementations.Environment): Phrase[CommandType] = {
    // TODO: generalise allocation
    `new`(dt2, idealised.OpenCL.PrivateMemory, acc =>
      (acc.wr :=|dt2| init) `;`
        `for`(n, i =>
          SubstituteImplementations(f(in `@` i)(acc.rd)(acc.wr), env)
        ) `;`
        out(acc.rd)
    )
  }
}
