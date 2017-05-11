package idealised.IntermediatePrimitives

import idealised._
import idealised.Core._
import idealised.Core.OperationalSemantics._
import idealised.Compiling.SubstituteImplementations
import idealised.DSL.typed._
import idealised.ImperativePrimitives.{TruncAcc, TruncExp}

import scala.xml.Elem

final case class IterateIExp(n: Nat,
                             m: Nat,
                             k: Nat,
                             dt: DataType,
                             out: Phrase[ExpType -> CommandType],
                             f: Phrase[`(nat)->`[AccType -> (ExpType -> CommandType)]],
                             in: Phrase[ExpType])
  extends CommandPrimitive with Intermediate[CommandType] {

  override def typeCheck(): Unit = {
    import TypeChecker._
    f match {
      case NatDependentLambda(l, _) =>
        (n: Nat) -> (m: Nat) -> (k: Nat) -> (dt: DataType) ->
          (out :: t"(exp[$m.$dt] -> comm)") ->
          (f :: t"($l : nat) -> acc[${l /^ n}.$dt] -> exp[$l.$dt] -> comm") ->
          (in :: exp"[${n.pow(k) * m}.$dt]") ->
          comm

      case _ => throw new Exception("This should not happen")
    }
  }

  override def eval(s: Store): Store = ???

  override def visitAndRebuild(fun: VisitAndRebuild.Visitor): Phrase[CommandType] = {
    IterateIExp(fun(n), fun(m), fun(k), fun(dt),
      VisitAndRebuild(out, fun),
      VisitAndRebuild(f, fun),
      VisitAndRebuild(in, fun))
  }

  override def substituteImpl(env: SubstituteImplementations.Environment): Phrase[CommandType] = {
    val addressSpace = OpenCL.GlobalMemory

    val `n^k*m` = n.pow(k) * m

    `new`(dt"[${`n^k*m`}.$dt]", addressSpace, buf1 =>
      `new`(dt"[${`n^k*m`}.$dt]", addressSpace, buf2 =>
        SubstituteImplementations(MapI(`n^k*m`, dt, dt,
          λ(exp"[$dt]")(e => λ(acc"[$dt]")(a => a := e)), in, buf1.wr), env) `;`
          dblBufFor(`n^k*m`, m, k, dt, addressSpace, buf1, buf2,
            _Λ_(l => λ(acc"[${`n^k*m`}.$dt]")(a => λ(exp"[${`n^k*m`}.$dt]")(e =>
              SubstituteImplementations(
                f (n.pow(k - l) * m)
                  (TruncAcc(`n^k*m`, n.pow(k - l - 1) * m, dt, a))
                  (TruncExp(`n^k*m`, n.pow(k - l    ) * m, dt, e)), env)))),
            out)))
  }

  override def prettyPrint: String = s"(iterateIExp ${PrettyPhrasePrinter(out)} ${PrettyPhrasePrinter(f)} ${PrettyPhrasePrinter(in)})"

  override def xmlPrinter: Elem = {
    val l = f match {
      case NatDependentLambda(l_, _) => l_
      case _ => throw new Exception("This should not happen")
    }
    <iterateIExp n={ToString(n)} m={ToString(m)} k={ToString(k)} dt={ToString(dt)}>
      <output type={ToString(ExpType(ArrayType(m, dt)) -> CommandType())}>
        {Core.xmlPrinter(out)}
      </output>
      <f type={ToString(l -> (AccType(ArrayType(l /^ n, dt)) -> (ExpType(ArrayType(l, dt)) -> CommandType())))}>
        {Core.xmlPrinter(f)}
      </f>
      <input type={ToString(ExpType(ArrayType(n.pow(k) * m, dt)))}>
        {Core.xmlPrinter(in)}
      </input>
    </iterateIExp>
  }
}
