package idealised.MidLevelCombinators

import idealised._
import idealised.Core._
import idealised.Core.OperationalSemantics._
import idealised.Compiling.SubstituteImplementations
import idealised.DSL.typed._
import idealised.LowLevelCombinators.{TruncAcc, TruncExp}

import scala.xml.Elem

final case class IterateIExp(n: Nat,
                             m: Nat,
                             k: Nat,
                             dt: DataType,
                             out: Phrase[ExpType -> CommandType],
                             f: Phrase[`(nat)->`[AccType -> (ExpType -> CommandType)]],
                             in: Phrase[ExpType])
  extends MidLevelCombinator {

  override def typeCheck(): Unit = {
    import TypeChecker._
    f match {
      case NatDependentLambdaPhrase(l, _) =>
        (n: Nat) -> (m: Nat) -> (k: Nat) -> (dt: DataType) ->
          (out `:` t"(exp[$m.$dt] -> comm)") ->
          (f `:` t"($l : nat) -> acc[${l /^ n}.$dt] -> exp[$l.$dt] -> comm") ->
          (in `:` exp"[${n.pow(k) * m}.$dt]") ->
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
    val sEnd = n.pow(k) * m
    val s = (l: Nat) => n.pow(k - l) * m

    val addressSpace = OpenCL.GlobalMemory

    `new`(dt"[$sEnd.$dt]", addressSpace, buf1 => {
      `new`(dt"[$sEnd.$dt]", addressSpace, buf2 => {
        SubstituteImplementations(MapI(sEnd, dt, dt, buf1.wr,
          λ(acc"[$dt]")(a => λ(exp"[$dt]")(e => a `:=` e)), in), env) `;`
          dblBufFor(sEnd, dt, addressSpace, buf1, buf2, k,
            _Λ_(l => {
              val s_l = s(l)
              val s_l1 = s(l + 1)
              λ(acc"[$sEnd.$dt]")(a =>
                λ(exp"[$sEnd.$dt]")(e =>
                  SubstituteImplementations(f(s_l)(TruncAcc(sEnd, s_l1, dt, a))(TruncExp(sEnd, s_l, dt, e)), env)
                )
              )
            }),
            out
          )
      })
    })
  }

  override def prettyPrint: String = s"(iterateIExp ${PrettyPrinter(out)} ${PrettyPrinter(f)} ${PrettyPrinter(in)})"

  override def xmlPrinter: Elem = {
    val l = f match {
      case NatDependentLambdaPhrase(l_, _) => l_
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
