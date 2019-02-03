package idealised.DPIA.IntermediatePrimitives

import idealised.DPIA.Compilation.{TranslationContext, SubstituteImplementations}
import idealised.DPIA.DSL._
import idealised.DPIA.FunctionalPrimitives.Take
import idealised.DPIA.ImperativePrimitives.TakeAcc
import idealised.DPIA.Phrases._
import idealised.DPIA.Semantics.OperationalSemantics._
import idealised.DPIA.Types._
import idealised.DPIA.{Phrases, _}
import lift.arithmetic.NamedVar

import scala.xml.Elem

final case class IterateIAcc(n: Nat,
                             m: Nat,
                             k: Nat,
                             dt: DataType,
                             out: Phrase[AccType],
                             f: Phrase[`(nat)->`[AccType -> (ExpType -> CommandType)]],
                             in: Phrase[ExpType])
  extends CommandPrimitive with Intermediate[CommandType] {

  override val `type`: CommandType =
    f match {
      case NatDependentLambda(l, _) =>
        (n: Nat) -> (m: Nat) -> (k: Nat) -> (dt: DataType) ->
          (out :: acc"[$m.$dt]") ->
            (f :: t"($l : nat) -> acc[${l /^ n}.$dt] -> exp[$l.$dt] -> comm") ->
              (in :: exp"[${n.pow(k) * m}.$dt]") ->
                comm

      case _ => throw new Exception("This should not happen")
    }

  override def eval(s: Store): Store = ???

  override def visitAndRebuild(fun: VisitAndRebuild.Visitor): Phrase[CommandType] = {
    IterateIAcc(fun(n), fun(m), fun(k), fun(dt),
      VisitAndRebuild(out, fun),
      VisitAndRebuild(f, fun),
      VisitAndRebuild(in, fun))
  }

  override def substituteImpl(env: SubstituteImplementations.Environment)
                             (implicit context: TranslationContext): Phrase[CommandType] = {
    val `n^k*m` = n.pow(k) * m

    newDoubleBuffer(dt"[${`n^k*m`}.$dt]", dt"[$m.$dt]", ArrayType(`n^k*m`, dt), in, out,
      (v: Phrase[VarType],
       swap: Phrase[CommandType],
       done: Phrase[CommandType]) => {
        `for`(k, ip => {
          val i = NamedVar(ip.name)

          SubstituteImplementations(
            f.apply(n.pow(k - i) * m)
              .apply(TakeAcc(n.pow(k - i - 1) * m, `n^k*m`, dt, v.wr))
              .apply(Take(n.pow(k - i) * m, `n^k*m`, dt, v.rd)), env) `;`
            IfThenElse(ip < Literal(IndexData(k - 1, IndexType(k))), swap, done)
        })
      })
  }

  override def prettyPrint: String = s"(iterateIAcc ${PrettyPhrasePrinter(out)} ${PrettyPhrasePrinter(f)} ${PrettyPhrasePrinter(in)})"

  override def xmlPrinter: Elem = {
    val l = f match {
      case NatDependentLambda(l_, _) => l_
      case _ => throw new Exception("This should not happen")
    }
    <iterateIAcc n={ToString(n)} m={ToString(m)} k={ToString(k)} dt={ToString(dt)}>
      <output type={ToString(AccType(ArrayType(m, dt)))}>
        {Phrases.xmlPrinter(out)}
      </output>
      <f type={ToString(l -> (AccType(ArrayType(l /^ n, dt)) -> (ExpType(ArrayType(l, dt)) -> CommandType())))}>
        {Phrases.xmlPrinter(f)}
      </f>
      <input type={ToString(ExpType(ArrayType(n.pow(k) * m, dt)))}>
        {Phrases.xmlPrinter(in)}
      </input>
    </iterateIAcc>
  }
}
