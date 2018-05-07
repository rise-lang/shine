package idealised.DPIA.IntermediatePrimitives

import idealised.DPIA.Compilation.SubstituteImplementations
import idealised.DPIA.DSL._
import idealised.DPIA.Phrases._
import idealised.DPIA.Semantics.OperationalSemantics
import idealised.DPIA.Semantics.OperationalSemantics._
import idealised.DPIA.Types._
import idealised.DPIA._
import idealised._

import scala.language.reflectiveCalls

import scala.xml.Elem

/**
  * Created by federico on 13/01/18.
  */
final case class ScanI(n: Nat,
            dt1: DataType,
            dt2: DataType,
            f: Phrase[ExpType -> (ExpType -> (AccType -> CommandType))],
            init: Phrase[ExpType],
            in: Phrase[ExpType],
            out: Phrase[AccType])
extends CommandPrimitive with Intermediate[CommandType]{

  override val `type`: CommandType =
    (n: Nat) -> (dt1: DataType) -> (dt2: DataType) ->
      (f :: t"exp[$dt1] -> exp[$dt2] -> acc[$dt2] -> comm") ->
      (in :: exp"[$n.$dt1]") ->
      (out :: acc"[$n.$dt2]") ->
      comm

  override def visitAndRebuild(fun: VisitAndRebuild.Visitor): Phrase[CommandType] = {
    ScanI(fun(n), fun(dt1), fun(dt2),
      VisitAndRebuild(f, fun),
      VisitAndRebuild(init, fun),
      VisitAndRebuild(in, fun),
      VisitAndRebuild(out, fun))
  }

  override def eval(s: Store): Store = ???

  override def prettyPrint =
    s"(scanIExp ${PrettyPhrasePrinter(out)} ${PrettyPhrasePrinter(f)} ${PrettyPhrasePrinter(init)} ${PrettyPhrasePrinter(in)})"

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
    `new`(dt2, OpenCL.PrivateMemory, acc =>
      (acc.wr :=|dt2|init) `;`
      `for`(n, i =>
        SubstituteImplementations(f(in `@` i)(acc.rd)(acc.wr), env) `;`
          ((out `@` i) :=|dt2|acc.rd)
      )
    )
  }
}
