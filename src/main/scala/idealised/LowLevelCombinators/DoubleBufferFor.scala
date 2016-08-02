package idealised.LowLevelCombinators

import idealised._
import idealised.Core._
import idealised.Core.OperationalSemantics._

import scala.xml.Elem

final case class DoubleBufferFor(n: Nat,
                                 dt: DataType,
                                 addressSpace: AddressSpace,
                                 buffer1: Phrase[VarType],
                                 buffer2: Phrase[VarType],
                                 k: Nat,
                                 body: Phrase[`(nat)->`[AccType -> (ExpType -> CommandType)]],
                                 C: Phrase[ExpType -> CommandType])
  extends LowLevelCommCombinator {

  override def typeCheck(): Unit = {
    import TypeChecker._
    body match {
      case NatDependentLambdaPhrase(l, _) =>
        (n: Nat) -> (k: Nat) -> (dt: DataType) ->
          (buffer1 `:` t"var[$n.$dt]") ->
          (buffer2 `:` t"var[$n.$dt]") ->
          (body `:` t"($l : nat) -> acc[$n.$dt] -> exp[$n.$dt] -> comm") ->
          (C `:` t"exp[$n.$dt] -> comm") ->
          comm

      case _ => throw new Exception("This should not happen")
    }
  }

  override def eval(s: Store): Store = ???

  override def visitAndRebuild(fun: VisitAndRebuild.Visitor): Phrase[CommandType] = {
    DoubleBufferFor(fun(n), fun(dt),
      addressSpace,
      VisitAndRebuild(buffer1, fun),
      VisitAndRebuild(buffer2, fun),
      fun(k),
      VisitAndRebuild(body, fun),
      VisitAndRebuild(C, fun))
  }

  override def prettyPrint: String = s"doubleBufferFor $buffer1 $buffer2 $k $body $C"

  override def xmlPrinter: Elem = {
    val l = body match {
      case NatDependentLambdaPhrase(l_, _) => l_
      case _ => throw new Exception("This should not happen")
    }
    <doubleBufferFor k={ToString(k)} n={ToString(n)} dt={ToString(dt)} addressSpace={ToString(addressSpace)}>
      <buffer1 type={ToString(VarType(ArrayType(n, dt)))}>
        {Core.xmlPrinter(buffer1)}
      </buffer1>
      <buffer2 type={ToString(VarType(ArrayType(n, dt)))}>
        {Core.xmlPrinter(buffer2)}
      </buffer2>
      <body type={ToString(l -> ((AccType(ArrayType(n, dt)) x ExpType(ArrayType(n, dt))) -> CommandType()))}>
        {Core.xmlPrinter(body)}
      </body>
      <continuation type={ToString(ExpType(ArrayType(n, dt)) -> CommandType())}>
        {Core.xmlPrinter(C)}
      </continuation>
    </doubleBufferFor>
  }
}
