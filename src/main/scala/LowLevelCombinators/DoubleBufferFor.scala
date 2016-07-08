package LowLevelCombinators

import Core.OperationalSemantics._
import Core._
import apart.arithmetic.ArithExpr

import scala.xml.Elem

case class DoubleBufferFor(n: ArithExpr,
                           dt: DataType,
                           addressSpace: AddressSpace,
                           buffer1: Phrase[VarType],
                           buffer2: Phrase[VarType],
                           k: ArithExpr,
                           body: Phrase[`(nat)->`[AccType -> (ExpType -> CommandType)]],
                           C: Phrase[ExpType -> CommandType])
  extends LowLevelCommCombinator {

  override def typeCheck(): Unit = {
    import TypeChecker._
    buffer1 checkType t"var[$n.$dt]"
    buffer2 checkType t"var[$n.$dt]"
    body match {
      case NatDependentLambdaPhrase(l, _) =>
        body checkType t"($l : nat) -> acc[$n.$dt] -> exp[$n.$dt] -> comm"
      case _ => throw new Exception("This should not happen")
    }
    C checkType t"exp[$n.$dt] -> comm"
  }

  override def eval(s: Store): Store = ???

  override def visitAndRebuild(fun: VisitAndRebuild.fun): Phrase[CommandType] = {
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
