package idealised.LowLevelPrimitives

import lift.arithmetic.{Cst, RangeAdd}
import idealised._
import idealised.Core._
import idealised.Core.OperationalSemantics._

import scala.xml.Elem

final case class DoubleBufferFor(n: Nat,
                                 m: Nat,
                                 k: Nat,
                                 dt: DataType,
                                 addressSpace: AddressSpace,
                                 buffer1: Phrase[VarType],
                                 buffer2: Phrase[VarType],
                                 body: Phrase[`(nat)->`[AccType -> (ExpType -> CommandType)]],
                                 C: Phrase[ExpType -> CommandType])
  extends CommandPrimitive {

  assert(
    body match {
      case NatDependentLambda(x, _) =>
        x.range match {
          case RangeAdd(start, stop, step) =>
            start == Cst(0) && stop == k && step == Cst(1)
          case _ => false
        }
      case _ => false
    },
    s"Range of NatIdentifier of $body does not match RangeAdd(0, $k, 1)"
  )

  override def typeCheck(): Unit = {
    import TypeChecker._
    body match {
      case NatDependentLambda(l, _) =>
        (n: Nat) -> (m: Nat) -> (k: Nat) -> (dt: DataType) ->
          (buffer1 :: t"var[$n.$dt]") ->
          (buffer2 :: t"var[$n.$dt]") ->
          (body :: t"($l : nat) -> acc[$n.$dt] -> exp[$n.$dt] -> comm") ->
          (C :: t"exp[$m.$dt] -> comm") ->
          comm

      case _ => throw new Exception("This should not happen")
    }
  }

  override def eval(s: Store): Store = ???

  override def visitAndRebuild(fun: VisitAndRebuild.Visitor): Phrase[CommandType] = {
    DoubleBufferFor(fun(n), fun(m), fun(k), fun(dt),
      addressSpace,
      VisitAndRebuild(buffer1, fun),
      VisitAndRebuild(buffer2, fun),
      VisitAndRebuild(body, fun),
      VisitAndRebuild(C, fun))
  }

  override def prettyPrint: String = s"doubleBufferFor $k $buffer1 $buffer2 $body $C"

  override def xmlPrinter: Elem = {
    val l = body match {
      case NatDependentLambda(l_, _) => l_
      case _ => throw new Exception("This should not happen")
    }
    <doubleBufferFor n={ToString(n)} m={ToString(m)} k={ToString(k)} dt={ToString(dt)} addressSpace={ToString(addressSpace)}>
      <buffer1 type={ToString(VarType(ArrayType(n, dt)))}>
        {Core.xmlPrinter(buffer1)}
      </buffer1>
      <buffer2 type={ToString(VarType(ArrayType(n, dt)))}>
        {Core.xmlPrinter(buffer2)}
      </buffer2>
      <body type={ToString(l -> ((AccType(ArrayType(n, dt)) x ExpType(ArrayType(n, dt))) -> CommandType()))}>
        {Core.xmlPrinter(body)}
      </body>
      <continuation type={ToString(ExpType(ArrayType(m, dt)) -> CommandType())}>
        {Core.xmlPrinter(C)}
      </continuation>
    </doubleBufferFor>
  }
}
