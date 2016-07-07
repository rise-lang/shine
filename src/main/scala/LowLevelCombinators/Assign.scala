package LowLevelCombinators

import Core.OperationalSemantics._
import Core._
import opencl.generator.OpenCLAST.{AssignmentExpression, Block}

import scala.xml.Elem

case class Assign(dt: BasicType,
                  lhs: Phrase[AccType],
                  rhs: Phrase[ExpType])
  extends LowLevelCommCombinator {

  override def typeCheck(): Unit = {
    import TypeChecker._
    lhs checkType acc"[$dt]"
    rhs checkType exp"[$dt]"
  }

  override def eval(s: Store): Store = {
    def evalAssign(s: Store, lhs: AccIdentifier, rhs: Data,
                   continuation: (Store, String, Data) => Store): Store = {
      lhs match {
        case NamedIdentifier(name) =>
          assert(s.contains(name))
          continuation(s, name, rhs)

        case ArrayAccessIdentifier(array, index) =>
          evalAssign(s, array, rhs, (s, arrayName, rhsValue) => {
            assert(s.contains(arrayName))
            s(arrayName) match {
              case ArrayData(vec) => continuation(s, arrayName, ArrayData(vec.updated(index.eval, rhsValue)))
              case _ => throw new Exception("This should not happen")
            }
          })

        case RecordIdentiers(fstI, sndI) =>
          rhs match {
            case RecordData(fstD, sndD) =>
              val s1 = evalAssign(s, fstI, fstD, continuation)
              evalAssign(s1, sndI, sndD, continuation)
            case _ => throw new Exception("This should not happen")
          }
      }
    }

    evalAssign(s, OperationalSemantics.eval(s, lhs), OperationalSemantics.eval(s, rhs), (s, identifier, value) => {
      s + (identifier -> value)
    })
  }

  override def visitAndRebuild(fun: VisitAndRebuild.fun): Phrase[CommandType] = {
    Assign(fun(dt), VisitAndRebuild(lhs, fun), VisitAndRebuild(rhs, fun))
  }

  override def toOpenCL(block: Block, env: ToOpenCL.Environment): Block = {
    (block: Block) += AssignmentExpression(ToOpenCL.acc(lhs, env), ToOpenCL.exp(rhs, env))
  }

  override def prettyPrint: String = s"(${PrettyPrinter(lhs)} := ${PrettyPrinter(rhs)})"

  override def xmlPrinter: Elem =
    <assign>
      <lhs>{Core.xmlPrinter(lhs)}</lhs>
      <rhs>{Core.xmlPrinter(rhs)}</rhs>
    </assign>
}
