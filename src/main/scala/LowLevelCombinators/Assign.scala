package LowLevelCombinators

import Core.OperationalSemantics._
import Core._

import scala.xml.Elem

case class Assign(dt: BasicType,
                  lhs: Phrase[AccType],
                  rhs: Phrase[ExpType])
  extends LowLevelCommCombinator with TypeInferable {

  override def typeCheck(): Unit = {
    import TypeChecker._
    (dt: BasicType) -> (lhs `:` acc"[$dt]") -> (rhs `:` exp"[$dt]") -> comm
  }

  override def inferTypes: Assign = {
    import TypeInference._
    val lhs_ = TypeInference(lhs)
    val rhs_ = TypeInference(rhs)
    (lhs_.t, rhs_.t) match {
      case (AccType(dt1), ExpType(dt2)) =>
        (dt1, dt2) match {
          case (t1, t2) if t1 == t2 =>
            t1 match {
              case bt: BasicType =>
                Assign(bt, lhs_, rhs_)
              case _ =>
                error(s"${t1.toString}", expected = "a basic data type")
            }
          case _ =>
            error(s"${dt1.toString} and ${dt2.toString}", expected = "them to match")
        }
      case x => error(x.toString(), "(AccType(dt1), ExpType(dt2))")
    }
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
      s + Tuple2(identifier, value)
    })
  }

  override def visitAndRebuild(fun: VisitAndRebuild.fun): Phrase[CommandType] = {
    Assign(fun(dt), VisitAndRebuild(lhs, fun), VisitAndRebuild(rhs, fun))
  }

  override def prettyPrint: String = s"(${PrettyPrinter(lhs)} := ${PrettyPrinter(rhs)})"

  override def xmlPrinter: Elem =
    <assign dt={ToString(dt)}>
      <lhs>
        {Core.xmlPrinter(lhs)}
      </lhs>
      <rhs>
        {Core.xmlPrinter(rhs)}
      </rhs>
    </assign>
}
