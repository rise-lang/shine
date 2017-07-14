package idealised.DPIA.ImperativePrimitives

import idealised.DPIA.DSL._
import idealised.DPIA.IntermediatePrimitives.MapI
import idealised.DPIA.Phrases._
import idealised.DPIA.Semantics.OperationalSemantics
import idealised.DPIA.Semantics.OperationalSemantics._
import idealised.DPIA.Types._
import idealised.DPIA._

import scala.language.reflectiveCalls
import scala.xml.Elem

final class Assign(val dt: DataType,
                   val lhs: Phrase[AccType],
                   val rhs: Phrase[ExpType])
  extends CommandPrimitive {

  override val `type`: CommandType =
    (dt: DataType) -> (lhs :: acc"[$dt]") -> (rhs :: exp"[$dt]") -> comm

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

        case RecordIdentifier(fstI, sndI) =>
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

  override def visitAndRebuild(fun: VisitAndRebuild.Visitor): Phrase[CommandType] = {
    Assign(fun(dt), VisitAndRebuild(lhs, fun), VisitAndRebuild(rhs, fun))
  }

  override def prettyPrint: String = s"(${PrettyPhrasePrinter(lhs)} := ${PrettyPhrasePrinter(rhs)})"

  override def xmlPrinter: Elem =
    <assign dt={ToString(dt)}>
      <lhs>
        {Phrases.xmlPrinter(lhs)}
      </lhs>
      <rhs>
        {Phrases.xmlPrinter(rhs)}
      </rhs>
    </assign>
}

object Assign {
  def apply(lhs: Phrase[AccType],
            rhs: Phrase[ExpType]): Assign = {
    (lhs.t, rhs.t) match {
      case (AccType(dt1), ExpType(dt2)) =>
        (dt1, dt2) match {
          case (t1, t2) if t1 == t2 =>
            t1 match {
              case _: BasicType | _: RecordType => // TODO: think about this more
                new Assign(t1, lhs, rhs)
              case _ =>
                error(s"${t1.toString}", expected = "a basic data type")
            }
          case _ =>
            error(s"${dt1.toString} and ${dt2.toString}", expected = "them to match")
        }
      case x => error(x.toString(), "(AccType(dt1), ExpType(dt2))")
    }
  }

  def apply(dt: DataType,
            A: Phrase[AccType],
            E: Phrase[ExpType]): Phrase[CommandType] = {
    dt match {
      // TODO: think about this more, but records (structs) are values ...
      case _: BasicType | _: RecordType => A := E

      case ArrayType(n, et) =>
        MapI(n, et, et, λ(ExpType(et))(x => λ(AccType(et))(a => a :=|et| x )), E, A)

//      case RecordType(dt1, dt2) =>
//        (recordAcc1(dt1, dt2, A) :=|dt1| fst(E)) `;` (recordAcc2(dt1, dt2, A) :=|dt2| snd(E))

      case _: DataTypeIdentifier => throw new Exception("This should not happen")
    }
  }
}
