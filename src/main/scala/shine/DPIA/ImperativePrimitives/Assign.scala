package shine.DPIA.ImperativePrimitives

import shine.DPIA.Phrases._
import shine.DPIA.Semantics.OperationalSemantics
import shine.DPIA.Semantics.OperationalSemantics._
import shine.DPIA.Types._
import shine.DPIA._

import scala.xml.Elem

case class Assign(dt: DataType,
                  lhs: Phrase[AccType],
                  rhs: Phrase[ExpType])
  extends CommandPrimitive {

  lhs :: accT(dt)
  rhs :: expT(dt, read)

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

        case VectorAccessIdentifier(vector, index) =>
          evalAssign(s, vector, rhs, (s, vectorName, rhsValue) => {
            assert(s.contains(vectorName))
            s(vectorName) match {
              case ArrayData(vec) => continuation(s, vectorName, ArrayData(vec.updated(index.eval, rhsValue)))
              case _ => throw new Exception("This should not happen")
            }
          })

        case PairIdentifier(fstI, sndI) =>
          rhs match {
            case PairData(fstD, sndD) =>
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

  override def visitAndRebuild(fun: VisitAndRebuild.Visitor): Phrase[CommType] = {
    Assign(fun.data(dt), VisitAndRebuild(lhs, fun), VisitAndRebuild(rhs, fun))
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

  override def toString: String = s"Assign(${dt.toString}, ${lhs.toString}, ${rhs.toString})"
}

