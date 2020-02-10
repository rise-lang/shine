package shine.OpenCL.ImperativePrimitives

import shine.DPIA.Phrases._
import shine.DPIA.Semantics.OperationalSemantics.Store
import shine.DPIA.Types._
import shine.DPIA.Types.DataType._
import shine.DPIA._

import scala.xml.Elem

abstract class OpenCLParFor(val n: Nat,
                            val dt: DataType,
                            val out: Phrase[AccType],
                            val body: Phrase[ExpType ->: AccType ->: CommType],
                            val init: Nat,
                            val step: Nat,
                            val unroll: Boolean)
  extends CommandPrimitive {

  out :: accT(n`.`dt)
  body :: expT(idx(n), read) ->: accT(dt) ->: comm

  def parallelismLevel: shine.OpenCL.ParallelismLevel

  //  protected var env: OpenCLOldCodeGenerator.Environment = _

  def name: String

  override def eval(s: Store): Store = ???

  override def visitAndRebuild(fun: VisitAndRebuild.Visitor): Phrase[CommType] = {
    makeCLParFor(fun.nat(n), fun.data(dt),
      VisitAndRebuild(out, fun),
      VisitAndRebuild(body, fun),
      fun.nat(init), fun.nat(step))
  }

  def makeCLParFor: (Nat, DataType,
    Phrase[AccType],
    Phrase[ExpType ->: AccType ->: CommType],
    Nat, Nat) => OpenCLParFor

  override def prettyPrint: String =
    s"(${this.getClass.getSimpleName} $n ${PrettyPhrasePrinter(out)} ${PrettyPhrasePrinter(body)})"

  override def xmlPrinter: Elem =
    <clParFor n={ToString(n)} dt={ToString(dt)}>
      <output type={ToString(AccType(ArrayType(n, dt)))}>
        {Phrases.xmlPrinter(out)}
      </output>
      <body type={ToString(ExpType(IndexType(n), read) ->: AccType(dt) ->: CommType())}>
        {Phrases.xmlPrinter(body)}
      </body>
    </clParFor>.copy(label = {
      val name = this.getClass.getSimpleName
      Character.toLowerCase(name.charAt(0)) + name.substring(1)
    })
}

object OpenCLParFor {
  def unapply(arg: OpenCLParFor): Option[(Nat, DataType, Phrase[AccType], Phrase[ExpType ->: AccType ->: CommType], Nat, Nat, Boolean)] =
    Some((arg.n, arg.dt, arg.out, arg.body, arg.init, arg.step, arg.unroll))
}
