package shine.OpenCL.ImperativePrimitives

import shine.DPIA.Phrases._
import shine.DPIA.Semantics.OperationalSemantics.Store
import shine.DPIA.Types._
import shine.DPIA.Types.DataType._
import shine.DPIA._

import scala.xml.Elem

abstract class OpenCLStridedFor(val n: Nat,
                            val body: Phrase[ExpType ->: CommType],
                            val init: Nat,
                            val step: Nat,
                            val unroll: Boolean)
  extends CommandPrimitive {

  body :: expT(idx(n), read) ->: comm

  def parallelismLevel: shine.OpenCL.ParallelismLevel

  //  protected var env: OpenCLOldCodeGenerator.Environment = _

  def name: String

  override def eval(s: Store): Store = ???

  override def visitAndRebuild(fun: VisitAndRebuild.Visitor): Phrase[CommType] = {
    makeCLStridedFor(fun.nat(n),
      VisitAndRebuild(body, fun),
      fun.nat(init), fun.nat(step), unroll)
  }

  def makeCLStridedFor: (Nat,
    Phrase[ExpType ->: CommType],
    Nat, Nat, Boolean) => OpenCLStridedFor

  override def prettyPrint: String =
    s"(${this.getClass.getSimpleName} $n ${PrettyPhrasePrinter(body)})"

  override def xmlPrinter: Elem =
    <clStridedFor n={ToString(n)}>
      <body type={ToString(ExpType(IndexType(n), read) ->: CommType())}>
        {Phrases.xmlPrinter(body)}
      </body>
    </clStridedFor>.copy(label = {
      val name = this.getClass.getSimpleName
      Character.toLowerCase(name.charAt(0)) + name.substring(1)
    })
}

object OpenCLStridedFor {
  def unapply(arg: OpenCLStridedFor): Option[(Nat, Phrase[ExpType ->: CommType], Nat, Nat, Boolean)] =
    Some((arg.n, arg.body, arg.init, arg.step, arg.unroll))
}
