package idealised.OpenCL.ImperativePrimitives

import idealised.C.AST.Stmt
import idealised.DPIA.Phrases.{CommandPrimitive, Phrase, PrettyPhrasePrinter, ToString, VisitAndRebuild}
import idealised.DPIA.Semantics.OperationalSemantics.Store
import idealised.DPIA.Types._
import idealised.DPIA._

import scala.xml.Elem

abstract class OpenCLParFor(val n: Nat,
                            val dt: DataType,
                            val out: Phrase[AccType],
                            val body: Phrase[ExpType ->: AccType ->: CommType],
                            val init: Nat,
                            val step: Nat,
                            val unroll: Boolean)
  extends CommandPrimitive {

  override val t: CommType =
    (n: Nat) ->: (dt: DataType) ->:
      (out :: acc"[$n.$dt]") ->:
      (body :: t"exp[idx($n), $read] -> acc[$dt] -> comm") ->:
      comm

  def parallelismLevel: idealised.OpenCL.ParallelismLevel

  //  protected var env: OpenCLOldCodeGenerator.Environment = _

  def name: String

  def synchronize: Stmt

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
