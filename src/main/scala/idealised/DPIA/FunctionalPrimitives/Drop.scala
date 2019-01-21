package idealised.DPIA.FunctionalPrimitives

import idealised.DPIA.Compilation.RewriteToImperative
import idealised.DPIA.DSL.{λ, _}
import idealised.DPIA.ImperativePrimitives.DropAcc
import idealised.DPIA.Phrases.{ExpPrimitive, Phrase, VisitAndRebuild}
import idealised.DPIA.Semantics.OperationalSemantics.{Data, Store}
import idealised.DPIA.Types.{AccType, CommandType, DataType, ExpType, _}
import idealised.DPIA.{->, Nat, Phrases, _}

import scala.language.reflectiveCalls
import scala.xml.Elem

// this drops n many elements from an array of m elements
final case class Drop(n: Nat,
                      m: Nat,
                      dt: DataType,
                      array: Phrase[ExpType])
  extends ExpPrimitive {

  override val `type`: ExpType =
    (n: Nat) -> (m: Nat) -> (dt: DataType) ->
      (array :: exp"[$m.$dt]") -> exp"[${m - n}.$dt]"

  override def eval(s: Store): Data = ???

  override def visitAndRebuild(fun: VisitAndRebuild.Visitor): Phrase[ExpType] = {
    Drop(fun(n), fun(m), fun(dt), VisitAndRebuild(array, fun))
  }

  override def acceptorTranslation(A: Phrase[AccType]): Phrase[CommandType] = {
    import RewriteToImperative._
    acc(array)(DropAcc(n, m, dt, A))
  }

  override def continuationTranslation(C: Phrase[->[ExpType, CommandType]]): Phrase[CommandType] = {
    import RewriteToImperative._
    con(array)(λ(exp"[$m.$dt]")(x => C(Drop(n, m, dt, x))))
  }

  override def xmlPrinter: Elem =
    <drop n={n.toString} m={m.toString} dt={dt.toString}>
      {Phrases.xmlPrinter(array)}
    </drop>

  override def prettyPrint: String = s"(drop $array)"

}
