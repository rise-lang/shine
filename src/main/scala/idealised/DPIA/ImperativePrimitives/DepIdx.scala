package idealised.DPIA.ImperativePrimitives

import idealised.DPIA.Compilation.{CodeGenerator, TranslationContext}
import idealised.DPIA.Phrases._
import idealised.DPIA.Semantics.OperationalSemantics._
import idealised.DPIA.Types._
import idealised.DPIA.{Phrases, _}

import scala.language.reflectiveCalls
import scala.xml.Elem

final case class DepIdx(n: Nat,
                        i: NatIdentifier,
                        dt: DataType,
                        index: Nat,
                        array: Phrase[ExpType])
  extends ExpPrimitive {

  private def makeDt(x:Nat):DataType = DataType.substitute(x, `for`=i, `in`=dt)

  override val `type`: ExpType =
    (n: Nat) -> (i: Nat) -> (dt: DataType) -> (index: Nat) ->
      (array :: exp"[${DepArrayType(n, makeDt)}]") ->
        exp"[${makeDt(index)}]"

  //  override def inferTypes: Idx = {
  //    import TypeInference._
  //    val index_ = TypeInference(index)
  //    val array_ = TypeInference(array)
  //    (index_.t, array_.t) match {
  //      case (ExpType(IndexType(n1)), ExpType(ArrayType(n2, dt_))) if n1 == n2 =>
  //        Idx(n1, dt_, index_, array_)
  //      case x => error(x.toString, "(exp[idx(n)], exp[n.dt])")
  //    }
  //  }

  override def eval(s: Store): Data = {
//    (OperationalSemantics.eval(s, array), OperationalSemantics.eval(s, index)) match {
//      case (ArrayData(xs), IntData(i)) => xs(i)
//      case _ => throw new Exception("This should not happen")
//    }
    ???
  }

  override def visitAndRebuild(fun: VisitAndRebuild.Visitor): Phrase[ExpType] = {
    DepIdx(fun(n), fun(i).asInstanceOf[NatIdentifier], fun(dt), fun(index), VisitAndRebuild(array, fun))
  }

  override def prettyPrint: String = s"(${PrettyPhrasePrinter(array)})[$index]"

  override def xmlPrinter: Elem =
    <depIdx n={ToString(n)} i={ToString(i)} dt={ToString(dt)} index={ToString(index)}>
      <input type={ToString(array.t)}>
        {Phrases.xmlPrinter(array)}
      </input>
    </depIdx>

  override def acceptorTranslation(A: Phrase[AccType])
                                  (implicit context: TranslationContext): Phrase[CommandType] = {
//    import RewriteToImperative._
//    con(array)(λ(exp"[$n.$dt]")(x => A :=| dt | Idx(n, dt, index, x)))
    ???
  }

  override def mapAcceptorTranslation(f: Phrase[ExpType -> ExpType], A: Phrase[AccType])
                                     (implicit context: TranslationContext): Phrase[CommandType] =
    ???

  override def continuationTranslation(C: Phrase[ExpType -> CommandType])
                                      (implicit context: TranslationContext): Phrase[CommandType] = {
//    import RewriteToImperative._
//    con(array)(λ(exp"[$n.$dt]")(e => C(Idx(n, dt, index, e))))
    ???
  }
}
