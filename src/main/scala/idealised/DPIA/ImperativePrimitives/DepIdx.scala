package idealised.DPIA.ImperativePrimitives

import idealised.DPIA.Compilation.TranslationToImperative.con
import idealised.DPIA.Compilation.{CodeGenerator, TranslationContext, TranslationToImperative}
import idealised.DPIA.DSL.λ
import idealised.DPIA.Phrases._
import idealised.DPIA.Semantics.OperationalSemantics._
import idealised.DPIA.Types._
import idealised.DPIA.{Phrases, _}
import idealised.DPIA.Compilation.{CodeGenerator, TranslationContext, TranslationToImperative}
import idealised.DPIA.DSL._
import idealised.DPIA.Phrases._
import idealised.DPIA.Semantics.OperationalSemantics
import idealised.DPIA.Semantics.OperationalSemantics._
import idealised.DPIA.Types._
import idealised.DPIA.{Phrases, _}

import scala.language.reflectiveCalls
import scala.xml.Elem

final case class DepIdx(n: Nat,
                        ft:NatToData,
                        index: Nat,
                        array: Phrase[ExpType])
  extends ExpPrimitive {

  override val t: ExpType =
    (n: Nat) ->: (ft: NatToData) ->: (index: Nat) ->:
      (array :: exp"[$n.$ft, $read]") ->:
        exp"[${ft(index)}, $read]"

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
    DepIdx(fun.nat(n), fun.natToData(ft), fun.nat(index), VisitAndRebuild(array, fun))
  }

  override def prettyPrint: String = s"(${PrettyPhrasePrinter(array)})[$index]"

  override def xmlPrinter: Elem =
    <depIdx n={ToString(n)} ft={ToString(ft)} index={ToString(index)}>
      <input type={ToString(array.t)}>
        {Phrases.xmlPrinter(array)}
      </input>
    </depIdx>

  override def acceptorTranslation(A: Phrase[AccType])
                                  (implicit context: TranslationContext): Phrase[CommType] = {
    import TranslationToImperative._
    con(array)(λ(exp"[$n.$ft, $read]")(x => A :=| {ft(index)} | DepIdx(n, ft, index, x)))

  }

  override def mapAcceptorTranslation(f: Phrase[ExpType ->: ExpType], A: Phrase[AccType])
                                     (implicit context: TranslationContext): Phrase[CommType] =
    ???

  override def continuationTranslation(C: Phrase[ExpType ->: CommType])
                                      (implicit context: TranslationContext): Phrase[CommType] = {
    import TranslationToImperative._
    con(array)(λ(exp"[$n.$ft, $read]")(e => C(DepIdx(n, ft, index, e))))
  }
}
