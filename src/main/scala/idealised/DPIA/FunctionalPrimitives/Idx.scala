package idealised.DPIA.FunctionalPrimitives

import idealised.DPIA.Compilation.{TranslationContext, TranslationToImperative}
import idealised.DPIA.DSL._
import idealised.DPIA.Phrases._
import idealised.DPIA.Semantics.OperationalSemantics
import idealised.DPIA.Semantics.OperationalSemantics._
import idealised.DPIA.Types._
import idealised.DPIA.{Phrases, _}

import scala.language.reflectiveCalls
import scala.xml.Elem

final case class Idx(n: Nat,
                     dt: DataType,
                     index: Phrase[ExpType],
                     array: Phrase[ExpType])
  extends ExpPrimitive {

  override val t: ExpType =
    (n: Nat) ->: (dt: DataType) ->:
      (index :: exp"[idx($n), $read]") ->:
        (array :: exp"[$n.$dt, $read]") ->:
          exp"[$dt, $read]"

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
    (OperationalSemantics.eval(s, array), OperationalSemantics.eval(s, index)) match {
      case (ArrayData(xs), IntData(i)) => xs(i)
      case _ => throw new Exception("This should not happen")
    }
  }

  override def visitAndRebuild(fun: VisitAndRebuild.Visitor): Phrase[ExpType] = {
    Idx(fun.nat(n), fun.data(dt), VisitAndRebuild(index, fun), VisitAndRebuild(array, fun))
  }

  override def prettyPrint: String = s"(${PrettyPhrasePrinter(array)})[${PrettyPhrasePrinter(index)}]"

  override def xmlPrinter: Elem =
    <idx n={ToString(n)} dt={ToString(dt)}>
      <input type={ToString(ExpType(ArrayType(n, dt), read))}>
        {Phrases.xmlPrinter(array)}
      </input>
      <index type={ToString(ExpType(int, read))}>
        {Phrases.xmlPrinter(index)}
      </index>
    </idx>

  override def acceptorTranslation(A: Phrase[AccType])
                                  (implicit context: TranslationContext): Phrase[CommType] = {
    import TranslationToImperative._
    con(array)(λ(exp"[$n.$dt, $read]")(x => A :=| dt | Idx(n, dt, index, x)))
  }

  override def mapAcceptorTranslation(f: Phrase[ExpType ->: ExpType], A: Phrase[AccType])
                                     (implicit context: TranslationContext): Phrase[CommType] =
    ???

  override def continuationTranslation(C: Phrase[ExpType ->: CommType])
                                      (implicit context: TranslationContext): Phrase[CommType] = {
    import TranslationToImperative._
    con(array)(λ(exp"[$n.$dt, $read]")(e => C(Idx(n, dt, index, e))))
  }
}

object Idx {
  def apply(index: Phrase[ExpType], array: Phrase[ExpType]): Idx = {
    (index.t, array.t) match {
      case (ExpType(IndexType(n1), _: read.type), ExpType(ArrayType(n2, dt_), _: read.type)) if n1 == n2 =>
        Idx(n1, dt_, index, array)
      case x => error(x.toString, "(exp[idx(n)], exp[n.dt])")
    }
  }
}
