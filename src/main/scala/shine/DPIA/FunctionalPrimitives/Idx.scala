package shine.DPIA.FunctionalPrimitives

import shine.DPIA.Compilation.{TranslationContext, TranslationToImperative}
import shine.DPIA.DSL._
import shine.DPIA.Phrases._
import shine.DPIA.Semantics.OperationalSemantics
import shine.DPIA.Semantics.OperationalSemantics._
import shine.DPIA.Types._
import shine.DPIA.Types.DataType._
import shine.DPIA.{Phrases, _}

import scala.language.reflectiveCalls
import scala.xml.Elem

final case class Idx(n: Nat,
                     dt: DataType,
                     index: Phrase[ExpType],
                     array: Phrase[ExpType])
  extends ExpPrimitive {

  index :: expT(idx(n), read)
  array :: expT(n`.`dt, read)
  override val t: ExpType = expT(dt, read)

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
    con(array)(λ(expT(n`.`dt, read))(x =>
      con(index)(fun(index.t)(i =>
        A :=| dt | Idx(n, dt, i, x)))))
  }

  override def continuationTranslation(C: Phrase[ExpType ->: CommType])
                                      (implicit context: TranslationContext): Phrase[CommType] = {
    import TranslationToImperative._
    con(array)(λ(expT(n`.`dt, read))(e =>
      con(index)(fun(index.t)(i =>
        C(Idx(n, dt, i, e))))))
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
