package shine.DPIA.FunctionalPrimitives

import shine.DPIA.Compilation.TranslationContext
import shine.DPIA.Phrases._
import shine.DPIA.Semantics.OperationalSemantics
import shine.DPIA.Semantics.OperationalSemantics._
import shine.DPIA.Types._
import shine.DPIA.Types.DataType._
import shine.DPIA._

import scala.xml.Elem

abstract class AbstractMap(val n: Nat,
                           val dt1: DataType,
                           val dt2: DataType,
                           val f: Phrase[ExpType ->: ExpType],
                           val array: Phrase[ExpType])
  extends ExpPrimitive {

  def makeMap: (Nat, DataType, DataType, Phrase[ExpType ->: ExpType], Phrase[ExpType]) => AbstractMap

  f :: expT(dt1, read) ->: expT(dt2, write)
  array :: expT(n`.`dt1, read)
  override val t: ExpType = expT(n`.`dt2, write)

  override def visitAndRebuild(fun: VisitAndRebuild.Visitor): Phrase[ExpType] = {
    makeMap(fun.nat(n), fun.data(dt1), fun.data(dt2), VisitAndRebuild(f, fun), VisitAndRebuild(array, fun))
  }

  override def eval(s: Store): Data = {
    import shine.DPIA.Semantics.OperationalSemantics._
    val fE = OperationalSemantics.eval(s, f)
    OperationalSemantics.eval(s, array) match {
      case ArrayData(xs) =>
        ArrayData(xs.map { x =>
          OperationalSemantics.eval(s, fE(Literal(x)))
        })

      case _ => throw new Exception("This should not happen")
    }
  }

  override def acceptorTranslation(A: Phrase[AccType])
                                  (implicit context: TranslationContext): Phrase[CommType] = {
    //mapAcceptorTranslation(fun(exp"[$dt2, $read]")(x => x), A)
    ???
  }

  override def prettyPrint: String =
    s"${this.getClass.getSimpleName} (${PrettyPhrasePrinter(f)}) (${PrettyPhrasePrinter(array)})"

  override def xmlPrinter: Elem =
    <map n={ToString(n)} dt1={ToString(dt1)} dt2={ToString(dt2)}>
      <f type={ToString(ExpType(dt1, read) ->: ExpType(dt2, write))}>
        {Phrases.xmlPrinter(f)}
      </f>
      <input type={ToString(ExpType(ArrayType(n, dt1), read))}>
        {Phrases.xmlPrinter(array)}
      </input>
    </map>.copy(label = {
      val name = this.getClass.getSimpleName
      Character.toLowerCase(name.charAt(0)) + name.substring(1)
    })
}
