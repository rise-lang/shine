package idealised.DPIA.FunctionalPrimitives


import idealised.DPIA.Compilation.{TranslationContext, TranslationToImperative}
import idealised.DPIA.DSL._
import idealised.DPIA.Phrases._
import idealised.DPIA.Semantics.OperationalSemantics
import idealised.DPIA.Semantics.OperationalSemantics._
import idealised.DPIA.Types._
import idealised.DPIA._

import scala.xml.Elem

abstract class AbstractReduce(n: Nat,
                              dt1: DataType,
                              dt2: DataType,
                              f: Phrase[ExpType -> (ExpType -> ExpType)],
                              init: Phrase[ExpType],
                              array: Phrase[ExpType])
  extends ExpPrimitive {

  def makeReduce: (Nat, DataType, DataType,
    Phrase[ExpType -> (ExpType -> ExpType)], Phrase[ExpType], Phrase[ExpType]) => AbstractReduce

  def makeReduceI: (Nat, DataType, DataType,
    Phrase[ExpType -> (ExpType -> (AccType -> CommandType))],
    Phrase[ExpType], Phrase[ExpType], Phrase[ExpType -> CommandType])
    => TranslationContext => Phrase[CommandType]

  override val `type`: ExpType =
    (n: Nat) -> (dt1: DataType) -> (dt2: DataType) ->
      (f :: t"exp[$dt1] -> exp[$dt2] -> exp[$dt2]") ->
      (init :: exp"[$dt2]") ->
      (array :: exp"[$n.$dt1]") -> exp"[$dt2]"

  override def visitAndRebuild(fun: VisitAndRebuild.Visitor): Phrase[ExpType] = {
    makeReduce(fun(n), fun(dt1), fun(dt2),
      VisitAndRebuild(f, fun), VisitAndRebuild(init, fun), VisitAndRebuild(array, fun))
  }

  override def eval(s: Store): Data = {
    val fE = OperationalSemantics.eval(s, f)(BinaryFunctionEvaluator)
    val initE = OperationalSemantics.eval(s, init)
    OperationalSemantics.eval(s, array) match {
      case ArrayData(xs) =>
        ArrayData(Vector(xs.fold(initE) {
          (x, y) => OperationalSemantics.eval(s,
            fE(Literal(x))(Literal(y)))
        }))
      case _ => throw new Exception("This should not happen")
    }
  }

  override def prettyPrint: String =
    s"${this.getClass.getSimpleName} (${PrettyPhrasePrinter(f)}) " +
      s"(${PrettyPhrasePrinter(init)}) (${PrettyPhrasePrinter(array)})"

  override def acceptorTranslation(A: Phrase[AccType])
                                  (implicit context: TranslationContext): Phrase[CommandType] = {
    import TranslationToImperative._

    con(this)(λ(exp"[$dt2]")(r => acc(r)(A)))
  }

  override def continuationTranslation(C: Phrase[ExpType -> CommandType])
                                      (implicit context: TranslationContext): Phrase[CommandType] = {
    import TranslationToImperative._

    con(array)(λ(exp"[$n.$dt1]")(X =>
      con(init)(λ(exp"[$dt2]")(Y =>
        makeReduceI(n, dt1, dt2,
          λ(exp"[$dt1]")(x => λ(exp"[$dt2]")(y => λ(acc"[$dt2]")(o => acc( f(x)(y) )( o )))),
          Y, X, C)(context)))))
  }

  override def xmlPrinter: Elem =
    <reduce n={ToString(n)} dt1={ToString(dt1)} dt2={ToString(dt2)}>
      <f type={ToString(ExpType(dt1) -> (ExpType(dt2) -> ExpType(dt2)))}>
        {Phrases.xmlPrinter(f)}
      </f>
      <init type={ToString(ExpType(dt2))}>
        {Phrases.xmlPrinter(init)}
      </init>
      <input type={ToString(ExpType(ArrayType(n, dt1)))}>
        {Phrases.xmlPrinter(array)}
      </input>
    </reduce>.copy(label = {
      val name = this.getClass.getSimpleName
      Character.toLowerCase(name.charAt(0)) + name.substring(1)
    })
}
