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

  def makeReduceI(n: Nat,
                  dt1: DataType,
                  dt2: DataType,
                  f: Phrase[ExpType -> (ExpType -> (AccType -> CommandType))],
                  init: Phrase[ExpType],
                  array: Phrase[ExpType],
                  out: Phrase[ExpType -> CommandType])
                 (implicit context: TranslationContext): Phrase[CommandType]

  override val t: ExpType =
    (n: Nat) -> (dt1: DataType) -> (dt2: DataType) ->
      (f :: t"exp[$dt1, $read] -> exp[$dt2, $read] -> exp[$dt2, $write]") ->
        (init :: exp"[$dt2, $write]") ->
          (array :: exp"[$n.$dt1, $read]") -> exp"[$dt2, $read]"

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

    con(this)(λ(exp"[$dt2, $read]")(r => acc(r)(A)))
  }

  override def mapAcceptorTranslation(f: Phrase[ExpType -> ExpType], A: Phrase[AccType])
                                     (implicit context: TranslationContext): Phrase[CommandType] =
    ???

  override def continuationTranslation(C: Phrase[ExpType -> CommandType])
                                      (implicit context: TranslationContext): Phrase[CommandType] = {
    import TranslationToImperative._

    con(array)(λ(exp"[$n.$dt1, $read]")(X =>
      con(init)(λ(exp"[$dt2, $read]")(Y =>
        makeReduceI(n, dt1, dt2,
          λ(exp"[$dt1, $read]")(x => λ(exp"[$dt2, $read]")(y => λ(acc"[$dt2, $read]")(o => acc( f(x)(y) )( o ) ))),
          Y, X, C)))))
  }

  override def xmlPrinter: Elem =
    <reduce n={ToString(n)} dt1={ToString(dt1)} dt2={ToString(dt2)}>
      <f type={ToString(ExpType(dt1, read) -> (ExpType(dt2, read) -> ExpType(dt2, write)))}>
        {Phrases.xmlPrinter(f)}
      </f>
      <init type={ToString(ExpType(dt2, write))}>
        {Phrases.xmlPrinter(init)}
      </init>
      <input type={ToString(ExpType(ArrayType(n, dt1), read))}>
        {Phrases.xmlPrinter(array)}
      </input>
    </reduce>.copy(label = {
      val name = this.getClass.getSimpleName
      Character.toLowerCase(name.charAt(0)) + name.substring(1)
    })
}
