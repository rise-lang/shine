package shine.OpenCL.FunctionalPrimitives

import shine.DPIA.Compilation.TranslationContext
import shine.DPIA.DSL._
import shine.DPIA._
import shine.DPIA.Phrases._
import shine.DPIA.Semantics.OperationalSemantics._
import shine.DPIA.Types._

import scala.xml.Elem // If (primitive):
// exp[t, read] -> exp[bool, read] ->
//  (exp[t, read] -> exp[s, write]) ->
//  (exp[t, read] -> exp[s, write]) -> exp[s, write]
final case class IfExpression(
  dt1: DataType,
  dt2: DataType,
  input: Phrase[ExpType],
  cond: Phrase[ExpType],
  firstCase: Phrase[ExpType ->: ExpType],
  secondCase: Phrase[ExpType ->: ExpType]
) extends ExpPrimitive {


  input :: expT(dt1, read)
  cond :: expT(bool, read)
  firstCase :: expT(dt1, read) ->: expT(dt2, write)
  secondCase :: expT(dt1, read) ->: expT(dt2, write)
  override val t: ExpType = expT(dt2, write)

  override def visitAndRebuild(
    fun: VisitAndRebuild.Visitor
  ): Phrase[ExpType] = {
    IfExpression(fun.data(dt1), fun.data(dt2), VisitAndRebuild(input, fun),
      VisitAndRebuild(cond, fun), VisitAndRebuild(firstCase, fun),
      VisitAndRebuild(secondCase, fun))
  }

  override def eval(s: Store): Data = ???

  override def prettyPrint: String =
    s"(If (${PrettyPhrasePrinter(cond)})" +
      s"{ ${PrettyPhrasePrinter(firstCase)} }" +
    s"else { ${PrettyPhrasePrinter(secondCase)} })"

  override def xmlPrinter: Elem = ???

  override def fedeTranslation(
    env: scala.Predef.Map[Identifier[ExpType],
    Identifier[AccType]]
  )(
    C: Phrase[AccType ->: AccType]
  ) : Phrase[AccType] = ???


  override def acceptorTranslation(A: Phrase[AccType])(
    implicit context: TranslationContext
  ): Phrase[CommType] = {
    import shine.DPIA.Compilation.TranslationToImperative._

    con(input)(fun(expT(dt1, read))(input =>
      con(cond)(fun(expT(bool, read))(cond =>
        //TODO: Can this also be used on the imperative level?
        // In C this would something like: A; (cond) ? firstCase(input) : secondCase(input)
        IfExpression(cond, acc(firstCasegessen(input))(A), acc(secondCase(input))(A))
      ))
    ))
  }
  //TODO: Figure out what kind of C code we want here.
  // Then, create Primitives if missing! (if we cannot generate C code from this
  // primitive immediately).


  override def continuationTranslation(C: Phrase[ExpType ->: CommType])(
    implicit context: TranslationContext
  ): Phrase[CommType] = ???

}

