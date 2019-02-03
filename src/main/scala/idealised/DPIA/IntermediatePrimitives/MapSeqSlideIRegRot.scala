package idealised.DPIA.IntermediatePrimitives

import idealised.DPIA.Compilation.{TranslationContext, SubstituteImplementations}
import idealised.DPIA._
import idealised.DPIA.DSL._
import idealised.DPIA.FunctionalPrimitives.{Drop, Take}
import idealised.DPIA.ImperativePrimitives._
import idealised.DPIA.Types._
import idealised.DPIA.Phrases._
import idealised.DPIA.Semantics.OperationalSemantics.Store

import scala.language.reflectiveCalls

// TODO: factorise with MapSeqSlideICircular?
final case class MapSeqSlideIRegRot(n: Nat,
                                    size: Nat,
                                    // step: Nat,
                                    dt1: DataType,
                                    dt2: DataType,
                                    f: Phrase[ExpType -> (AccType -> CommandType)],
                                    input: Phrase[ExpType],
                                    output: Phrase[AccType])
  extends CommandPrimitive with Intermediate[CommandType]
{
  private val step = 1
  private val inputSize = step * n + size - step

  override def `type`: CommandType =
    (n: Nat) -> (size: Nat) -> // (step: Nat) ->
      (dt1: DataType) -> (dt2: DataType) ->
      (f :: t"exp[$size.$dt1] -> acc[$dt2] -> comm") ->
      (input :: exp"[$inputSize.$dt1]") ->
      (output :: acc"[$n.$dt2]") ->
      comm

  override def eval(s: Store): Store = ???

  override def visitAndRebuild(v: VisitAndRebuild.Visitor): Phrase[CommandType] = {
    MapSeqSlideIRegRot(v(n), v(size), v(dt1), v(dt2),
      VisitAndRebuild(f, v),
      VisitAndRebuild(input, v),
      VisitAndRebuild(output, v))
  }

  override def substituteImpl(env: SubstituteImplementations.Environment)
                             (implicit context: TranslationContext): Phrase[CommandType] = {
    NewRegRot(size, dt1,
      fun(exp"[$size.$dt1]" x acc"[$size.$dt1]")(rs =>
        fun(comm : CommandType)(rotate => {
          SubstituteImplementations({
            // prologue initialisation
            MapSeqI(size - 1, dt1, dt1, fun(ExpType(dt1))(exp => fun(AccType(dt1))(acc => acc :=|dt1| exp)),
              Take(size - 1, inputSize, dt1, input),
              TakeAcc(size - 1, size, dt1, rs.wr)) `;`
            // core loop
            ForNat(n, _Λ_(i => {
              // load current value
              ((rs.wr `@` (size - 1)) :=|dt1| (Drop(size - 1, inputSize, dt1, input) `@` i)) `;`
              f(rs.rd)(output `@` i) `;` // body
              rotate
            }))
          }, env)
        })))
  }

  override def prettyPrint: String = s"(mapSeqSlideIRegRot $f $input $output)"

  override def xmlPrinter: xml.Elem =
    <mapSeqSlideIRegRot n={ToString(n)} size={ToString(size)} dt1={ToString(dt1)} dt2={ToString(dt2)}>
      <f>{Phrases.xmlPrinter(f)}</f>
      <input>{Phrases.xmlPrinter(input)}</input>
      <output>{Phrases.xmlPrinter(output)}</output>
    </mapSeqSlideIRegRot>
}