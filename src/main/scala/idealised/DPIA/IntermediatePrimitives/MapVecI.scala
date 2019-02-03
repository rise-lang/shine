package idealised.DPIA.IntermediatePrimitives

import idealised.DPIA.Compilation.{TranslationContext, SubstituteImplementations}
import idealised.DPIA.DSL._
import idealised.DPIA.Phrases._
import idealised.DPIA.Semantics.OperationalSemantics
import idealised.DPIA.Semantics.OperationalSemantics._
import idealised.DPIA.Types._
import idealised.DPIA._
import idealised.OpenMP.DSL.parForVec

import scala.xml.Elem

final case class MapVecI(n: Nat,
                         st1: ScalarType,
                         st2: ScalarType,
                         f: Phrase[ExpType -> (AccType -> CommandType)],
                         in: Phrase[ExpType],
                         out: Phrase[AccType])
  extends CommandPrimitive with Intermediate[CommandType]
{
  override val `type`: CommandType =
    (n: Nat) -> (st1: DataType) -> (st2: DataType) ->
      (f :: t"exp[$st1] -> acc[$st2] -> comm") ->
        (in :: exp"[${VectorType(n, st1)}]") ->
          (out :: acc"[${VectorType(n, st2)}]") ->
            comm

  override def eval(s: Store): Store = {
    val fE = OperationalSemantics.eval(s, f)(BinaryFunctionEvaluator)
    val n = in.t match {
      case ExpType(VectorType(len, _)) => len
    }

    (0 until n.eval).foldLeft(s)((sOld, i) => {
      val comm = fE(in `@v` Literal(i))(out `@v` Literal(i))
      OperationalSemantics.eval(sOld, comm)
    })
  }

  override def substituteImpl(env: SubstituteImplementations.Environment)
                             (implicit context: TranslationContext): Phrase[CommandType] = {
    parForVec(n, st2, out, i => a =>
      SubstituteImplementations(f(in `@v` i)(a), env)
    )
  }

  override def visitAndRebuild(fun: VisitAndRebuild.Visitor): Phrase[CommandType] = {
    MapVecI(fun(n), fun(st1), fun(st2),
      VisitAndRebuild(f, fun),
      VisitAndRebuild(in, fun),
      VisitAndRebuild(out, fun))
  }

  override def prettyPrint =
    s"(MapVecI ${PrettyPhrasePrinter(out)} ${PrettyPhrasePrinter(f)} ${PrettyPhrasePrinter(in)})"

  override def xmlPrinter: Elem =
    <mapVecI n={ToString(n)} st1={ToString(st1)} st2={ToString(st2)}>
      <output type={ToString(AccType(ArrayType(n, st2)))}>
        {Phrases.xmlPrinter(out)}
      </output>
      <f type={ToString(AccType(st2) -> (ExpType(st1) -> CommandType()))}>
        {Phrases.xmlPrinter(f)}
      </f>
      <input type={ToString(ExpType(ArrayType(n, st1)))}>
        {Phrases.xmlPrinter(in)}
      </input>
    </mapVecI>
}
