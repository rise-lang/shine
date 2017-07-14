package idealised.DPIA.FunctionalPrimitives

import idealised.DPIA.Compilation.RewriteToImperative
import idealised.DPIA.DSL._
import idealised.DPIA.ImperativePrimitives.UnzipAcc
import idealised.DPIA.Phrases._
import idealised.DPIA.Semantics.OperationalSemantics
import idealised.DPIA.Semantics.OperationalSemantics._
import idealised.DPIA.Types._
import idealised.DPIA.{Phrases, _}

import scala.xml.Elem

final case class Unzip(n: Nat,
                       dt1: DataType,
                       dt2: DataType,
                       e: Phrase[ExpType])
  extends ExpPrimitive {

  override val `type`: ExpType =
    (n: Nat) -> (dt1: DataType) -> (dt2: DataType) ->
      (e :: exp"[$n.($dt1 x $dt2)]") ->
      ExpType(RecordType(ArrayType(n, dt1), ArrayType(n, dt2)))

  // TODO: fix parsing of this:
//        exp"[($n.$dt1 x $n.$dt2)]"

  override def visitAndRebuild(f: VisitAndRebuild.Visitor): Phrase[ExpType] = {
    Unzip(f(n), f(dt1), f(dt2), VisitAndRebuild(e, f))
  }

  override def eval(s: Store): Data = {
    OperationalSemantics.eval(s, e) match {
      case ArrayData(xs) =>
        val (lhs, rhs) = xs.foldLeft(Vector[Data](), Vector[Data]()){
          case (vs: (Vector[Data], Vector[Data]), p: RecordData) =>
            (vs._1 :+ p.fst, vs._2 :+ p.snd)
          case _ => throw new Exception("This should not happen")
        }
        RecordData(ArrayData(lhs), ArrayData(rhs))

      case _ => throw new Exception("This should not happen")
    }
  }

  override def prettyPrint: String = s"(unzip ${PrettyPhrasePrinter(e)})"

  override def xmlPrinter: Elem =
    <unzip n={ToString(n)} dt1={ToString(dt1)} dt2={ToString(dt2)}>
      <e type={ToString(ExpType(ArrayType(n, dt1)))}>
        {Phrases.xmlPrinter(e)}
      </e>
    </unzip>

  override def acceptorTranslation(A: Phrase[AccType]): Phrase[CommandType] = {
    import RewriteToImperative._

    con(e)(λ(exp"[$n.($dt1 x $dt2)]")(x =>
      A :=|RecordType(ArrayType(n, dt1), ArrayType(n, dt2))| Unzip(n, dt1, dt2, x)
    ))
  }

  override def continuationTranslation(C: Phrase[ExpType -> CommandType]): Phrase[CommandType] = {
    import RewriteToImperative._

    con(e)(λ(exp"[$n.($dt1 x $dt2)]")(x =>
      C(Unzip(n, dt1, dt2, x)) ))
  }
}