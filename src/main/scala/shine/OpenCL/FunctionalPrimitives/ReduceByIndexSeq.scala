package shine.OpenCL.FunctionalPrimitives

import shine.DPIA.Compilation._
import shine.DPIA.DSL._
import shine.DPIA.Phrases._
import shine.DPIA.Semantics.OperationalSemantics._
import shine.DPIA.Types._
import shine.DPIA.Types.DataType._
import shine.DPIA._
import shine.OpenCL.IntermediatePrimitives.{ReduceByIndexSeqI}

import scala.xml.Elem

final case class ReduceByIndexSeq(
                                  n: Nat,
                                  k: Nat,
                                  dt: DataType,
                                  f: Phrase[ExpType ->: ExpType ->: ExpType],
                                  hist: Phrase[ExpType],
                                  is: Phrase[ExpType],
                                  xs: Phrase[ExpType],
                                ) extends ExpPrimitive {

  f :: expT(dt, read) ->: expT(dt, read) ->: expT(dt, write)
  hist :: expT(k`.`dt, read)
  is :: expT(k`.`NatType, read)
  xs :: expT(n`.`dt, read)
  override val t: ExpType = expT(k`.`dt, read)

  override def visitAndRebuild(
                                fun: VisitAndRebuild.Visitor
                              ): Phrase[ExpType] = {
    ReduceByIndexSeq(fun.nat(n), fun.nat(k), fun.data(dt),
      VisitAndRebuild(f, fun), VisitAndRebuild(hist, fun),
      VisitAndRebuild(is, fun), VisitAndRebuild(xs, fun))
  }

  override def eval(s: Store): Data = ???

  override def prettyPrint: String =
      s"(${PrettyPhrasePrinter(f)}) (${PrettyPhrasePrinter(hist)})" +
      s"(${PrettyPhrasePrinter(is)}) (${PrettyPhrasePrinter(xs)})"

  override def acceptorTranslation(A: Phrase[AccType])(
    implicit context: TranslationContext
  ): Phrase[CommType] = ???

  override def continuationTranslation(C: Phrase[ExpType ->: CommType])(
    implicit context: TranslationContext
  ): Phrase[CommType] = {
    import TranslationToImperative._

    con(xs)(λ(expT(n`.`dt, read))(X =>
      con(is)(λ(expT(n`.`NatType, read))(I =>
        con(hist)(λ(expT(k`.`dt, read))(H =>
      ReduceByIndexSeqI(n, k, dt,
        λ(expT(dt, read))(x =>
          λ(expT(dt, read))(y =>
            λ(accT(dt))(o => acc( f(x)(y) )( o )))),
        H, I, X, C)(context)))))))
  }

  override def xmlPrinter: Elem =
    <reduce n={ToString(n)} k={ToString(n)} dt={ToString(dt)}>
      <f type={ToString(
        ExpType(dt, read) ->: (ExpType(dt, read) ->: ExpType(dt, write)))}>
        {Phrases.xmlPrinter(f)}
      </f>
      <hist type={ToString(ExpType(ArrayType(k, dt), read))}>
        {Phrases.xmlPrinter(hist)}
      </hist>
      <is type={ToString(ExpType(ArrayType(n, NatType), read))}>
        {Phrases.xmlPrinter(is)}
      </is>
      <xs type={ToString(ExpType(ArrayType(n, dt), read))}>
        {Phrases.xmlPrinter(is)}
      </xs>
    </reduce>

}
