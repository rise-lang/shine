package idealised.DPIA.FunctionalPrimitives

import idealised.DPIA.Compilation.TranslationToImperative.acc
import idealised.DPIA.Compilation.{CodeGenerator, TranslationContext, TranslationToImperative}
import idealised.DPIA.DSL._
import idealised.DPIA.ImperativePrimitives.SplitAcc
import idealised.DPIA.Phrases._
import idealised.DPIA.Semantics.OperationalSemantics
import idealised.DPIA.Semantics.OperationalSemantics._
import idealised.DPIA.Types._
import idealised.DPIA._

import scala.xml.Elem

final case class Split(n: Nat,
                       m: Nat,
                       w: AccessType,
                       dt: DataType,
                       array: Phrase[ExpType])
  extends ExpPrimitive {

  override val t: ExpType =
    (n: Nat) ->: (m: Nat) ->: (w: AccessType) ->: (dt: DataType) ->:
      (array :: exp"[${m * n}.$dt, $w]") ->: exp"[$m.$n.$dt, $w]"

  override def visitAndRebuild(fun: VisitAndRebuild.Visitor): Phrase[ExpType] = {
    Split(fun.nat(n), fun.nat(m), fun.access(w), fun.data(dt), VisitAndRebuild(array, fun))
  }

  override def eval(s: Store): Data = {
    OperationalSemantics.eval(s, array) match {
      case ArrayData(arrayE) =>

        def split[T](n: Int, vector: Vector[T]): Vector[Vector[T]] = {
          val builder = Vector.newBuilder[Vector[T]]
          var vec = vector
          while (vec.nonEmpty) {
            builder += vec.take(n)
            vec = vec.drop(n)
          }
          builder.result()
        }

        ArrayData(split(n.eval, arrayE).map(ArrayData))

      case _ => throw new Exception("This should not happen")
    }
  }

  override def prettyPrint: String = s"(split $n ${PrettyPhrasePrinter(array)})"

  override def xmlPrinter: Elem =
    <split n={ToString(n)} m={ToString(m)} dt={ToString(dt)}>
      {Phrases.xmlPrinter(array)}
    </split>

  override def fedeTranslation(env: scala.Predef.Map[Identifier[ExpType], Identifier[AccType]])
                     (C: Phrase[AccType ->: AccType]) : Phrase[AccType] = {
    import TranslationToImperative._

    val otype = C.t.inT.dataType
    fedAcc(env)(array)(λ(acc"[$otype]")(o => SplitAcc(n, m, dt, C(o))))
  }

  override def acceptorTranslation(A: Phrase[AccType])
                                  (implicit context: TranslationContext): Phrase[CommType] = {
    import TranslationToImperative._

    acc(array)(SplitAcc(n, m, dt, A))
  }

  // TODO?
  override def mapAcceptorTranslation(g: Phrase[ExpType ->: ExpType], A: Phrase[AccType])
                                     (implicit context: TranslationContext): Phrase[CommType] = {
    import TranslationToImperative._

    // A : acc[m.n.float]
    // g : exp[n.float] -> exp[n.float]
    // ? : exp[float] -> exp[float]

    // mapAcc(fun(exp"[$dt, $read]")(x => g(x)), array)(A)
    ???
  }

  override def continuationTranslation(C: Phrase[ExpType ->: CommType])
                                      (implicit context: TranslationContext): Phrase[CommType] = {
    import TranslationToImperative._

    con(array)(λ(exp"[${m * n}.$dt, $read]")(x => C(Split(n, m, w, dt, x)) ))
  }
}