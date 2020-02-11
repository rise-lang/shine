package shine.DPIA.FunctionalPrimitives

import shine.DPIA.Compilation.TranslationContext
import shine.DPIA.DSL._
import shine.DPIA.IntermediatePrimitives.IterateIAcc
import shine.DPIA.Phrases._
import shine.DPIA.Semantics.OperationalSemantics._
import shine.DPIA.Types._
import shine.DPIA.Types.DataType._
import shine.DPIA.{Phrases, _}

import scala.xml.Elem

final case class Iterate(n: Nat,
                         m: Nat,
                         k: Nat,
                         dt: DataType,
                         f: Phrase[`(nat)->:`[ExpType ->: ExpType]],
                         array: Phrase[ExpType])
  extends ExpPrimitive {

  {
    val l = f.t.x
    f :: l ->: expT({l * n}`.`dt, read) ->: expT(l`.`dt, read)
    array :: expT({m * n.pow(k)}`.`dt, read)
  }
  override val t: ExpType = expT(m`.`dt, read)

  override def visitAndRebuild(fun: VisitAndRebuild.Visitor): Phrase[ExpType] = {
    Iterate(fun.nat(n), fun.nat(m), fun.nat(k), fun.data(dt), VisitAndRebuild(f, fun), VisitAndRebuild(array, fun))
  }

  override def eval(s: Store): Data = {
//    val fE = OperationalSemantics.eval(s, f)
//    OperationalSemantics.eval(s, array) match {
//      case ArrayData(xs) =>
//        var a = array
//        for (i <- 0 until k.eval) {
//          a = fE(a)
//        }
//        OperationalSemantics.eval(s, a)
//      case _ => throw new Exception("This should not happen")
//    }
    ???
  }

  override def xmlPrinter: Elem = {
    val l = f.t.x
    <iterate n={ToString(n)} m={ToString(m)} k={ToString(k)} dt={ToString(dt)}>
      <f type={ToString(l ->: ExpType(ArrayType(l, dt), read) ->: ExpType(ArrayType(l /^ n, dt), read))}>
        {Phrases.xmlPrinter(f)}
      </f>
      <input type={ToString(ExpType(ArrayType(m, dt), read))}>
        {Phrases.xmlPrinter(array)}
      </input>
    </iterate>
  }

  override def prettyPrint: String =
    s"(iterate $k ${PrettyPhrasePrinter(f)} ${PrettyPhrasePrinter(array)})"

  override def acceptorTranslation(A: Phrase[AccType])
                                  (implicit context: TranslationContext): Phrase[CommType] = {
    import shine.DPIA.Compilation.TranslationToImperative._

    con(array)(λ(expT({m * n.pow(k)}`.`dt, read))(x =>
      IterateIAcc(n, m, k, dt, A,
        _Λ_[NatKind]()(l => λ(accT(l`.`dt))(o => λ(expT({l * n}`.`dt, read))(x => acc(f(l)(x))(o)))),
        x) ))
  }

  override def continuationTranslation(C: Phrase[ExpType ->: CommType])
                                      (implicit context: TranslationContext): Phrase[CommType] = {

    ???
  }
}
