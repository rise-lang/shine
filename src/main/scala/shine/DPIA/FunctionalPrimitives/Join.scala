package shine.DPIA.FunctionalPrimitives

import shine.DPIA.Compilation.{TranslationContext, TranslationToImperative}
import shine.DPIA.DSL.{λ, _}
import shine.DPIA.ImperativePrimitives.{JoinAcc, MkDPairFstI}
import shine.DPIA.Phrases._
import shine.DPIA.Semantics.OperationalSemantics
import shine.DPIA.Semantics.OperationalSemantics._
import shine.DPIA.Types._
import shine.DPIA.Types.DataType._
import shine.DPIA._

import scala.xml.Elem

final case class Join(
  n: Nat,
  m: Nat,
  w: AccessType,
  dt: DataType,
  array: Phrase[ExpType]
) extends ExpPrimitive {

  array :: expT(n`.`(m`.`dt), w)
  override val t: ExpType = expT({n * m}`.`dt, w)

  override def visitAndRebuild(fun: VisitAndRebuild.Visitor): Phrase[ExpType] = {
    Join(fun.nat(n), fun.nat(m), fun.access(w), fun.data(dt), VisitAndRebuild(array, fun))
  }

  override def eval(s: Store): Data = {
    OperationalSemantics.eval(s, array) match {
      case ArrayData(outer) =>
        val arrays = outer.map {
          case ArrayData(inner) => inner
          case _ => throw new Exception("This should not happen")
        }
        ArrayData(arrays.flatten)

      case _ => throw new Exception("This should not happen")
    }
  }

  override def prettyPrint: String = s"(join ${PrettyPhrasePrinter(array)})"

  override def xmlPrinter: Elem =
    <join n={ToString(n)} m={ToString(m)} w={ToString(w)} dt={ToString(dt)}>
      {Phrases.xmlPrinter(array)}
    </join>

  override def fedeTranslation(env: scala.Predef.Map[Identifier[ExpType], Identifier[AccType]])
                     (C: Phrase[AccType ->: AccType]) : Phrase[AccType] = {
    import TranslationToImperative._

    val otype = C.t.inT.dataType
    fedAcc(env)(array)(λ(accT(otype))(o => JoinAcc(n, m, dt, C(o))))
  }

  override def acceptorTranslation(A: Phrase[AccType])
                                  (implicit context: TranslationContext): Phrase[CommType] = {
    import TranslationToImperative._

    acc(array)(JoinAcc(n, m, dt, A))
  }

  override def continuationTranslation(C: Phrase[ExpType ->: CommType])
                                      (implicit context: TranslationContext): Phrase[CommType] = {
    import TranslationToImperative._

    con(array)(λ(expT(n`.`(m`.`dt), read))(x => C(Join(n, m, w, dt, x)) ))
  }
}

final case class DPairJoin(
                          n: Nat,
                          ns: NatCollectionIdentifier,
                          ft: NatToData,
                          m: NatIdentifier,
                          w: AccessType,
                          dt: DataType,
                          pair: Phrase[ExpType]
                          ) extends ExpPrimitive {

  val t = expT(DepPairType[NatKind](m, dt), w)

  override def visitAndRebuild(f: VisitAndRebuild.Visitor): Phrase[ExpType] =
    DPairJoin(
      f.nat(n),
      f.natCollection(ns),
      f.natToData(ft),
      f.nat(m),
      f.access(w),
      f.data(dt),
      VisitAndRebuild(pair, f)
    )

  override def continuationTranslation(C: Phrase[ExpType ->: CommType])(implicit context: TranslationContext): Phrase[CommType] = {
    import TranslationToImperative._
    con(pair)(λ(expT(DepPairType[NatCollectionKind](ns, DepArrayType(n, ft)), w))(x => C(DPairJoin(n, ns, ft, m, w, dt, x))))
  }

  override def acceptorTranslation(A: Phrase[AccType])(implicit context: TranslationContext): Phrase[CommType] = {
    import TranslationToImperative._
    // We must cheat...
    val rns = rise.core.types.NatCollectionIdentifier(this.ns.name, isExplicit = true)
    MkDPairFstI[NatKind](rns `@` (n+1), A) `;`
    acc(pair)(DepPairJoinAcc(n, ns, ft, m, dt, A))
  }

  override def eval(s: Store): Data = ???

  override def prettyPrint: String = ""

  override def xmlPrinter: Elem = <dPairJoin></dPairJoin>
}

final case class DepPairJoinAcc(
                                 n: Nat,
                                 ns: NatCollectionIdentifier,
                                 ft: NatToData,
                                 m: NatIdentifier,
                                 dt: DataType,
                                 pair: Phrase[AccType]
                               ) extends AccPrimitive {
  override val t: AccType = accT(DepPairType[NatCollectionKind](ns, DepArrayType(n, ft)))

  override def eval(s: Store): AccIdentifier = ???

  override def prettyPrint: String = ""

  override def xmlPrinter: Elem = <dPairJoinAAcc></dPairJoinAAcc>

  override def visitAndRebuild(f: VisitAndRebuild.Visitor): Phrase[AccType] = {
    DepPairJoinAcc(
      f.nat(n),
      f.natCollection(ns),
      f.natToData(ft),
      f.nat(m),
      f.data(dt),
      VisitAndRebuild(pair, f)
    )
  }
}