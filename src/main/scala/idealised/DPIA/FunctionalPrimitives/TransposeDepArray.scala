package idealised.DPIA.FunctionalPrimitives

import idealised.DPIA.Compilation.{TranslationContext, TranslationToImperative}
import idealised.DPIA.DSL._
import idealised.DPIA.Phrases._
import idealised.DPIA.Semantics.OperationalSemantics._
import idealised.DPIA.Types._
import idealised.DPIA._

import scala.xml.Elem

case class TransposeDepArray(n:Nat, m:Nat, i:NatIdentifier, dt:DataType, array:Phrase[ExpType]) extends ExpPrimitive {

  private def dt(x:Nat):DataType = DataType.substitute(x, `for`=i, `in`=this.dt)

  override val t: ExpType = {
    (n: Nat) ->: (m: Nat) ->: (i: Nat) ->: (dt: DataType) ->:
      (array :: exp"[$n.${DepArrayType(m, k => dt(k))}]") ->: exp"[${DepArrayType(m, k => ArrayType(n, dt(k)))}]"
  }

  override def visitAndRebuild(f: VisitAndRebuild.Visitor): Phrase[ExpType] = {
    TransposeDepArray(f.nat(n), f.nat(m), f.nat(i).asInstanceOf[NatIdentifier], f.data(dt), VisitAndRebuild(array, f))
  }

  override def acceptorTranslation(A: Phrase[AccType])(implicit context: TranslationContext): Phrase[CommType] = {
    ???
  }

  override def mapAcceptorTranslation(f: Phrase[ExpType ->: ExpType], A: Phrase[AccType])(implicit context: TranslationContext): Phrase[CommType] = ???


  override def continuationTranslation(C: Phrase[ExpType ->: CommType])(implicit context: TranslationContext): Phrase[CommType] = {
    import TranslationToImperative._
    con(array)(λ(exp"[$n.${DepArrayType(m, k => dt(k))}]")(x => C(TransposeDepArray(n, m, i, dt, x))))
  }

  override def xmlPrinter: Elem = {
    <transposeArrayDep n={ToString(n)} m={ToString(m)} i={ToString(i)} dt={ToString(dt)}>
      {Phrases.xmlPrinter(array)}
    </transposeArrayDep>
  }

  override def prettyPrint: String = s"(transposeArrayDep $n $m ($i -> $dt) ${PrettyPhrasePrinter(array)}"

  override def eval(s: Store): Data = ???
}
