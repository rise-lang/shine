package shine.DPIA

import shine.DPIA.Phrases._
import shine.DPIA.Types._
import shine.DPIA.primitives.imperative._
import shine.DPIA.primitives.functional._

sealed trait ShowTypes
case object On extends ShowTypes
case object Off extends ShowTypes

object showDPIA {
  def apply[T <: PhraseType](p : Phrase[T]) : String = showDPIA().showPhrase(p)
  def apply[T <: PhraseType](p : Phrase[T], showTypes : ShowTypes) : String = showDPIA(showTypes).showPhrase(p)
}

case class showDPIA(showTypes : ShowTypes = Off) {
  val TAB : String = "  "
  def tab : String => String =
    _.linesIterator.map(l => if (l.isEmpty) l else TAB + l).mkString("\n")

  def showTypeId : Kind.Identifier => String = _.name
  def showKind : Kind[_, _] => String = _.name
  def showUnary : Operators.Unary.Value => String = _.toString
  def showBinary : Operators.Binary.Value => String = _.toString
  def showData : Data => String = _.toString
  def showNat : Nat => String = _.toString
  def showPhraseType : PhraseType => String = _.toString
  def showDataType : DataType => String = _.toString
  def showAddressSpace : AddressSpace => String = _.toString
  def showAccess : AccessType => String = _.toString
  def showNatToNat : NatToNat => String = _.toString
  def showNatToData : NatToData => String = _.toString
  def showType : PhraseType => String = _.toString
  def showType[T](kind : Kind[T, _], arg : T) : String = kind match {
    case PhraseKind => showPhraseType(arg.asInstanceOf[PhraseType])
    case DataKind => showDataType(arg.asInstanceOf[DataType])
    case NatKind => showNat(arg.asInstanceOf[Nat])
    case AddressSpaceKind => showAddressSpace(arg.asInstanceOf[AddressSpace])
    case AccessKind => showAccess(arg.asInstanceOf[AccessType])
    case NatToNatKind => showNatToNat(arg.asInstanceOf[NatToNat])
    case NatToDataKind => showNatToData(arg.asInstanceOf[NatToData])
  }
  def showPrim : Primitive[_] => String = {
    case Comment(s) => s"/* ${s} */"
    case Seq(c1, c2) => s"${showPhrase(c1)};\n${showPhrase(c2)}"
    case New(dt, f) => s"new (exp[${showDataType(dt)}] x acc[${showDataType(dt)}]) in \n${tab(showPhrase(f))}"
    case Assign(_, lhs, rhs) => s"${showTypedPhrase(lhs)} = ${showPhrase(rhs)}"
    case IdxAcc(_, _, idx, array) => s"${showTypedPhrase(array)}[${showPhrase(idx)}]"
    case Idx(_, _, idx, array) => s"${showTypedPhrase(array)}[${showPhrase(idx)}]"
    case NatAsIndex(_, e) => showTypedPhrase(e)
    case Drop(n, _, _, e) => s"drop ${showNat(n)} ${showTypedPhrase(e)}"
    case DropAcc(n, _, _, e) => s"drop ${showNat(n)} ${showTypedPhrase(e)}"
    case Take(n, _, _, e) => s"take ${showNat(n)} ${showTypedPhrase(e)}"
    case TakeAcc(n, _, _, e) => s"take ${showNat(n)} ${showTypedPhrase(e)}"
    case f : For => s"for ${showNat(f.n)}\n${tab(showPhrase(f.loopBody))}"
    case f : ForNat => s"forNat ${showNat(f.n)}\n${tab(showPhrase(f.loopBody))}"
    case p => p.toString
  }

  def showKindedType[T](kind : Kind[T, _], t : T) : String = showTypes match {
    case On => s"(${showType(kind, t)} : ${showKind(kind)})"
    case Off => s"(${showType(kind, t)})"
  }
  def showTypedPhrase[T <: PhraseType](p : Phrase[T]) : String = showTypes match {
    case On => s"(${showPhrase(p)} : ${showType(p.t)})"
    case Off => s"(${showPhrase(p)})"
  }
  def showPhrase[T <: PhraseType] : Phrase[T] => String = {
    case Identifier(name, t) => if (showTypes == On) s"(${name} : ${t})" else name
    case Lambda(param, body) => s"λ${showTypedPhrase(param)}.\n${tab(showTypedPhrase(body))}\n"
    case Apply(fun, arg) => s"${showTypedPhrase(fun)} ${showTypedPhrase(arg)}"
    case DepLambda(kind, x, body) => s"Λ${showKindedType(kind, x)}.\n${tab(showTypedPhrase(body))}\n"
    case DepApply(kind, fun, arg) => s"${showTypedPhrase(fun)} ${showKindedType(kind, arg)}"
    case LetNat(binder, defn, body) => s"let ${showTypeId(binder.id)} = ${showTypedPhrase(defn)} in \n${tab(showTypedPhrase(body))}\n"
    case PhrasePair(fst, snd) => s"${showTypedPhrase(fst)}, ${showTypedPhrase(snd)}"
    case Proj1(pair) => s"fst ${showTypedPhrase(pair)}"
    case Proj2(pair) => s"snd ${showTypedPhrase(pair)}"
    case IfThenElse(cond, thenP, elseP) => s"if ${showTypedPhrase(cond)} then ${showTypedPhrase(thenP)} else ${showTypedPhrase(elseP)}"
    case UnaryOp(op, p) => s"${showUnary(op)} ${showTypedPhrase(p)}"
    case BinOp(op, lhs, rhs) => s"${showTypedPhrase(lhs)} ${showBinary(op)} ${showTypedPhrase(rhs)}"
    case Literal(d) => showData(d)
    case Natural(d) => showNat(d)
    case primitive: Primitive[_] => showPrim(primitive)
  }

}
