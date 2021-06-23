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

  def showKind : Kind[_, _, _] => String = _.name
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
  def showType[T](kind : Kind[T, _, _], arg : T) : String = kind match {
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
    case IterateStream(_, _, _, f, array) => s"iterate ${showPhrase(array)}\n${tab(showPhrase(f))}"
    case Map(_, _, _, _, f, array) => s"map ${showPhrase(array)} \n ${showPhrase(f)}"
    case f : MapSeq => s"map ${showPhrase(f.array)} \n ${showPhrase(f.f)}"
    case Join(_, _, _, _, array) => s"join ${showPhrase(array)}"
    case Fst(_, _, p) => s"fst ${showPhrase(p)}"
    case Snd(_, _, p) => s"snd ${showPhrase(p)}"
    case Transpose(_, _, _, _, array) => s"transpose ${showPhrase(array)}"
    case TransposeAcc(_, _, _, array) => s"transpose ${showPhrase(array)}"
    case MakePair(_, _, _, fst, snd) => s"makePair ${showPhrase(fst)} ${showPhrase(snd)}"
    case MapRead(_, _, _, f, array) => s"map ${showPhrase(array)} ${showPhrase(f)}"
    case Slide(_, sz, _, _, array) => s"slide ${showNat(sz)} ${showPhrase(array)}"
    case f : MakeArray => s"makeArray ${f.elements.map(showPhrase).mkString(",")}"
    case Generate(n, _, f) => s"generate ${showNat(n)} ${showPhrase(f)}"
    case Zip(_, _, _, _, e1, e2) => s"zip ${showPhrase(e1)} ${showPhrase(e2)}"
    case Cast(_, dt2, e) => s"cast ${showKindedType(DataKind, dt2)} ${showPhrase(e)}"
    case RotateValues(_,_,_,wrt,in) => s"rotate ${showPhrase(in)}\n${tab(showPhrase(wrt))}"
    case f : ReduceSeq => s"reduce ${showPhrase(f.array)} ${showPhrase(f.init)}\n${tab(showPhrase(f.f))}"
    case p => p.toString
  }

  def showKindedType[T](kind : Kind[T, _, _], t : T) : String = showTypes match {
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
    case LetNat(binder, defn, body) => s"let ${showKindedType(NatKind, binder.id)} = ${showTypedPhrase(defn)} in \n${tab(showTypedPhrase(body))}\n"
    case PhrasePair(fst, snd) => s"${showTypedPhrase(fst)}, ${showTypedPhrase(snd)}"
    case Proj1(pair) => s"proj1 ${showTypedPhrase(pair)}"
    case Proj2(pair) => s"proj2 ${showTypedPhrase(pair)}"
    case IfThenElse(cond, thenP, elseP) => s"if ${showTypedPhrase(cond)} then ${showTypedPhrase(thenP)} else ${showTypedPhrase(elseP)}"
    case UnaryOp(op, p) => s"${showUnary(op)} ${showTypedPhrase(p)}"
    case BinOp(op, lhs, rhs) => s"${showTypedPhrase(lhs)} ${showBinary(op)} ${showTypedPhrase(rhs)}"
    case Literal(d) => showData(d)
    case Natural(d) => showNat(d)
    case primitive: Primitive[_] => showPrim(primitive)
  }

}
