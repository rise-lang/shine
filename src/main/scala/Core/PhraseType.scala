package Core

import apart.arithmetic.{ArithExpr, NamedVar}

sealed trait PhraseType

abstract class BasePhraseTypes extends PhraseType

final case class ExpType(dataType: DataType) extends BasePhraseTypes

final case class AccType(dataType: DataType) extends BasePhraseTypes

final case class CommandType() extends PhraseType

final case class PairType[T1 <: PhraseType, T2 <: PhraseType](t1: T1, t2: T2) extends PhraseType

final case class FunctionType[T1 <: PhraseType, T2 <: PhraseType](inT: T1, outT: T2) extends PhraseType

final case class PassiveFunctionType[T1 <: PhraseType, T2 <: PhraseType](inT: T1, outT: T2)
  extends PhraseType

final case class NatDependentFunctionType[T <: PhraseType](x: NamedVar, outT: T) extends PhraseType

// convenience types for writing the phrase types more readable
object PhraseType {
  // TODO: move into package object ...

  type x[T1 <: PhraseType, T2 <: PhraseType] = PairType[T1, T2]
  type ->[T1 <: PhraseType, T2 <: PhraseType] = FunctionType[T1, T2]
  type `->p`[T1 <: PhraseType, T2 <: PhraseType] = PassiveFunctionType[T1, T2]
  type `(nat)->`[T <: PhraseType] = NatDependentFunctionType[T]
  type VarType = ExpType x AccType

  implicit class PairTypeConstructor[T1 <: PhraseType](t1: T1) {
    def x[T2 <: PhraseType](t2: T2) = PairType(t1, t2)
  }

  implicit class FunctionTypeConstructor[T1 <: PhraseType](t1: T1) {
    def ->[T2 <: PhraseType](t2: T2) = FunctionType(t1, t2)
  }

  implicit class PassiveFunctionTypeConstructor[T1 <: PhraseType](t1: T1) {
    def `->p`[T2 <: PhraseType](t2: T2) = PassiveFunctionType(t1, t2)
  }

  implicit class NatDependentFunctionTypeConstructor(x: NamedVar) {
    def ->[T <: PhraseType](outT: T) = NatDependentFunctionType(x, outT)
  }

  def substitute[T <: PhraseType](ae: ArithExpr,
                                  `for`: NamedVar,
                                  in: Phrase[T]): Phrase[T] = {

    case class fun() extends VisitAndRebuild.fun {
      override def apply[T2 <: PhraseType](p: Phrase[T2]): Result[Phrase[T2]] = {
        p.t = substitute(ae, `for`, p.t).asInstanceOf[T2]
        Continue(p, this)
      }

      override def apply(e: ArithExpr) = substitute(ae, `for`, e)

      override def apply(dt: DataType) = substitute(ae, `for`, dt)
    }

    val p = VisitAndRebuild(in, fun())
    println(p)
    TypeChecker(p)
    p
  }

  def substitute(ae: ArithExpr, `for`: NamedVar, in: PhraseType): PhraseType = {
    in match {
      case b: BasePhraseTypes => b match {
        case e: ExpType => ExpType(substitute(ae, `for`, e.dataType))
        case a: AccType => AccType(substitute(ae, `for`, a.dataType))
      }
      case c: CommandType => c
      case p: PairType[_, _] =>
        PairType(substitute(ae, `for`, p.t1), substitute(ae, `for`, p.t2))
      case f: FunctionType[_, _] =>
        FunctionType(substitute(ae, `for`, f.inT), substitute(ae, `for`, f.outT))
      case pf: PassiveFunctionType[_, _] =>
        PassiveFunctionType(substitute(ae, `for`, pf.inT), substitute(ae, `for`, pf.outT))
      case nf: NatDependentFunctionType[_] =>
        NatDependentFunctionType(nf.x, substitute(ae, `for`, nf.outT))
    }
  }

  def substitute(ae: ArithExpr, `for`: NamedVar, in: DataType): DataType = {
    in match {
      case b: BasicType => b
      case a: ArrayType =>
        ArrayType(ArithExpr.substitute(a.size, Map( (`for`, ae) )),
          substitute(ae, `for`, a.elemType))
      case r: RecordType =>
        RecordType(substitute(ae, `for`, r.fst), substitute(ae, `for`, r.snd))
    }
  }

  def substitute(ae: ArithExpr, `for`: NamedVar, in: ArithExpr): ArithExpr = {
   ArithExpr.substitute(in, Map( (`for`, ae) ))
  }

}
