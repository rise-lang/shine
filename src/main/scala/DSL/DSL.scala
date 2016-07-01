package DSL

import Core.OperationalSemantics._
import Core._
import HighLevelCombinators._
import LowLevelCombinators._
import apart.arithmetic.{ArithExpr, NamedVar}

object `if` {
  def apply[T <: PhraseType](cond: Phrase[ExpType], thenP: Phrase[T], elseP: Phrase[T]) = {
    IfThenElsePhrase(cond, thenP, elseP)
  }
}

object `for` {
  def apply(n: ArithExpr, f: (Phrase[ExpType] => Phrase[CommandType])) = {
    For(n, λ(ExpType(int)) { i => f(i) })
  }
}

object `parFor` {
  def apply(n: ArithExpr,
            dt: DataType,
            out: Phrase[AccType],
            f: (Phrase[ExpType] => Phrase[AccType] => Phrase[CommandType])) = {
    ParFor(n, dt, out, λ(ExpType(int)) { i => λ(AccType(dt)) { o => f(i)(o) } })
  }
}

object dblBufFor {
  def apply(n: ArithExpr,
            dt: DataType,
            addressSpace: AddressSpace,
            buffer1: Phrase[VarType],
            buffer2: Phrase[VarType],
            k: ArithExpr,
            body: Phrase[`(nat)->`[AccType -> (ExpType -> CommandType)]],
            C: Phrase[ExpType -> CommandType]) = {
    DoubleBufferFor(n, dt, addressSpace, buffer1, buffer2, k, body, C)
  }
}

object `new` {
  def apply(dt: DataType, addressSpace: AddressSpace, f: Phrase[(ExpType x AccType) -> CommandType]) = New(dt, addressSpace, f)

  def apply(dt: DataType, addressSpace: AddressSpace, f: Phrase[ExpType x AccType] => Phrase[CommandType]) = {
    New(dt, addressSpace, λ(ExpType(dt) x AccType(dt)) { v => f(v) })
  }
}

object fst {
  def apply(record: Phrase[ExpType]) = Fst(null, null, record)
}

object snd {
  def apply(record: Phrase[ExpType]) = Snd(null, null, record)
}

object fstAcc {
  def apply(fstT: DataType, sndT: DataType, record: Phrase[AccType]) =
    FstAcc(fstT, sndT, record)
}

object sndAcc {
  def apply(fstT: DataType, sndT: DataType, record: Phrase[AccType]) =
    SndAcc(fstT, sndT, record)
}

object π1 {
  def apply[T1 <: PhraseType, T2 <: PhraseType](pair: Phrase[T1 x T2]) = Proj1Phrase(pair)
}

object π2 {
  def apply[T1 <: PhraseType, T2 <: PhraseType](pair: Phrase[T1 x T2]) = Proj2Phrase(pair)
}

object identifier {
  def exp(name: String) = IdentPhrase[ExpType](name, null)

  def acc(name: String) = IdentPhrase[AccType](name, null)

  def newVar(name: String) = IdentPhrase[ExpType x AccType](name, null)

  def newVar(name: String, dt: DataType) = {
    IdentPhrase[ExpType x AccType](name, PairType[ExpType, AccType](ExpType(dt), AccType(dt)))
  }

  def apply[T <: PhraseType](name: String, t: T) = {
    IdentPhrase[T](name, t)
  }
}

trait funDef {
  def apply[T <: PhraseType](f: IdentPhrase[ExpType] => Phrase[T]): LambdaPhrase[ExpType, T] = {
    val param = identifier.exp(newName())
    LambdaPhrase(param, f(param))
  }

  def apply[T <: PhraseType](f: (Phrase[ExpType], Phrase[ExpType]) => Phrase[T]): LambdaPhrase[ExpType x ExpType, T] = {
    val param = IdentPhrase[PairType[ExpType, ExpType]](newName(), null)
    val g = λ(PairType(ExpType(int), ExpType(int))) { x => f(π1(x), π2(x)) }
    LambdaPhrase(param, g(param))
  }

  def apply[T1 <: PhraseType, T2 <: PhraseType](t: T1)
                                               (f: IdentPhrase[T1] => Phrase[T2]): LambdaPhrase[T1, T2] = {
    val param = identifier(newName(), t)
    LambdaPhrase(param, f(param))
  }

  def apply[T1 <: PhraseType, T2 <: PhraseType](param: IdentPhrase[T1])
                                               (f: IdentPhrase[T1] => Phrase[T2]): LambdaPhrase[T1, T2] = {
    LambdaPhrase(param, f(param))
  }

  def apply[T1 <: PhraseType, T2 <: PhraseType](param: IdentPhrase[T1],
                                                body: Phrase[T2]): LambdaPhrase[T1, T2] = {
    LambdaPhrase(param, body)
  }
}

object fun extends funDef

object \ extends funDef

object λ extends funDef


trait natDependentFunDef {

  def apply[T <: PhraseType](f: NamedVar => Phrase[T]): NatDependentLambdaPhrase[T] = {
    val x = NamedVar(newName())
    NatDependentLambdaPhrase(x, f(x))
  }

}

object _Λ_ extends natDependentFunDef

object skip extends Skip

object map {
  def apply(f: Phrase[ExpType -> ExpType]): Phrase[ExpType -> ExpType] = λ(x => {
    map(f, x)
  })

  def apply(f: Phrase[ExpType -> ExpType], x: Phrase[ExpType]): HighLevelCombinators.Map = {
    HighLevelCombinators.Map(null, null, null, f, x)
  }
}

object mapSeq {
  def apply(f: Phrase[ExpType -> ExpType]): Phrase[ExpType -> ExpType] = λ(x =>
    mapSeq(f, x))

  def apply(f: Phrase[ExpType -> ExpType], x: Phrase[ExpType]) =
    MapSeq(null, null, null, f, x)
}

object mapWorkgroup {
  def apply(f: Phrase[ExpType -> ExpType]): Phrase[ExpType -> ExpType] = λ(x =>
    mapWorkgroup(f, x)
  )

  def apply(f: Phrase[ExpType -> ExpType], x: Phrase[ExpType]) =
    MapWorkgroup(null, null, null, f, x)
}

object mapLocal {
  def apply(f: Phrase[ExpType -> ExpType]): Phrase[ExpType -> ExpType] = λ(x =>
    mapLocal(f, x))

  def apply(f: Phrase[ExpType -> ExpType], x: Phrase[ExpType]) =
    MapLocal(null, null, null, f, x)
}

object zip {
  def apply(lhs: Phrase[ExpType], rhs: Phrase[ExpType]) =
    Zip(null, null, null, lhs, rhs)
}

object split {
  def apply(n: ArithExpr): Phrase[ExpType -> ExpType] =
    λ(array => split(n, array))

  def apply(n: ArithExpr, array: Phrase[ExpType]): Split =
    Split(n, null, null, array)
}

object join {
  def apply(): Phrase[ExpType -> ExpType] = λ(array => join(array))

  def apply(array: Phrase[ExpType]): Join = Join(null, null, null, array)
}

object toLocal {
  def apply(f: Phrase[ExpType -> ExpType]): Phrase[ExpType -> ExpType] =
    λ(x => toLocal(f, x))

  def apply(f: Phrase[ExpType -> ExpType], x: Phrase[ExpType]): ToLocal =
    ToLocal(null, null, f, x)
}

object toGlobal {
  def apply(f: Phrase[ExpType -> ExpType]): Phrase[ExpType -> ExpType] =
    λ(x => toGlobal(f, x))

  def apply(f: Phrase[ExpType -> ExpType], x: Phrase[ExpType]): ToGlobal =
    ToGlobal(null, null, f, x)
}

object reduce {
  def apply(f: Phrase[ExpType -> (ExpType -> ExpType)]): Phrase[(ExpType x ExpType) -> ExpType] =
    λ((init, array) => reduce(f, init, array))

  def apply(f: Phrase[ExpType -> (ExpType -> ExpType)], init: Phrase[ExpType]): Phrase[ExpType -> ExpType] =
    λ(array => reduce(f, init, array))

  def apply(f: Phrase[ExpType -> (ExpType -> ExpType)], init: Phrase[ExpType],
            array: Phrase[ExpType]): Reduce = {
    Reduce(null, null, null, f, init, array)
  }
}

object reduceSeq {
  def apply(f: Phrase[ExpType -> (ExpType -> ExpType)]): Phrase[(ExpType x ExpType) -> ExpType] =
    λ((init, array) => reduceSeq(f, init, array))

  def apply(f: Phrase[ExpType -> (ExpType -> ExpType)], init: Phrase[ExpType]): Phrase[ExpType -> ExpType] =
    λ(array => reduceSeq(f, init, array))

  def apply(f: Phrase[ExpType -> (ExpType -> ExpType)],
            init: Phrase[ExpType],
            array: Phrase[ExpType]) =
    ReduceSeq(null, null, null, f, init, array)
}

object iterate {
  def apply(k: ArithExpr, f: Phrase[`(nat)->`[ExpType -> ExpType]]): Phrase[ExpType -> ExpType] =
    λ(array => iterate(k, f, array))

  def apply(k: ArithExpr,
            f: Phrase[`(nat)->`[ExpType -> ExpType]],
            array: Phrase[ExpType]): Iterate = {
    Iterate(null, null, k, null, f, array)
  }
}

object gather {
  def apply(idxF: (ArithExpr, DataType) => ArithExpr) = λ(array =>
      Gather(null, null, idxF, array)
  )
}

object asVector {
  def apply(n: ArithExpr): Phrase[ExpType -> ExpType] = λ(array => asVector(n, array))

  def apply(n: ArithExpr, array: Phrase[ExpType]): AsVector =
      AsVector(n, null, null, array)
}

object asScalar {
  def apply(): Phrase[ExpType -> ExpType] = λ(array => asScalar(array))

  def apply(array: Phrase[ExpType]): AsScalar =
      AsScalar(null, null, null, array)
}

object vectorize {
  def apply(len: Int, f: Float) = LiteralPhrase(VectorData(Vector.fill(len)(FloatData(f))))
}

