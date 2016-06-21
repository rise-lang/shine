package DSL

import Core.OperationalSemantics._
import Core.PhraseType._
import Core._
import AccPatterns._
import ExpPatterns._
import CommandPatterns._
import apart.arithmetic.ArithExpr

object VarType {
  def apply(dataType: DataType) = ExpType(dataType) x AccType(dataType)
}

object `;` {
  def apply() = {
    //: Phrase[CommandType x CommandType -> CommandType]
    λ(CommandType() x CommandType()) {
      pair => Seq(Proj1Phrase(pair), Proj2Phrase(pair))
    }
  }
}

object := {
  // TODO: add passivity
  def apply(t: DataType) = {
    //: Phrase[ AccType x ExpType -> CommandType ]
    λ(AccType(t) x ExpType(t)) {
      pair => Assign(π1(pair), π2(pair))
    }
  }
}

object makeIfThenElse {
  // TODO: add passivity
  def apply[T <: PhraseType](t: T) = {
    //: Phrase[ ExpType x T x T -> T ]
    λ(ExpType(bool) x t x t) {
      args => {
        val firstTwo = π1(args)
        val cond = π1(firstTwo)
        val thenP = π2(firstTwo)
        val elseP = π2(args)
        IfThenElsePhrase(cond, thenP, elseP)
      }
    }
  }
}

object `if` {
  def apply[T <: PhraseType](cond: Phrase[ExpType], thenP: Phrase[T], elseP: Phrase[T]) = {
    IfThenElsePhrase(cond, thenP, elseP)
  }
}

object `for` {
  def apply(n: Phrase[ExpType], f: (Phrase[ExpType] => Phrase[CommandType])) = {
    For(n, λ(ExpType(int)) { i => f(i) })
  }
}

object `parFor` {
  def apply(n: Phrase[ExpType],
            out: Phrase[AccType],
            f: (Phrase[ExpType] => Phrase[AccType] => Phrase[CommandType])) = {
    val elemT = out.t match { case AccType(ArrayType(_, dt)) => dt }
    ParFor(n, out, λ( ExpType(int) ) { i => λ( AccType(elemT) ) { o => f(i)(o) } })
  }
}

object `new` {
  def apply(dt: DataType, addressSpace: AddressSpace, f: Phrase[ (ExpType x AccType) -> CommandType ]) = New(dt, addressSpace, f)

  def apply(dt: DataType, addressSpace: AddressSpace, f: Phrase[ExpType x AccType] => Phrase[CommandType]) = {
    New(dt, addressSpace, λ( ExpType(dt) x AccType(dt) ) { v => f(v) })
  }
}

object fst {
  def apply(record: Phrase[ExpType]) = Fst(record)
}

object snd {
  def apply(record: Phrase[ExpType]) = Snd(record)
}

object fstAcc {
  def apply(record: Phrase[AccType]) = FstAcc(record)
}

object sndAcc {
  def apply(record: Phrase[AccType]) = SndAcc(record)
}

object π1 {
  def apply[T1 <: PhraseType, T2 <: PhraseType](pair: Phrase[T1 x T2]) = Proj1Phrase(pair)
}

object π2 {
  def apply[T1 <: PhraseType, T2 <: PhraseType](pair: Phrase[T1 x T2]) = Proj2Phrase(pair)
}

object identifier {
  def exp(name: String) = IdentPhrase[ExpType](name)
  def acc(name: String) = IdentPhrase[AccType](name)

  def newVar(name: String) = IdentPhrase[ExpType x AccType](name)

  def newVar(name: String, dt: DataType) = {
    val i = IdentPhrase[ExpType x AccType](name)
    i.t = PairType[ExpType, AccType](ExpType(dt), AccType(dt))
    i
  }

  def apply(name: String) = IdentPhrase[ExpType](name)

  def apply[T <: PhraseType](name: String, t: T) = {
    val i = IdentPhrase[T](name)
    i.t = t
    i
  }
}

trait funDef {
  def apply[T <: PhraseType](f: IdentPhrase[ExpType] => Phrase[T]): LambdaPhrase[ExpType, T] = {
    val param = IdentPhrase[ExpType]( newName() )
    LambdaPhrase(param, f(param))
  }

  def apply[T <: PhraseType](f: (Phrase[ExpType], Phrase[ExpType]) => Phrase[T]): LambdaPhrase[ExpType x ExpType, T] = {
    val param = IdentPhrase[PairType[ExpType, ExpType]]( newName() )
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

object skip extends Skip

object map {
  def apply(f: Phrase[ExpType -> ExpType]) = λ( x => Map(f, x))

  def apply(f: Phrase[ExpType -> ExpType], array: Phrase[ExpType]) = Map(f, array)
}

object mapSeq {
  def apply(f: Phrase[ExpType -> ExpType]) = λ( x => MapSeq(f, x))

  def apply(f: Phrase[ExpType -> ExpType], array: Phrase[ExpType]) = MapSeq(f, array)
}

object mapWorkgroup {
  def apply(f: Phrase[ExpType -> ExpType]) = λ( x => MapWorkgroup(f, x))

  def apply(f: Phrase[ExpType -> ExpType], array: Phrase[ExpType]) = MapWorkgroup(f, array)
}

object mapLocal {
  def apply(f: Phrase[ExpType -> ExpType]) = λ( x => MapLocal(f, x))

  def apply(f: Phrase[ExpType -> ExpType], array: Phrase[ExpType]) = MapLocal(f, array)
}

object zip {
  def apply(lhs: Phrase[ExpType], rhs: Phrase[ExpType]) = Zip(lhs, rhs)
}

object split {
  def apply(n: ArithExpr) = λ(array => Split(n, array) )
  def apply(n: ArithExpr, array: Phrase[ExpType]) = Split(n, array)
}

object join {
  def apply() = λ( array => Join(array) )
  def apply(array: Phrase[ExpType]) = Join(array)
}

object toLocal {
  def apply(f: Phrase[ExpType -> ExpType]) = λ( x => ToLocal(f, x))
  def apply(f: Phrase[ExpType -> ExpType], x: Phrase[ExpType]) = ToLocal(f, x)
}

object toGlobal {
  def apply(f: Phrase[ExpType -> ExpType]) = λ( x => ToGlobal(f, x))
  def apply(f: Phrase[ExpType -> ExpType], x: Phrase[ExpType]) = ToGlobal(f, x)
}

object reduce {
  def apply(f: Phrase[ExpType -> (ExpType -> ExpType)]) =
    λ( (init, array) => Reduce(f, init, array))

  def apply(f: Phrase[ExpType -> (ExpType -> ExpType)], init: Phrase[ExpType]) =
    λ( array => Reduce(f, init, array))

  def apply(f: Phrase[ExpType -> (ExpType -> ExpType)], init: Phrase[ExpType],
            array: Phrase[ExpType]) = Reduce(f, init, array)
}

object reduceSeq {
  def apply(f: Phrase[ExpType -> (ExpType -> ExpType)]) =
    λ( (init, array) => ReduceSeq(f, init, array))

  def apply(f: Phrase[ExpType -> (ExpType -> ExpType)], init: Phrase[ExpType]) =
    λ( array => ReduceSeq(f, init, array))

  def apply(f: Phrase[ExpType -> (ExpType -> ExpType)],
            init: Phrase[ExpType],
            array: Phrase[ExpType]) =
    ReduceSeq(f, init, array)
}

object iterate {
  def apply(n: ArithExpr, f: Phrase[ExpType -> ExpType]) =
    λ( array => Iterate(n, f, array))

  def apply(n: ArithExpr, f: Phrase[ExpType -> ExpType], array: Phrase[ExpType]) =
    Iterate(n, f, array)
}

object length {
  def apply[T <: BasePhraseTypes](array: Phrase[T]) = Length(array)
}

object mapI {
  def apply(out: Phrase[AccType], f: Phrase[AccType -> (ExpType -> CommandType)], in: Phrase[ExpType]) =
    MapI(out, f, in)
}

object gather {
  def apply(idxF: (ArithExpr, DataType) => ArithExpr) = λ( array => Gather(idxF, array))
}

object asVector {
  def apply(n: ArithExpr) = λ(array => AsVector(n, array) )
  def apply(n: ArithExpr, array: Phrase[ExpType]) = AsVector(n, array)
}

object asScalar {
  def apply() = λ( array => AsScalar(array) )
  def apply(array: Phrase[ExpType]) = AsScalar(array)
}

object vectorize {
  def apply(len: Int, f: Float) = LiteralPhrase(VectorData(Vector.fill(len)(FloatData(f))))
}
