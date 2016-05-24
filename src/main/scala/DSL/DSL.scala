package DSL

import Core.OperationalSemantics._
import Core.PhraseType._
import Core._
import AccPatterns._
import ExpPatterns._
import CommandPatterns._

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

object makeNew {
  // TODO: make passive lambda ...
  def apply(t: DataType) = {
    //: Phrase[(VarType -> CommandType) -> CommandType]
    λ(VarType(t) -> CommandType()) {
      f => New(f)
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


object makeFor {
  def apply() = {
    //: Phrase[ ExpType x (ExpType -> CommandType) -> CommandType ]
    λ(ExpType(int) x (ExpType(int) -> CommandType())) {
      args => {
        For(π1(args), π2(args))
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

object `new` {
  def apply(f: Phrase[ (ExpType x AccType) -> CommandType ]) = New(f)

  def apply(f: Phrase[ExpType x AccType] => Phrase[CommandType]) = {
    New(λ( ExpType(int) x AccType(int) ) { v => f(v) })
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

  def apply(f: Phrase[ExpType -> ExpType], array: Phrase[ExpType]) =
    Map(f, array)
}

object zip {
  def apply(lhs: Phrase[ExpType], rhs: Phrase[ExpType]) = Zip(lhs, rhs)
}

object split {
  def apply(n: Int, array: Phrase[ExpType]) = Split(n, array)
}

object join {
  def apply(array: Phrase[ExpType]) = Join(array)
}

object reduce {
  def apply(f: Phrase[ExpType -> (ExpType -> ExpType)]) =
    λ( (init, array) => Reduce(f, init, array))

  def apply(f: Phrase[ExpType -> (ExpType -> ExpType)], init: Phrase[ExpType]) =
    λ( array => Reduce(f, init, array))

  def apply(f: Phrase[ExpType -> (ExpType -> ExpType)], init: Phrase[ExpType],
            array: Phrase[ExpType]) = Reduce(f, init, array)
}

object iterate {
  def apply(n: Int, f: Phrase[ExpType -> ExpType]) =
    λ( array => Iterate(n, f, array))

  def apply(n: Int, f: Phrase[ExpType -> ExpType], array: Phrase[ExpType]) =
    Iterate(n, f, array)
}

object length {
  def apply[T <: BasePhraseTypes](array: Phrase[T]) = Length(array)
}

object mapI {
  def apply(out: Phrase[AccType], f: Phrase[AccType -> (ExpType -> CommandType)], in: Phrase[ExpType]) =
    MapI(out, f, in)
}
