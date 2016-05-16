package DSL

import Core.OperationalSemantics._
import Core.PhraseType._
import Core._
import ExpPatterns._
import CommandPatterns._

object VarType {
  def apply(dataType: DataType) = ExpType(dataType) x AccType(dataType)
}

object `;` {
  def apply() = {
    //: Phrase[CommandType x CommandType -> CommandType]
    λ(CommandType() x CommandType()) {
      pair => Seq(Proj1(pair), Proj2(pair))
    }
  }
}

object makeNew {
  // TODO: make passive lambda ...
  def apply(t: DataType) = {
    //: Phrase[(VarType -> CommandType) -> CommandType]
    λ(VarType(t) -> CommandType()) {
      f => NewPhrase(f)
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
        IfThenElse(cond, thenP, elseP)
      }
    }
  }
}


object makeFor {
  def apply() = {
    //: Phrase[ ExpType x (ExpType -> CommandType) -> CommandType ]
    λ(ExpType(int) x (ExpType(int) -> CommandType())) {
      args => {
        ForPhrase(π1(args), π2(args))
      }
    }
  }
}

object `if` {
  def apply[T <: PhraseType](cond: Phrase[ExpType], thenP: Phrase[T], elseP: Phrase[T]) = {
    IfThenElse(cond, thenP, elseP)
  }
}

object `for` {
  def apply(n: Phrase[ExpType], f: (Phrase[ExpType] => Phrase[CommandType])) = {
    ForPhrase(n, λ(ExpType(int)) { i => f(i) })
  }
}

object `new` {
  def apply(f: Phrase[ (ExpType x AccType) -> CommandType ]) = NewPhrase(f)

  def apply(f: Phrase[ExpType x AccType] => Phrase[CommandType]) = {
    NewPhrase(λ( ExpType(int) x AccType(int) ) { v => f(v) })
  }
}

object π1 {
  def apply[T1 <: PhraseType, T2 <: PhraseType](pair: Phrase[T1 x T2]) = Proj1(pair)
}

object π2 {
  def apply[T1 <: PhraseType, T2 <: PhraseType](pair: Phrase[T1 x T2]) = Proj2(pair)
}

object identifier {
  def apply(name: String) = Ident[ExpType](name)

  def apply[T <: PhraseType](name: String, t: T) = {
    val i = Ident[T](name)
    i.t = t
    i
  }
}

trait funDef {
  def apply[T <: PhraseType](f: Ident[ExpType] => Phrase[T]): Lambda[ExpType, T] = {
    val param = Ident[ExpType]( newName() )
    Lambda(param, f(param))
  }

  def apply[T <: PhraseType](f: (Phrase[ExpType], Phrase[ExpType]) => Phrase[T]): Lambda[ExpType x ExpType, T] = {
    val param = Ident[PairType[ExpType, ExpType]]( newName() )
    val g = λ(PairType(ExpType(int), ExpType(int))) { x => f(π1(x), π2(x)) }
    Lambda(param, g(param))
  }

  def apply[T1 <: PhraseType, T2 <: PhraseType](t: T1)
                                               (f: Ident[T1] => Phrase[T2]): Lambda[T1, T2] = {
    val param = identifier(newName(), t)
    Lambda(param, f(param))
  }

  def apply[T1 <: PhraseType, T2 <: PhraseType](param: Ident[T1],
                                                body: Phrase[T2]): Lambda[T1, T2] = {
    Lambda(param, body)
  }
}

object fun extends funDef

object \ extends funDef

object λ extends funDef

object skip extends SkipPhrase

object map {
  def apply(f: Phrase[ExpType -> ExpType]) = λ( x => MapPhrase(f, x))

  def apply(f: Phrase[ExpType -> ExpType], array: Phrase[ExpType]) =
    MapPhrase(f, array)
}

object zip {
  def apply(lhs: Phrase[ExpType], rhs: Phrase[ExpType]) = ZipPhrase(lhs, rhs)
}

object split {
  def apply(n: Int, array: Phrase[ExpType]) = SplitPhrase(n, array)
}

object join {
  def apply(array: Phrase[ExpType]) = JoinPhrase(array)
}

object reduce {
  def apply(f: Phrase[ExpType x ExpType -> ExpType]) =
    λ( (init, array) => ReducePhrase(f, init, array))

  def apply(f: Phrase[ExpType x ExpType -> ExpType], init: Phrase[ExpType]) =
    λ( array => ReducePhrase(f, init, array))

  def apply(f: Phrase[ExpType x ExpType -> ExpType], init: Phrase[ExpType],
            array: Phrase[ExpType]) = ReducePhrase(f, init, array)
}

object iterate {
  def apply(n: Int, f: Phrase[ExpType -> ExpType]) =
    λ( array => IteratePhrase(n, f, array))

  def apply(n: Int, f: Phrase[ExpType -> ExpType], array: Phrase[ExpType]) =
    IteratePhrase(n, f, array)
}

object length {
  def apply[T <: BasePhraseTypes](array: Phrase[T]) =
    LengthPhrase(array)
}

object mapI {
  def apply(out: Phrase[AccType], f: Phrase[AccType -> (ExpType -> CommandType)], in: Phrase[ExpType]) =
    MapIPhrase(out, f, in)
}


