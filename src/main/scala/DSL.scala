import PhraseType._

import scala.language.implicitConversions

object PhraseExtensions {

  implicit class BinOps(lhs: Phrase[ExpType]) {
    def +(rhs: Phrase[ExpType]) = BinOp(BinOp.Op.ADD, lhs, rhs)

    def -(rhs: Phrase[ExpType]) = BinOp(BinOp.Op.SUB, lhs, rhs)

    def *(rhs: Phrase[ExpType]) = BinOp(BinOp.Op.MUL, lhs, rhs)

    def /(rhs: Phrase[ExpType]) = BinOp(BinOp.Op.DIV, lhs, rhs)

    def %(rhs: Phrase[ExpType]) = BinOp(BinOp.Op.MOD, lhs, rhs)
  }

  implicit class CallLambda[T1 <: PhraseType, T2 <: PhraseType](lambda: Lambda[T1, T2]) {
    def apply(arg: Phrase[T1]) = Apply(lambda, arg)
  }

  implicit class SequentialComposition(c1: Phrase[CommandType]) {
    def `;`(c2: Phrase[CommandType]) = Seq(c1, c2)
  }

  implicit class Assignment(lhs: Phrase[AccType]) {
    def :=(rhs: Phrase[ExpType]) = Assign(lhs, rhs)
  }

  implicit class PairTypeConstructor[T1 <: PhraseType](t1: T1) {
    def x[T2 <: PhraseType](t2: T2) = PairType(t1, t2)
  }

  implicit class FunctionTypeConstructor[T1 <: PhraseType](t1: T1) {
    def ->[T2 <: PhraseType](t2: T2) = FunctionType(t1, t2)
  }

  implicit class PassiveFunctionTypeConstructor[T1 <: PhraseType](t1: T1) {
    def `->p`[T2 <: PhraseType](t2: T2) = PassiveFunctionType(t1, t2)
  }

  implicit def toPair[T1 <: PhraseType,
  T2 <: PhraseType](pair: (Phrase[T1], Phrase[T2])): Pair[T1, T2] = {
    Pair(pair._1, pair._2)
  }

  implicit def toLiteral(i: Int): IntLiteral = IntLiteral(i)

  implicit class ExpPhraseExtensions(e: Phrase[ExpType]) {
    def _1() = FieldAccess(0, e)

    def _2() = FieldAccess(1, e)

    def `@`(index: Phrase[ExpType]) = ArrayExpAccessPhrase(e, index)
  }

  implicit class AccPhraseExtensions(a: Phrase[AccType]) {
    def `@`(index: Phrase[ExpType]) = ArrayAccAccessPhrase(a, index)
  }

  implicit class VarExtensions(v: Phrase[VarType]) {
    def exp = π1(v)
    def acc = π2(v)
  }

}

import PhraseExtensions._

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
  var counter = 0

  def newName(): String = {
    counter += 1
    "v" + counter
  }

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


