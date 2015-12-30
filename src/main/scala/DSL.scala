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

//  implicit class ArrayAssignment(lhs: Phrase[ArrayType[AccType]]) {
//    def :==(rhs: Phrase[ArrayType[ExpType]]): Phrase[CommandType] = {
//      `for`(lhs.length, { i => lhs `@` i := rhs `@` i })
//    }
//  }
//
//  implicit class MatrixAssignment(lhs: Phrase[ArrayType[ArrayType[AccType]]]) {
//    def :==(rhs: Phrase[ArrayType[ArrayType[ExpType]]]): Phrase[CommandType] = {
//      `for`(lhs.length, { i =>
//        `for`((lhs `@` i).length, { j =>
//          (lhs `@` i) `@` j := (rhs `@` i) `@`j
//        })
//      })
//    }
//  }
//
//  implicit class ArrayVarAssignment(lhs: Phrase[ArrayType[VarType]]) {
//    def :==(rhs: Phrase[ArrayType[ExpType]]): Phrase[CommandType] = {
//      `for`(lhs.length, { i => lhs.acc `@` i := rhs `@` i })
//    }
//  }

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

    def `@`(index: Phrase[ExpType]) = ArrayExpAccess(e, index)
  }

  implicit class AccPhraseExtensions(a: Phrase[AccType]) {
    def `@`(index: Phrase[ExpType]) = ArrayAccAccess(a, index)
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
    \(CommandType() x CommandType()) {
      pair => Seq(Proj1(pair), Proj2(pair))
    }
  }
}

object makeNew {
  // TODO: make passive lambda ...
  def apply(t: DataType) = {
    //: Phrase[(VarType -> CommandType) -> CommandType]
    \(VarType(t) -> CommandType()) {
      f => NewPhrase(f)
    }
  }
}

object := {
  // TODO: add passivity
  def apply(t: DataType) = {
    //: Phrase[ AccType x ExpType -> CommandType ]
    \(AccType(t) x ExpType(t)) {
      pair => Assign(π1(pair), π2(pair))
    }
  }
}

object makeIfThenElse {
  // TODO: add passivity
  def apply[T <: PhraseType](t: T) = {
    //: Phrase[ ExpType x T x T -> T ]
    \(ExpType(bool) x t x t) {
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
    \(ExpType(int) x (ExpType(int) -> CommandType())) {
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

  def apply[T <: PhraseType](f: (Phrase[ExpType], Phrase[ExpType]) => Phrase[T]): Lambda[ExpType, T] = {
    val param = Ident[ExpType]( newName() )
    val g = λ(ExpType(RecordType(int, int))) { x => f(x._1(), x._2()) }
    Lambda(param, g(param))
  }

  def apply[T <: PhraseType](t: ExpType)(f: Ident[ExpType] => Phrase[T]): Lambda[ExpType, T] = {
    val param = identifier(newName(), t)
    Lambda(param, f(param))
  }

  def apply[T <: PhraseType](t: AccType)(f: Ident[AccType] => Phrase[T]): Lambda[AccType, T] = {
    val param = identifier(newName(), t)
    Lambda(param, f(param))
  }


  def apply[T1 <: PhraseType,
            T2 <: PhraseType,
            T3 <: PhraseType](t: T1 x T2)
                             (f: Phrase[T1 x T2] => Phrase[T3]): Lambda[T1 x T2, T3] = {
    t match {
        // VarType
      case PairType(ExpType(t1), AccType(t2)) if t1 == t2 =>
        val param = identifier(newName(), t)
        Lambda(param, f(param))
      case _ =>
        val param = Pair(identifier(newName(), t.t1), identifier(newName(), t.t2))
        Lambda(param, f(param))
    }
  }

  def apply[T1 <: PhraseType,
            T2 <: PhraseType,
            T3 <: PhraseType](t: T1 -> T2)
                             (f: Ident[T1 -> T2] => Phrase[T3]): Lambda[T1 -> T2, T3] = {
    val param = identifier(newName(), t)
    Lambda(param, f(param))
  }

  def apply[T1 <: PhraseType, T2 <: PhraseType](param: Phrase[T1],
                                                body: Phrase[T2]): Lambda[T1, T2] = {
    Lambda(param, body)
  }
}

object fun extends funDef

object \ extends funDef

object λ extends funDef

object skip extends SkipPhrase

//object Map {
//  def apply[T1 <: PhraseType, T2 <: PhraseType](f: Phrase[T1 -> T2]) = {
//    val param = Ident[ArrayType[T1]](λ.newName())
//    Lambda(param, MapPhrase(f, param))
//  }
//
//  def apply[T1 <: PhraseType,
//            T2 <: PhraseType](f: Phrase[T1 -> T2],
//                              array: Phrase[ArrayType[T1]]) = {
//    MapPhrase(f, array)
//  }
//}
//
//object Zip {
//  def apply[T1 <: PhraseType,
//            T2 <: PhraseType](lhs: Phrase[ArrayType[T1]],
//                              rhs: Phrase[ArrayType[T2]]) = ZipPhrase(lhs, rhs)
//}
//
//object Split {
//  def apply[T <: PhraseType](n: Int, array: Phrase[ArrayType[T]]) =
//    SplitPhrase(n, array)
//}
//
//object Join {
//  def apply[T <: PhraseType](array: Phrase[ArrayType[ArrayType[T]]]) =
//    JoinPhrase(array)
//}
//
//object Reduce {
//  def apply[T <: PhraseType](f: Phrase[T x T -> T]) = {
//    val param = Pair(Ident[T](λ.newName()), Ident[ArrayType[T]](λ.newName()))
//    Lambda(param, ReducePhrase(f, π1(param), π2(param)))
//  }
//
//  def apply[T <: PhraseType](f: Phrase[T x T -> T],
//                             init: Phrase[T]) = {
//    val param = Ident[ArrayType[T]](λ.newName())
//    Lambda(param, ReducePhrase(f, init, param))
//  }
//
//  def apply[T <: PhraseType](f: Phrase[T x T -> T],
//                             init: Phrase[T],
//                             array: Phrase[ArrayType[T]]) =
//    ReducePhrase(f, init, array)
//}
//
//object Iterate {
//  def apply[T <: PhraseType](n: Int, f: Phrase[T -> T]) = {
//    val param = Ident[T](λ.newName())
//    Lambda(param, IteratePhrase(n, f, param))
//  }
//
//  def apply[T <: PhraseType](n: Int, f: Phrase[T -> T], in: Phrase[T]) =
//    IteratePhrase(n, f, in)
//}


