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
}

object identifier {
  def apply(name: String) = Ident[ExpType](name)
}

object fun {
  def apply[T1 <: PhraseType, T2 <: PhraseType](f: Ident[T1] => Phrase[T2]): Lambda[T1, T2] = {
    val param = Ident[T1]("")
    Lambda(param, f(param))
  }

  def apply[T1 <: PhraseType, T2 <: PhraseType](param : Phrase[T1],
                                                body : Phrase[T2]): Lambda[T1, T2] = {
    Lambda(param, body)
  }
}

