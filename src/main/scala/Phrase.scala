sealed abstract class Phrase {
  var t : PhraseType = null
}

case class Ident(name : String) extends Phrase

case class Lambda(param : Phrase, body : Phrase) extends Phrase

case class Apply(fun : Phrase, arg: Phrase) extends Phrase

// tuples
case class Pair(a : Phrase, b : Phrase) extends Phrase
case class Proj0(p : Phrase) extends Phrase
case class Proj1(p : Phrase) extends Phrase

case class Skip() extends Phrase
case class Seq(c1 : Phrase, c2 : Phrase) extends Phrase
case class New(f : Phrase) extends Phrase
case class Assign(lhs : Phrase, rhs : Phrase) extends Phrase
case class IfThenElse(cond : Phrase, thenP : Phrase, elseP : Phrase) extends Phrase
case class For(upper : Phrase, body: Phrase) extends Phrase
case class IntLiteral(i : Int) extends Phrase



case class BinOp(op : BinOp.Op.Value, lhs : Phrase, rhs : Phrase) extends Phrase

object BinOp {
  object Op extends Enumeration {
    val ADD = Value("+")
    val SUB = Value("-")
    val MUL = Value("*")
    val DIV = Value("/")
    val MOD = Value("%")
  }
}