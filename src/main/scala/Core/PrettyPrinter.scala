package Core

object PrettyPrinter {

  case class Indent(i: Int) {
    assert(i >= 0)

    override def toString = " " * i

    def more = Indent(i + 1)
    def less = Indent(i - 1)
  }

  def apply[T <: PhraseType](p: Phrase[T], indent: Indent = Indent(0)): String = {
    p match {
      case p: ExpPattern => p.prettyPrint(indent)

      case p: AccPattern => p.prettyPrint(indent)

      case p: IntermediateCommandPattern => p.prettyPrint(indent)

      case _ => indent + (p match {
        case app: ApplyPhrase[a, T] => s"(${apply(app.fun)} ${apply(app.arg)})"

        case app: NatDependentApplyPhrase[T] => s"(${apply(app.fun)} ${app.arg})"

        case p1: Proj1Phrase[a, b] => s"${apply(p1.pair)}._1"

        case p2: Proj2Phrase[a, b] => s"${apply(p2.pair)}._2"

        case IfThenElsePhrase(cond, thenP, elseP) =>
          s"if(${apply(cond)}) { ${apply(thenP)} } else { ${apply(elseP)} }"

        case UnaryOpPhrase(op, x) => s"(${op.toString} ${apply(x)})"

        case BinOpPhrase(op, lhs, rhs) => s"(${apply(lhs)} ${op.toString} ${apply(rhs)})"

        case IdentPhrase(name) => s"($name : ${p.t})"

        case LambdaPhrase(param, body) => s"((λ ${apply(param)}.\n ${apply(body, indent)}) : ${p.t})"

        case NatDependentLambdaPhrase(param, body) => s"(Λ ${param.name}. ${apply(body)})"

        case LiteralPhrase(d) => d.toString

        case PairPhrase(fst, snd) => s"(${apply(fst)}, ${apply(snd)})"

        case _: ExpPattern | _: AccPattern | _: IntermediateCommandPattern =>
          throw new Exception("This should not happen")
      })
    }
  }

}
