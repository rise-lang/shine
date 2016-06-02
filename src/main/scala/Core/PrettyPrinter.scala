package Core

object PrettyPrinter {

  def apply[T <: PhraseType](p: Phrase[T]): String = {
    p match {
      case app: ApplyPhrase[a, T] => s"(${apply(app.fun)} ${apply(app.arg)})"

      case p1: Proj1Phrase[a, b] => s"${apply(p1.pair)}._1"

      case p2: Proj2Phrase[a, b] => s"${apply(p2.pair)}._2"

      case IfThenElsePhrase(cond, thenP, elseP) =>
        s"if(${apply(cond)}) { ${apply(thenP)} } else { ${apply(elseP)} }"

      case UnaryOpPhrase(op, x) => s"(${op.toString} ${apply(x)})"

      case BinOpPhrase(op, lhs, rhs) => s"(${apply(lhs)} ${op.toString} ${apply(rhs)})"

      case IdentPhrase(name) => name

      case LambdaPhrase(param, body) => s"(λ ${apply(param)}. ${apply(body)})"

      case LiteralPhrase(d) => d.toString

      case PairPhrase(fst, snd) => s"(${apply(fst)}, ${apply(snd)})"

      case p: ExpPattern => p.prettyPrint

      case p: AccPattern => p.prettyPrint

      case p: CommandPattern => p.prettyPrint
    }
  }

}
