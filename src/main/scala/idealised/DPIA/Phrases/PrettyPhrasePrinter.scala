package idealised.DPIA.Phrases

import idealised.DPIA.Types.PhraseType

object PrettyPhrasePrinter {

  def apply[T <: PhraseType](p: Phrase[T]): String = {
    p match {
      case app: Apply[a, T] => s"(${apply(app.fun)})(${apply(app.arg)})"

      case app: NatDependentApply[T] => s"(${apply(app.fun)})(${app.arg})"

      case app: TypeDependentApply[T] => s"(${apply(app.fun)})(${app.arg})"

      case p1: Proj1[a, b] => s"π1(${apply(p1.pair)})"

      case p2: Proj2[a, b] => s"π2(${apply(p2.pair)})"

      case IfThenElse(cond, thenP, elseP) =>
        s"if (${apply(cond)}) (${apply(thenP)}) else (${apply(elseP)})"

      case UnaryOp(op, x) => s"${op.toString} ${apply(x)}"

      case BinOp(op, lhs, rhs) => s"${apply(lhs)} ${op.toString} ${apply(rhs)}"

      case Identifier(name, _) => name

      case Lambda(param, body) => s"λ ${apply(param)}: ${param.t} -> ${apply(body)}"

      case NatDependentLambda(param, body) => s"Λ (${param.name}: nat) -> ${apply(body)}"

      case TypeDependentLambda(param, body) => s"Λ (${param.name}: dt) -> ${apply(body)}"

      case Literal(d) => d.toString

      case Natural(n) => n.toString

      case Pair(fst, snd) => s"(${apply(fst)}, ${apply(snd)})"

      case c: Primitive[_] => c.prettyPrint
    }
  }

}
