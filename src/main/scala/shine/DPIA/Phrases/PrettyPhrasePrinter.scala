package shine.DPIA.Phrases

import shine.DPIA.Types._
import rise.core.types.Kind

object PrettyPhrasePrinter {

  def apply[T <: PhraseType](p: Phrase[T]): String = {
    p match {
      case app: Apply[a, T] => s"(${apply(app.fun)})(${apply(app.arg)})"

      case app: DepApply[_, _, T] => s"(${apply(app.fun)})(${app.arg})"

      case p1: Proj1[a, b] => s"π1(${apply(p1.pair)})"

      case p2: Proj2[a, b] => s"π2(${apply(p2.pair)})"

      case IfThenElse(cond, thenP, elseP) =>
        s"if (${apply(cond)}) (${apply(thenP)}) else (${apply(elseP)})"

      case UnaryOp(op, x) => s"${op.toString} ${apply(x)}"

      case BinOp(op, lhs, rhs) => s"${apply(lhs)} ${op.toString} ${apply(rhs)}"

      case Identifier(name, _) => name

      case Lambda(param, body) => s"λ ${apply(param)}: ${param.t} -> ${apply(body)}"

      case DepLambda(kind, param, body) => s"Λ (${Kind.idName(kind, param)}: ${kind.name}) -> ${apply(body)}"

      case LetNat(binder, defn, body) => s"nLet ${binder.name} = ${apply(defn)} in ${apply(body)}"

      case Literal(d) => d.toString

      case Natural(n) => n.toString

      case PhrasePair(fst, snd) => s"(${apply(fst)}, ${apply(snd)})"

      case shine.DPIA.primitives.imperative.Comment(comment) => s"\n//$comment\n"

      case shine.OpenCL.primitives.imperative.Barrier(local, global) =>
        s"""barrier( ${if(local) "CLK_LOCAL_MEM_FENCE" else ""} ${if(global && local) "|" else ""}
          ${if(global) "CLK_GLOBAL_MEM_FENCE" else ""})"""

      case shine.cuda.primitives.imperative.SyncThreads() => "__syncthreads()"

      case shine.cuda.primitives.imperative.SyncWarp() => "__syncwarp()"

      case shine.cuda.primitives.imperative.SyncPipeline(pipe) => s"$pipe.commit_and_wait()"

      case c: Primitive[_] => c.prettyPrint
    }
  }
}
