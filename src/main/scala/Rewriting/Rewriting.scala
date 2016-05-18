package Rewriting

import Core._
import Core.PhraseType._
import DSL._
import ExpPatterns._
import AccPatterns._
import CommandPatterns._

object Rewriting {

  def acc(E: Phrase[ExpType], A: Phrase[AccType]): Phrase[CommandType] = {
    E match {
      case x: IdentPhrase[ExpType] if x.t.dataType.isBasicType =>
        A `:=` x

      case x: IdentPhrase[ExpType] if x.t.dataType.isInstanceOf[ArrayType] =>
        MapI(A,
          λ(A.t) { o => λ(x.t) { x => acc(x, o) } },
          x
        ).asPhrase

      case x: IdentPhrase[ExpType] if x.t.dataType.isInstanceOf[RecordType] =>
        acc(fst(x), fstAcc(A)) `;` acc(snd(x), sndAcc(A))

      case c : LiteralPhrase => A `:=` c

      case BinOpPhrase(op, e1, e2) =>
        exp(e1, λ(e1.t) { x =>
          exp(e2, λ(e2.t) { y =>
            A `:=` BinOpPhrase(op, x, y)
          })
        })

      case ExpPatternPhrase(pattern) => pattern match {

        case Map(f, e) =>
          exp(e, λ(e.t) { x =>
            MapI(A,
              λ(A.t) { o => λ(e.t) { x => acc(f(x), o) } },
              x
            ).asPhrase
          })

        case Reduce(f, i, e) =>
          exp(e, λ(e.t) { x =>
            exp(i, λ(i.t) { y =>
              ReduceIAcc(A,
                λ(A.t) { o => λ(e.t) { x => λ(i.t) { y => acc(f(x)(y), o) } } },
                y,
                x
              ).asPhrase
            })
          })

        case Zip(e1, e2) =>
          exp(e1, λ(e1.t) { x =>
            exp(e2, λ(e2.t) { y =>
              MapI(A,
                λ(A.t) { o => λ(ExpType(RecordType(e1.t.dataType, e2.t.dataType))) { x => acc(x, o) } },
                Zip(x, y).asPhrase
              ).asPhrase
            })
          })

        case Join(e) => acc(e, JoinAcc(A).asPhrase)

        case Split(n, e) => acc(e, SplitAcc(n, A).asPhrase)

      }
    }
  }

  def exp(E: Phrase[ExpType], C: Phrase[ExpType -> CommandType]): Phrase[CommandType] = {
    E match {
      case x: IdentPhrase[ExpType] => C(x)

      case c : LiteralPhrase => C(c)

      case BinOpPhrase(op, e1, e2) =>
        exp(e1, λ(e1.t) { x =>
          exp(e2, λ(e2.t) { y =>
            C(BinOpPhrase(op, x, y))
          })
        })

      case ExpPatternPhrase(pattern) => pattern match {

        case Map(f, e) =>
          // specify array type + size info
          `new`( tmp =>
            acc(Map(f, e), π2(tmp)) `;`
            C(π1(tmp))
          )

        case Reduce(f, i, e) =>
          exp(e, λ(e.t) { x =>
            exp(i, λ(i.t) { y =>
              ReduceIExp(C,
                λ(AccType(i.t.dataType)) { o => λ(e.t) { x => λ(i.t) { y => acc(f(x)(y), o) } } },
                y,
                x
              ).asPhrase
            })
          })

        case Zip(e1, e2) =>
          exp(e1, λ(e1.t) { x =>
            exp(e2, λ(e2.t) { y =>
              C(Zip(x, y).asPhrase)
            })
          })

        case Join(e) =>
          exp(e, λ(e.t) { x =>
            C(Join(x))
          })

        case Split(n, e) =>
          exp(e, λ(e.t) { x =>
            C(Split(n, x))
          })

      }
    }
  }

}
