package Rewriting

import Core._
import Core.PhraseType._
import CommandPatterns._
import DSL._
import ExpPatterns._

object Rewriting {

  def acc(E: Phrase[ExpType], A: Phrase[AccType]): Phrase[CommandType] = {
    E match {
      case x: IdentPhrase[ExpType] if x.t.dataType.isBasicType =>
        A `:=` x

      case x: IdentPhrase[ExpType] if x.t.dataType.isInstanceOf[ArrayType] =>
        MapIPattern(A,
          λ(A.t) { o => λ(x.t) { x => acc(x, o) } },
          x
        ).asPhrase

      case x: IdentPhrase[ExpType] if x.t.dataType.isInstanceOf[RecordType] =>
        acc(fst(x), fstAcc(A)) `;` acc(snd(x), sndAcc(A))

      case BinOpPhrase(op, e1, e2) =>
        exp(e1, λ(e1.t) { x =>
          exp(e2, λ(e2.t) { y =>
            A `:=` BinOpPhrase(op, x, y)
          })
        })

      case ExpPatternPhrase(pattern) => pattern match {

        case MapPattern(f, e) =>
          exp(e, λ(e.t) { x =>
            MapIPattern(A,
              λ(A.t) { o => λ(e.t) { x => acc(f(x), o) } },
              x
            ).asPhrase
          })

        case ReducePattern(f, i, e) =>
          exp(e, λ(e.t) { x =>
            exp(i, λ(i.t) { y =>
              ReduceIAccPattern(A,
                λ(A.t) { o => λ(e.t) { x => λ(i.t) { y => acc(f(x)(y), o) } } },
                y,
                x
              ).asPhrase
            })
          })

        case ZipPattern(e1, e2) =>
          exp(e1, λ(e1.t) { x =>
            exp(e2, λ(e2.t) { y =>
              MapIPattern(A,
                λ(A.t) { o => λ(ExpType(RecordType(e1.t.dataType, e2.t.dataType))) { x => acc(x, o) } },
                ZipPattern(x, y).asPhrase
              ).asPhrase
            })
          })

//        case JoinPattern(e) =>
//          acc(e, )

//        case SplitPattern(e) =>
//            acc(e, )

      }
    }
  }

  def exp(E: Phrase[ExpType], C: Phrase[ExpType -> CommandType]): Phrase[CommandType] = {
    E match {
      case x: IdentPhrase[ExpType] => C(x)

      case BinOpPhrase(op, e1, e2) =>
        exp(e1, λ(e1.t) { x =>
          exp(e2, λ(e2.t) { y =>
            C(BinOpPhrase(op, x, y))
          })
        })

      case ExpPatternPhrase(pattern) => pattern match {

        case MapPattern(f, e) =>
          // specify array type + size info
          `new`( tmp =>
            acc(MapPattern(f, e), π2(tmp)) `;`
            C(π1(tmp))
          )

        case ReducePattern(f, i, e) =>
          exp(e, λ(e.t) { x =>
            exp(i, λ(i.t) { y =>
              ReduceIExpPattern(C,
                λ(AccType(i.t.dataType)) { o => λ(e.t) { x => λ(i.t) { y => acc(f(x)(y), o) } } },
                y,
                x
              ).asPhrase
            })
          })

        case ZipPattern(e1, e2) =>
          exp(e1, λ(e1.t) { x =>
            exp(e2, λ(e2.t) { y =>
              C(ZipPattern(x, y).asPhrase)
            })
          })

        case JoinPattern(e) =>
          exp(e, λ(e.t) { x =>
            C(JoinPattern(x))
          })

        case SplitPattern(n, e) =>
          exp(e, λ(e.t) { x =>
            C(SplitPattern(n, x))
          })

      }
    }
  }

}
