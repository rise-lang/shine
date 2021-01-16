package shine.DPIA.Compilation

import shine.C.CodeGeneration.CodeGenerator._
import shine.DPIA.FunctionalPrimitives._
import shine.DPIA.ImperativePrimitives.{IdxAcc, JoinAcc, ReorderAcc, SplitAcc}
import shine.DPIA.{LetNatIdentifier, Nat}
import shine.DPIA.Semantics.OperationalSemantics
import shine.DPIA.Phrases._
import shine.DPIA.Types._
import shine.DPIA.Lifting

/*
Translates transformations on index accessed arrays into transformations on the
indices, e.g, drop(3, xs)[5] is rewritten into xs[8]. The translation is
triggered on the outermost exp/acc phrase. Translatable exp/acc phrases will be
removed and their effects applied to the index path, until a non translatable
index is reached. Then the path is dumped as nested Idx expressions.
 */

object TranslateIndices {

  def apply(p : Phrase[CommType]) : Phrase[CommType] = {
    VisitAndRebuild(p, new VisitAndRebuild.Visitor {
      override def phrase[T <: PhraseType](p: Phrase[T]): Result[Phrase[T]] = (p.t match {
        case ExpType(_, _) => Stop(idx(p.asInstanceOf[Phrase[ExpType]], Nil))
        case AccType(_) => Stop(idxAcc(p.asInstanceOf[Phrase[AccType]], Nil))
        case _ => Continue(p, this)
      }).asInstanceOf[Result[Phrase[T]]]
    })
  }

  // FIXME: This might already exist, and should probably return an Option[Nat]
  // Interprets both naturals-as-indices and index variables as type level nats
  private def idx2nat : Phrase[ExpType] => Nat = {
    case NatAsIndex(_, Natural(i)) => i
    case Identifier(i, ExpType(IndexType(n), _)) =>
      arithexpr.arithmetic.NamedVar(i, arithexpr.arithmetic.RangeAdd(0, n, 1))
    case _ => throw new Exception("could not use index expression as nat")
  }
  /*
  // FIXME: why do we need to introduce a let binding?
  private def letNat[T <: PhraseType](exp: Phrase[ExpType], f: Nat => Phrase[T]): Phrase[T] = {
    val identifier = LetNatIdentifier()
    LetNat(identifier, exp, f(identifier()))
  }
 */

  private def nat2idx(i : Nat, n : Nat) : Phrase[ExpType] = {
    NatAsIndex(n, Natural(i))
  }

  // TODO: move PathExpr in here
  val Index = CIntExpr

  private def reduce(fun : Phrase[FunType[ExpType, ExpType]], arg : Phrase[ExpType]) = Lifting.liftFunction(fun).reducing(arg)

  def idx(p : Phrase[ExpType], path : List[PathExpr]) : Phrase[ExpType] = {
    (p, path) match {
      case (Idx(_, _, i, e), ps) => idx(e, Index(idx2nat(i)) :: ps)
      //case (Idx(_, _, i, e), ps) => letNat(i, in => idx(e, Index(in) :: ps))
      case (Fst(_, _, e), ps) => idx(e, FstMember :: ps)
      case (Snd(_, _, e), ps) => idx(e, SndMember :: ps)
      case (Split(n, _, _, _, e), Index(i) :: Index(j) :: ps) => idx(e, Index(n * i + j) :: ps)
      case (Join(_, m, _, _, e), Index(i) :: ps) => idx(e, Index(i / m) :: Index(i % m) :: ps)
      case (Map(n, dt1, _, _, f, e), Index(i) :: ps) => idx(reduce(f, Idx(n, dt1, nat2idx(i, n), e)), ps)
      case (Generate(n, _, f), Index(i) :: ps) => idx(reduce(f, nat2idx(i, n)), ps)
      case (Zip(_, _, _, _, e1, e2), Index(i) :: (xj: PairAccess) :: ps) => xj match {
        case FstMember => idx(e1, Index(i) :: ps)
        case SndMember => idx(e2, Index(i) :: ps) }
      case (Unzip(_, _, _, _, e), (xj : PairAccess) :: Index(i) :: ps) => idx(e, Index(i) :: xj :: ps)
      case (Pair(_, _, _, e1, e2), (xj: PairAccess) :: ps) => xj match {
        case FstMember => idx(e1, ps)
        case SndMember => idx(e2, ps) }
      case (Take(_, _, _, e), ps) => idx(e, ps)
      case (Drop(n, _, _, e), Index(i) :: ps) => idx(e, Index(i + n) :: ps)
      case (Cycle(_, m, _, e), Index(i) :: ps) => idx(e, Index(i % m) :: ps)
      /*
      case (Reorder(n, _, _, idxF, _, e), Index(i) :: ps) =>
        // TODO: should we use operational semantics here instead? why?
        idx(e, Index(idx2nat(idxF(nat2idx(i, n)))) :: ps)
         */
      case (Slide(_, _, s2, _, e), Index(i) :: Index(j) :: ps) => idx(e, Index(i * s2 + j) :: ps)
      case (UnaryOp(op, e), Nil) => UnaryOp(op, idx(e, Nil))
      case (BinOp(op, e1, e2), Nil) => BinOp(op, idx(e1, Nil), idx(e2, Nil))

      case (e, ps) => ps.foldLeft(e)({
          case (e, Index(i)) => e.t match {
            case ExpType(ArrayType(n, dt), _) => Idx(n, dt, nat2idx(i, n), e)
            // TODO: do we need to add anything else here? dependent arrays?
            case _ => throw new Exception("this should not happen")
          }
          case (e, p: PairAccess) => e.t match {
            case ExpType(PairType(l, r), _) => p match {
              case FstMember => Fst(l, r, e)
              case SndMember => Snd(l, r, e)
            }
            case _ => throw new Exception("this should not happen")
          }
          // TODO: other path expressions
          case _ => ???
        })
    }
  }

  def idxAcc(p : Phrase[AccType], path : List[PathExpr]) : Phrase[AccType] = {
    (p, path) match {
      case (IdxAcc(_, _, i, a), ps) => idxAcc(a, Index(idx2nat(i)) :: ps)
      //case (IdxAcc(_, _, i, a), ps) => letNat(i , in => idxAcc(a, Index(in) :: ps))
      case (SplitAcc(n, _, _, a), Index(i) :: ps) => idxAcc(a, Index(i / n) :: Index(i % n) :: ps)
      case (JoinAcc(_, m, _, a), Index(i) :: Index(j) :: ps) => idxAcc(a, Index(i * m + j) :: ps)
        // FIXME: why operational semantics here?
      //case (ReorderAcc(n, _, idxF, a), Index(i) :: ps) => idxAcc(a, Index(OperationalSemantics.evalIndexExp(idxF(nat2idx(i, n)))) :: ps)
      case (a, ps) => ps.foldLeft(a)({
        case (a, Index(i)) => a.t match {
          case AccType(ArrayType(n, dt)) => IdxAcc(n, dt, nat2idx(i, n), a)
          case _ => throw new Exception("this should not happen") }
        case _ => ??? // TODO
      })
    }
  }
}
