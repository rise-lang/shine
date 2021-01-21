package shine.DPIA.Compilation

import arithexpr.arithmetic.BigSum
import shine.C.CodeGeneration.CodeGenerator._
import shine.DPIA.FunctionalPrimitives._
import shine.DPIA.ImperativePrimitives._
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

  private def reduce[T1 <: PhraseType, T2 <: PhraseType](fun : Phrase[FunType[T1, T2]], arg : Phrase[T1]) : Phrase[T2]
    = Lifting.liftFunction(fun).reducing(arg)

  def idx(p : Phrase[ExpType], path : List[PathExpr]) : Phrase[ExpType] = {
    (p, path) match {
      case (Idx(_, _, i, e), ps) => idx(e, CIntExpr(idx2nat(i)) :: ps)
      //case (Idx(_, _, i, e), ps) => letNat(i, in => idx(e, CIntExpr(in) :: ps))
      case (Fst(_, _, e), ps) => idx(e, FstMember :: ps)
      case (Snd(_, _, e), ps) => idx(e, SndMember :: ps)
      case (Split(n, _, _, _, e), CIntExpr(i) :: CIntExpr(j) :: ps) => idx(e, CIntExpr(n * i + j) :: ps)
      case (Join(_, m, _, _, e), CIntExpr(i) :: ps) => idx(e, CIntExpr(i / m) :: CIntExpr(i % m) :: ps)
      case (Map(n, dt1, _, _, f, e), CIntExpr(i) :: ps) => idx(reduce(f, Idx(n, dt1, nat2idx(i, n), e)), ps)
      case (Generate(n, _, f), CIntExpr(i) :: ps) => idx(reduce(f, nat2idx(i, n)), ps)
      case (Zip(_, _, _, _, e1, e2), CIntExpr(i) :: (xj: PairAccess) :: ps) => xj match {
        case FstMember => idx(e1, CIntExpr(i) :: ps)
        case SndMember => idx(e2, CIntExpr(i) :: ps) }
      case (DepZip(_, _, _, e1, e2), CIntExpr(i) :: (xj : PairAccess) :: ps) => xj match {
        case FstMember => idx(e1, CIntExpr(i) :: ps)
        case SndMember => idx(e2, CIntExpr(i) :: ps)
      }
      case (Gather(n, m, dt, y, e), CIntExpr(i) :: ps) => {
          val yi = Idx(m, IndexType(n), nat2idx(m, i), y)
          idx(Idx(n, dt, yi, e), ps)
      }
      case (Transpose(_, _, _, _, e), CIntExpr(i) :: CIntExpr(j) :: ps) => idx(e, CIntExpr(j) :: CIntExpr(i) :: ps)
      case (TransposeDepArray(_, _, _, e), CIntExpr(i) :: CIntExpr(j) :: ps) => idx(e, CIntExpr(j) :: CIntExpr(i) :: ps)
      case (Unzip(_, _, _, _, e), (xj : PairAccess) :: CIntExpr(i) :: ps) => idx(e, CIntExpr(i) :: xj :: ps)
      case (Pair(_, _, _, e1, e2), (xj: PairAccess) :: ps) => xj match {
        case FstMember => idx(e1, ps)
        case SndMember => idx(e2, ps) }
      case (Take(_, _, _, e), ps) => idx(e, ps)
      case (Drop(n, _, _, e), CIntExpr(i) :: ps) => idx(e, CIntExpr(i + n) :: ps)
      case (Cycle(_, m, _, e), CIntExpr(i) :: ps) => idx(e, CIntExpr(i % m) :: ps)
      case (part@Partition(_, _, _, _, e), CIntExpr(i) :: CIntExpr(j) :: ps) =>
        idx(e, CIntExpr(BigSum(0, i - 1, x => part.lenF(x)) + j) :: ps)
      case (Reorder(n, _, _, idxF, _, e), CIntExpr(i) :: ps) =>
        idx(e, CIntExpr(OperationalSemantics.evalIndexExp(reduce(idxF, nat2idx(i, n)))) :: ps)
      case (Slide(_, _, s2, _, e), CIntExpr(i) :: CIntExpr(j) :: ps) => idx(e, CIntExpr(i * s2 + j) :: ps)
      case (UnaryOp(op, e), Nil) => UnaryOp(op, idx(e, Nil))
      case (BinOp(op, e1, e2), Nil) => BinOp(op, idx(e1, Nil), idx(e2, Nil))

      case (e, ps) => ps.foldLeft(e)({
          case (e, CIntExpr(i)) => e.t match {
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
      case (IdxAcc(_, _, i, a), ps) => idxAcc(a, CIntExpr(idx2nat(i)) :: ps)
      //case (IdxAcc(_, _, i, a), ps) => letNat(i , in => idxAcc(a, CIntExpr(in) :: ps))
      case (SplitAcc(n, _, _, a), CIntExpr(i) :: ps) => idxAcc(a, CIntExpr(i / n) :: CIntExpr(i % n) :: ps)
      case (JoinAcc(_, m, _, a), CIntExpr(i) :: CIntExpr(j) :: ps) => idxAcc(a, CIntExpr(i * m + j) :: ps)
      case (TakeAcc(_, _, _, a), ps) => idxAcc(a, path)
      case (DropAcc(n, _, _, a), CIntExpr(i) :: ps) => idxAcc(a, CIntExpr(i + n) :: ps)
      case (ReorderAcc(n, _, idxF, a), CIntExpr(i) :: ps) =>
        idxAcc(a, CIntExpr(OperationalSemantics.evalIndexExp(reduce(idxF, nat2idx(i, n)))) :: ps)
      case (depJ@DepJoinAcc(_, _, _, a), CIntExpr(i) :: CIntExpr(j) :: ps) =>
        idxAcc(a, CIntExpr(BigSum(0, i - 1, x => depJ.lenF(x)) + j) :: ps)
      case (TransposeAcc(_, _, _, a), CIntExpr(i) :: CIntExpr(j) :: ps) =>
        idxAcc(a, CIntExpr(j) :: CIntExpr(i) :: ps)
      case (CycleAcc(_, m, _, a), CIntExpr(i) :: ps) => idxAcc(a, CIntExpr(i % m) :: ps)
      case (MapAcc(n, dt, _, f, a), CIntExpr(i) :: ps) => idxAcc(reduce(f, IdxAcc(n, dt, nat2idx(n, i), a) ), ps)
      case (MapFstAcc(_, dt2, dt3, f, a), (xi : PairAccess) :: ps) => xi match {
        case FstMember => idxAcc(reduce(f, PairAcc1(dt3, dt2, a)), ps)
        case SndMember => idxAcc(          PairAcc2(dt3, dt2, a) , ps)
      }
      case (MapSndAcc(dt1, _, dt3, f, a), (xi : PairAccess) :: ps) => xi match {
        case FstMember => idxAcc(          PairAcc1(dt1, dt3, a) , ps)
        case SndMember => idxAcc(reduce(f, PairAcc2(dt1, dt3, a)), ps)
      }
      case (DepIdxAcc(_, _, i, a), ps) => idxAcc(a, CIntExpr(i) :: ps)
      case (MkDPairSndAcc(_, _, a), ps) => idxAcc(a, DPairSnd :: ps)




      case (a, ps) => ps.foldLeft(a)({
        case (a, CIntExpr(i)) => a.t match {
          case AccType(ArrayType(n, dt)) => IdxAcc(n, dt, nat2idx(i, n), a)
          case _ => throw new Exception("this should not happen") }
        case _ => ??? // TODO
      })
    }
  }
}
