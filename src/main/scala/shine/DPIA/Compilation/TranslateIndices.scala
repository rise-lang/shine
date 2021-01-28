package shine.DPIA.Compilation

import arithexpr.arithmetic
import rise.core.freshName
import shine.DPIA.primitives.functional._
import shine.DPIA.primitives.imperative._
import shine.DPIA.{Lifting, Nat}
import shine.DPIA.Semantics.OperationalSemantics
import shine.DPIA.Phrases._
import shine.DPIA.Types._
import shine.DPIA.Phrases.Operators.Binary.LT
import shine.OpenCL.primitives.imperative.{IdxDistribute, IdxDistributeAcc}

import scala.annotation.tailrec

/*
Translates transformations on index accessed arrays into transformations on the
indices, e.g, drop(3, xs)[5] is rewritten to xs[8]. The translation starts at
the outermost exp/acc phrase and goes down, accumulating path indices and
eliminating those phrases that can be translated by applying their effects to
the path. When a leave that cannot be translated is found, the path is dumped
as Idx/Fst/Snd phrases (and their Acc counterparts) that get prepended to the
expression.
 */

object TranslateIndices {

  def translate(p : Phrase[CommType]) : Phrase[CommType] = {
    VisitAndRebuild(p, new VisitAndRebuild.Visitor {
      override def phrase[T <: PhraseType](p: Phrase[T]): Result[Phrase[T]] = (p.t match {
        case ExpType(_, _) => Stop(idx(p.asInstanceOf[Phrase[ExpType]], Nil))
        case AccType(_) => Stop(idxAcc(p.asInstanceOf[Phrase[AccType]], Nil))
        case _ => Continue(p, this)
      }).asInstanceOf[Result[Phrase[T]]]
    })
  }

  // Interprets both naturals-as-indices and index variables as type level nats
  private def idx2nat : Phrase[ExpType] => Nat = {
    case Natural(n) => n
    case NatAsIndex(_, n) => idx2nat(n)
    case Idx(_, _, i, _) => idx2nat(i)
    case Cast(_, _, i) => idx2nat(i)
    case Identifier(i, ExpType(IndexType(n), _)) =>
      arithmetic.NamedVar(i, arithmetic.RangeAdd(0, n, 1))
    case p => throw new Exception(s"Could not use index expression $p as nat")
  }

  private def nat2idx(i : Nat, n : Nat) : Phrase[ExpType] = {
    NatAsIndex(n, Natural(i))
  }

  private def reduce[T1 <: PhraseType, T2 <: PhraseType](fun : Phrase[FunType[T1, T2]], arg : Phrase[T1]) : Phrase[T2]
    = Lifting.liftFunction(fun).reducing(arg)

  def fromPathT[T](p : T, path : List[PathExpr])(f: PartialFunction[List[PathExpr],  T]): T = {
    f.lift(path) match {
      case Some(p) => p
      case None => throw new Exception(s"Unexpected path $path for phrase $p.")
    }
  }

  @tailrec
  def zoomIntoType(dt : DataType, path : List[PathExpr]) : DataType =
    (dt, path) match {
      case (_, Nil) => dt
      case (ArrayType(_, edt), CIntExpr(_) :: ps) => zoomIntoType(edt, ps)
      case (PairType(fst, _), FstMember :: ps) => zoomIntoType(fst, ps)
      case (PairType(_, snd), SndMember :: ps) => zoomIntoType(snd, ps)
      case _ => throw new Exception(s"Cannot compute the datatype in $dt at path $path.")
    }

  def idx(p : Phrase[ExpType], path : List[PathExpr]) : Phrase[ExpType] = {
    def fromPath(f : PartialFunction[List[PathExpr], Phrase[ExpType]]) : Phrase[ExpType] = fromPathT(p, path)(f)

    p match {
      // Traverse AST
      case Idx(_, _, i, e)     => idx(e, CIntExpr(idx2nat(i)) :: path)
      case DepIdx(_, _, i, e)  => idx(e, CIntExpr(i) :: path)
      case Fst(_, _, e)        => idx(e, FstMember :: path)
      case Snd(_, _, e)        => idx(e, SndMember :: path)
      case IndexAsNat(n, e)    => IndexAsNat(n, idx(e, path))
      case NatAsIndex(n, e)    => NatAsIndex(n, idx(e, path))
      case IfThenElse(t, l, r) => IfThenElse(t, idx(l, path), idx(r, path))
      case Natural(n)          => fromPath { case Nil => Natural(n) }
      case Cast(m, n, e)       => fromPath { case Nil => Cast(m, n, idx(e, path)) }
      case UnaryOp(op, e)      => fromPath { case Nil => UnaryOp(op, idx(e, Nil)) }
      case BinOp(op, e1, e2)   => fromPath { case Nil => BinOp(op, idx(e1, Nil), idx(e2, Nil)) }
      case ff@ForeignFunctionCall(_, _, _, _) => ff
      case Continuation(dt, Lambda(cont, body)) =>
        val dt2 = zoomIntoType(dt, path)
        val cont2 = Identifier(freshName("k"), ExpType(dt2, read) ->: (comm: CommType))
        val body2 = VisitAndRebuild(body, new VisitAndRebuild.Visitor {
          override def phrase[T <: PhraseType](p: Phrase[T]): Result[Phrase[T]] = p match {
            case Apply(f, e) if f == cont =>
              Stop(Apply(cont2, idx(e.asInstanceOf[Phrase[ExpType]], path))
                .asInstanceOf[Phrase[T]])
            case _ => Continue(p, this)
          }
        })
        Continuation(dt2, Lambda(cont2, body2))

      // Eliminate index transforming primitives
      case Split(n, _, _, _, e) => fromPath {
        case CIntExpr(i) :: CIntExpr(j) :: ps => idx(e, CIntExpr(n * i + j) :: ps) }
      case IdxDistribute(_, _, s, _, _, e) => fromPath {
        // TODO: ensure that i % s == init ?
        case CIntExpr(i) :: ps => idx(e, CIntExpr(i / s) :: ps) }
      case Join(_, m, _, _, e) => fromPath {
        case CIntExpr(i) :: ps => idx(e, CIntExpr(i / m) :: CIntExpr(i % m) :: ps) }
      case Map(n, dt1, _, _, f, e) => fromPath {
        case CIntExpr(i) :: ps => idx(reduce(f, Idx(n, dt1, nat2idx(i, n), e)), ps) }
      case Generate(n, _, f) => fromPath {
        case CIntExpr(i) :: ps => idx(reduce(f, nat2idx(i, n)), ps) }
      case Zip(n, dt1, dt2, _, e1, e2) => fromPath {
        case CIntExpr(i) :: FstMember :: ps => idx(e1, CIntExpr(i) :: ps)
        case CIntExpr(i) :: SndMember :: ps => idx(e2, CIntExpr(i) :: ps)
        case CIntExpr(i) :: Nil =>
          val idx1 = Idx(n, dt1, nat2idx(n, i), e1)
          val idx2 = Idx(n, dt2, nat2idx(n, i), e2)
          idx(MakePair(dt1, dt2, read, idx1, idx2), Nil) }
      case DepZip(_, _, _, e1, e2) => fromPath {
        case CIntExpr(i) :: FstMember :: ps => idx(e1, CIntExpr(i) :: ps)
        case CIntExpr(i) :: SndMember :: ps => idx(e2, CIntExpr(i) :: ps) }
      case Gather(n, m, dt, y, e) => fromPath {
        case CIntExpr(i) :: ps =>
          val yi = Idx(m, IndexType(n), nat2idx(m, i), y)
          idx(Idx(n, dt, yi, e), ps) }
      case Transpose(_, _, _, _, e) => fromPath {
        case CIntExpr(i) :: CIntExpr(j) :: ps => idx(e, CIntExpr(j) :: CIntExpr(i) :: ps) }
      case TransposeDepArray(_, _, _, e) => fromPath {
        case CIntExpr(i) :: CIntExpr(j) :: ps => idx(e, CIntExpr(j) :: CIntExpr(i) :: ps) }
      case Unzip(_, _, _, _, e) => fromPath {
        case (xj : PairAccess) :: CIntExpr(i) :: ps => idx(e, CIntExpr(i) :: xj :: ps) }
      case MakePair(_, _, _, e1, e2) => fromPath {
        case FstMember :: ps => idx(e1, ps)
        case SndMember :: ps => idx(e2, ps) }
      case Take(_, _, _, e) => idx(e, path)
      case Drop(n, _, _, e) => fromPath {
        case CIntExpr(i) :: ps => idx(e, CIntExpr(i + n) :: ps) }
      case Cycle(_, m, _, e) => fromPath {
        case CIntExpr(i) :: ps => idx(e, CIntExpr(i % m) :: ps) }
      case part@Partition(_, _, _, _, e) => fromPath {
        case CIntExpr(i) :: CIntExpr(j) :: ps => idx(e, CIntExpr(arithmetic.BigSum(0, i - 1, x => part.lenF(x)) + j) :: ps) }
      case Reorder(n, _, _, idxF, _, e) => fromPath {
        case CIntExpr(i) :: ps => idx(e, CIntExpr(OperationalSemantics.evalIndexExp(reduce(idxF, nat2idx(i, n)))) :: ps) }
      case Slide(_, _, s2, _, e) => fromPath {
        case CIntExpr(i) :: CIntExpr(j) :: ps => idx(e, CIntExpr(i * s2 + j) :: ps) }
      case PadClamp(n, l, r, _, e) => fromPath {
        case CIntExpr(i) :: ps =>
          val lt : Nat => Nat => arithmetic.BoolExpr = l => r =>
            arithmetic.BoolExpr.ArithPredicate(l, r, arithmetic.BoolExpr.ArithPredicate.Operator.<)
          val j = arithmetic.IfThenElse(l `lt` r, 0, arithmetic.IfThenElse(r `lt` (i + 1), n - 1, i - l))
          idx(e, CIntExpr(j) :: ps) }
      case Pad(n, l, r, _, pad, e) => fromPath {
        case CIntExpr(i) :: ps =>
          val rec = idx(pad, ps)
          IfThenElse(BinOp(LT, Natural(i), Natural(l)), rec,
            IfThenElse(BinOp(LT, Natural(r), Natural(i + 1)), rec,
              idx(e, CIntExpr(i - l) :: ps))) }
      case VectorFromScalar(_, _, e) => fromPath {
        case CIntExpr(i) :: ps => idx(e, ps) }
      case MakeArray(_, elems) => fromPath {
        case CIntExpr(i) :: ps => idx(elems(i.eval), ps) }

      case Identifier(_ , _)
         | Literal(_)
         | Proj1(_)
         | Proj2(_)
         | Apply(_, _)
         | DepApply(_, _)
         | LetNat(_, _, _)
      // Write back Idx, Fst, Snd: code generation will need to deal with these
      => path.foldLeft(p)({
          case (e, CIntExpr(i)) => e.t match {
            case ExpType(ArrayType(n, dt), _) => Idx(n, dt, nat2idx(i, n), e)
            case ExpType(DepArrayType(n, dt), _) => DepIdx(n, dt, i, e)
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

      case _ : ExpPrimitive => throw new Exception(s"Cannot index-translate primitive $p")
    }
  }

  def idxAcc(p : Phrase[AccType], path : List[PathExpr]) : Phrase[AccType] = {
    def fromPath(f : PartialFunction[List[PathExpr], Phrase[AccType]]) : Phrase[AccType] = fromPathT(p, path)(f)

    p match {
      case IdxAcc(_, _, i, a)     => idxAcc(a, CIntExpr(idx2nat(i)) :: path)
      case DepIdxAcc(_, _, i, a)  => idxAcc(a, CIntExpr(i) :: path)
      case MkDPairSndAcc(_, _, a) => idxAcc(a, DPairSnd :: path)
      case PairAcc1(_, _, a)      => idxAcc(a, FstMember :: path)
      case PairAcc2(_, _, a)      => idxAcc(a, SndMember :: path)
      case IfThenElse(t, l, r)    => IfThenElse(t, idxAcc(l, path), idxAcc(r, path))
      case ScatterAcc(n, m, d, y, a) => ScatterAcc(n, m, d, y, idxAcc(a, path))

      case ZipAcc1(_, _, _, a) => fromPath {
        case CIntExpr(i) :: ps => idxAcc(a, CIntExpr(i) :: FstMember :: ps) }
      case ZipAcc2(_, _, _, a) => fromPath {
        case CIntExpr(i) :: ps => idxAcc(a, CIntExpr(i) :: SndMember :: ps) }
      case UnzipAcc(_, _, _, a) => fromPath {
        case CIntExpr(i) :: FstMember :: ps => idxAcc(a, FstMember :: CIntExpr(i) :: ps)
        case CIntExpr(i) :: SndMember :: ps => idxAcc(a, SndMember :: CIntExpr(i) :: ps) }
      case TakeAcc(_, _, _, a) => idxAcc(a, path)
      case SplitAcc(n, _, _, a) => fromPath {
        case CIntExpr(i) :: ps => idxAcc(a, CIntExpr(i / n) :: CIntExpr(i % n) :: ps) }
      case JoinAcc(_, m, _, a) => fromPath {
        case CIntExpr(i) :: CIntExpr(j) :: ps => idxAcc(a, CIntExpr(i * m + j) :: ps) }
      case DropAcc(n, _, _, a) => fromPath {
        case CIntExpr(i) :: ps => idxAcc(a, CIntExpr(i + n) :: ps) }
      case ReorderAcc(n, _, idxF, a) => fromPath {
        case CIntExpr(i) :: ps => idxAcc(a, CIntExpr(OperationalSemantics.evalIndexExp(reduce(idxF, nat2idx(i, n)))) :: ps) }
      case depJ@DepJoinAcc(_, _, _, a) => fromPath {
        case CIntExpr(i) :: CIntExpr(j) :: ps => idxAcc(a, CIntExpr(arithmetic.BigSum(0, i - 1, x => depJ.lenF(x)) + j) :: ps) }
      case TransposeAcc(_, _, _, a) => fromPath {
        case CIntExpr(i) :: CIntExpr(j) :: ps => idxAcc(a, CIntExpr(j) :: CIntExpr(i) :: ps) }
      case CycleAcc(_, m, _, a) => fromPath {
        case CIntExpr(i) :: ps => idxAcc(a, CIntExpr(i % m) :: ps) }
      case MapAcc(n, dt, _, f, a) => fromPath {
        case CIntExpr(i) :: ps => idxAcc(reduce(f, IdxAcc(n, dt, nat2idx(i, n), a) ), ps) }
      case MapFstAcc(_, dt2, dt3, f, a) => fromPath {
        case FstMember :: ps => idxAcc(reduce(f, PairAcc1(dt3, dt2, a)), ps)
        case SndMember :: ps => idxAcc(          PairAcc2(dt3, dt2, a) , ps) }
      case MapSndAcc(dt1, _, dt3, f, a) => fromPath {
        case FstMember :: ps => idxAcc(          PairAcc1(dt1, dt3, a) , ps)
        case SndMember :: ps => idxAcc(reduce(f, PairAcc2(dt1, dt3, a)), ps) }
      case IdxDistributeAcc(_, _, s, _, _, a) => fromPath {
        // TODO: ensure that i % s == init ?
        case CIntExpr(i) :: ps => idxAcc(a, CIntExpr(i / s) :: ps) }
      case AsVectorAcc(n, _, _, a) => fromPath {
        case CIntExpr(i) :: ps => idxAcc(a, CIntExpr(i / n) :: ps) }
      case AsScalarAcc(_, m, _, a) => fromPath {
        // TODO: deal with the single path element case
        case CIntExpr(i) :: CIntExpr(j) :: ps => idxAcc(a, CIntExpr((i * m) + j) :: ps) }


        case Identifier(_ , _)
         | Apply(_, _)
         | Proj1(_)
         | Proj2(_)
         | DepApply(_, _)
         | LetNat(_, _, _)
      // Write back IdxAcc, PairAcc1, PairAcc2: code generation will need to deal with these
      => path.foldLeft(p)({
        case (a, CIntExpr(i)) => a.t match {
          case AccType(ArrayType(n, dt)) => IdxAcc(n, dt, nat2idx(i, n), a)
          case _ => throw new Exception("this should not happen") }
        case (e, p: PairAccess) => e.t match {
          case AccType(PairType(l, r)) => p match {
            case FstMember => PairAcc1(l, r, e)
            case SndMember => PairAcc2(l, r, e) }
          case _ => throw new Exception("this should not happen")
        }
        case _ => ??? // TODO
      })
      case _ : AccPrimitive => throw new Exception(s"Cannot index-translate primitive $p")
    }
  }
}
