package shine.DPIA.Compilation

import arithexpr.arithmetic
import rise.core.freshName
import shine.DPIA.FunctionalPrimitives._
import shine.DPIA.ImperativePrimitives._
import shine.DPIA.{LetNatIdentifier, Lifting, Nat, NatIdentifier}
import shine.DPIA.Semantics.OperationalSemantics
import shine.DPIA.Phrases._
import shine.DPIA.Types._
import shine.DPIA.Phrases.Operators.Binary.LT

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
      arithmetic.NamedVar(i, arithmetic.RangeAdd(0, n, 1))
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
    def fromPath[T](f: PartialFunction[List[PathExpr],  T]): T = {
      f.lift(path) match {
        case Some(p) => p
        case None => throw new Exception(s"Unexpected path for $p : $path")
      }
    }

    p match {
      // Traverse AST
      case Idx(_, _, i, e)    => idx(e, CIntExpr(idx2nat(i)) :: path)
      case DepIdx(_, _, i, e) => idx(e, CIntExpr(i) :: path)
      case Fst(_, _, e)       => idx(e, FstMember :: path)
      case Snd(_, _, e)       => idx(e, SndMember :: path)
      case UnaryOp(op, e)     => fromPath { case Nil => UnaryOp(op, idx(e, Nil)) }
      case BinOp(op, e1, e2)  => fromPath { case Nil => BinOp(op, idx(e1, Nil), idx(e2, Nil)) }
      case Continuation(dt, Lambda(cont, body)) =>
        def continuationDataType(dt: DataType): Int => DataType = {
          case 0 => dt
          case n => dt match {
            case ArrayType(_, edt) => continuationDataType(edt)(n - 1)
            case _ => ???
          }
        }
        val dt2 = continuationDataType(dt)(path.length)
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
          idx(Pair(dt1, dt2, read, idx1, idx2), Nil) }
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
      case Pair(_, _, _, e1, e2) => fromPath {
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
          idx(e, CIntExpr(j) :: ps)
      }
      case Pad(n, l, r, _, pad, e) => fromPath {
        case CIntExpr(i) :: ps =>
          val rec = idx(pad, ps)
          IfThenElse(BinOp(LT, Natural(i), Natural(l)), rec,
            IfThenElse(BinOp(LT, Natural(r), Natural(i + 1)), rec,
              idx(e, CIntExpr(i - l) :: ps)))
      }

      case Identifier(_ , _)
         | Literal(_)
         | Natural(_)
         | Cast(_, _, _)
         | Apply(_, _)
         | Proj1(_)
         | Proj2(_)
         | DepApply(_, _)
         | IfThenElse(_, _, _)
         | LetNat(_, _, _)
      => path.foldLeft(p)({
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

      case _ : ExpPrimitive => throw new Exception(s"Cannot index-translate primitive $p")
    }
  }

  def idxAcc(p : Phrase[AccType], path : List[PathExpr]) : Phrase[AccType] = {
    (p, path) match {
      case (IdxAcc(_, _, i, a), path) => idxAcc(a, CIntExpr(idx2nat(i)) :: path)
      //case (IdxAcc(_, _, i, a), path) => letNat(i , in => idxAcc(a, CIntExpr(in) :: path))
      case (SplitAcc(n, _, _, a), CIntExpr(i) :: path) => idxAcc(a, CIntExpr(i / n) :: CIntExpr(i % n) :: path)
      case (JoinAcc(_, m, _, a), CIntExpr(i) :: CIntExpr(j) :: path) => idxAcc(a, CIntExpr(i * m + j) :: path)
      case (TakeAcc(_, _, _, a), path) => idxAcc(a, path)
      case (DropAcc(n, _, _, a), CIntExpr(i) :: path) => idxAcc(a, CIntExpr(i + n) :: path)
      case (ReorderAcc(n, _, idxF, a), CIntExpr(i) :: path) =>
        idxAcc(a, CIntExpr(OperationalSemantics.evalIndexExp(reduce(idxF, nat2idx(i, n)))) :: path)
      case (depJ@DepJoinAcc(_, _, _, a), CIntExpr(i) :: CIntExpr(j) :: path) =>
        idxAcc(a, CIntExpr(arithmetic.BigSum(0, i - 1, x => depJ.lenF(x)) + j) :: path)
      case (TransposeAcc(_, _, _, a), CIntExpr(i) :: CIntExpr(j) :: path) =>
        idxAcc(a, CIntExpr(j) :: CIntExpr(i) :: path)
      case (CycleAcc(_, m, _, a), CIntExpr(i) :: path) => idxAcc(a, CIntExpr(i % m) :: path)
      case (MapAcc(n, dt, _, f, a), CIntExpr(i) :: path) => idxAcc(reduce(f, IdxAcc(n, dt, nat2idx(n, i), a) ), path)
      case (MapFstAcc(_, dt2, dt3, f, a), (xi : PairAccess) :: path) => xi match {
        case FstMember => idxAcc(reduce(f, PairAcc1(dt3, dt2, a)), path)
        case SndMember => idxAcc(          PairAcc2(dt3, dt2, a) , path)
      }
      case (MapSndAcc(dt1, _, dt3, f, a), (xi : PairAccess) :: path) => xi match {
        case FstMember => idxAcc(          PairAcc1(dt1, dt3, a) , path)
        case SndMember => idxAcc(reduce(f, PairAcc2(dt1, dt3, a)), path)
      }
      case (DepIdxAcc(_, _, i, a), path) => idxAcc(a, CIntExpr(i) :: path)
      case (MkDPairSndAcc(_, _, a), path) => idxAcc(a, DPairSnd :: path)




      case (a, path) => path.foldLeft(a)({
        case (a, CIntExpr(i)) => a.t match {
          case AccType(ArrayType(n, dt)) => IdxAcc(n, dt, nat2idx(i, n), a)
          case _ => throw new Exception("this should not happen") }
        case _ => ??? // TODO
      })
    }
  }
}
