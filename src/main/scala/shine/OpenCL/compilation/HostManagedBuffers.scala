package shine.OpenCL.compilation

import shine.DPIA._
import shine.DPIA.Phrases._
import shine.DPIA.Semantics.OperationalSemantics
import shine.DPIA.Types._
import shine.DPIA.primitives.imperative._
import shine.OpenCL._
import shine.OpenCL.primitives.imperative._

import scala.annotation.tailrec
import scala.collection.{immutable, mutable}

object HostManagedBuffers {
  def populate(params: immutable.Seq[Identifier[ExpType]], outParam: Identifier[AccType]): Phrase[CommType] => Phrase[CommType] = { p =>
    def outsideParams() = mutable.Set[Identifier[_ <: PhraseType]]() ++ (params :+ outParam)
    val worstCasePrevious = Metadata(outsideParams(), outsideParams(), outsideParams(), outsideParams())
    val managed = mutable.Set[Identifier[_ <: PhraseType]]()
    insertHostExecutions(worstCasePrevious, outsideParams().toSet, managed, p)._1 |>
    insertManagedBuffers(managed)
  }

  final case class HostExecution(env: Map[Identifier[_ <: PhraseType], AccessFlags],
                                 execute: Phrase[CommType]) extends CommandPrimitive {
    execute :: comm

    override def visitAndRebuild(f: VisitAndRebuild.Visitor): Phrase[CommType] =
      HostExecution(env.map({ case (k, v) => VisitAndRebuild(k, f).asInstanceOf[Identifier[_ <: PhraseType]] -> v }),
        VisitAndRebuild(execute, f))

    override def eval(s: OperationalSemantics.Store): OperationalSemantics.Store = ???
    override def prettyPrint: String = ???
    override def xmlPrinter: xml.Elem = ???
  }

  // effects to outer scope allocations
  private case class Metadata(
    host_reads: mutable.Set[Identifier[_ <: PhraseType]],
    host_writes: mutable.Set[Identifier[_ <: PhraseType]],
    target_reads: mutable.Set[Identifier[_ <: PhraseType]],
    target_writes: mutable.Set[Identifier[_ <: PhraseType]])

  private object Metadata {
    def empty: Metadata = Metadata(mutable.Set(), mutable.Set(), mutable.Set(), mutable.Set())
  }

  private def insertHostExecutions(previous: Metadata,
                                   allocs: Set[Identifier[_ <: PhraseType]],
                                   managed: mutable.Set[Identifier[_ <: PhraseType]],
                                   p: Phrase[CommType]): (Phrase[CommType], Metadata) = {
    val (p2, current) = analyzeAndInsertHostExecution(p, allocs, managed)
    val syncAccesses = previous.target_reads.union(previous.target_writes)
      .intersect(current.host_reads.union(current.host_writes))
    if (syncAccesses.isEmpty) {
      current.host_reads ++= previous.host_reads
      current.host_writes ++= previous.host_writes
      current.target_reads ++= previous.target_reads
      current.target_writes ++= previous.target_writes
      (p2, current)
    } else {
      assert(current.target_reads.isEmpty)
      assert(current.target_writes.isEmpty)
      val managedEnv = (current.host_reads ++ current.host_writes).filter(optionallyManaged(_).isDefined).toSeq
      managed ++= managedEnv
      val env = managedEnv.map(i => {
        var access = 0
        if (current.host_reads.contains(i)) { access |= HOST_READ }
        if (current.host_writes.contains(i)) { access |= HOST_WRITE }
        i -> access
      }).toMap
      (HostExecution(env, p2), Metadata.empty)
    }
  }

  private def analyzeAndInsertHostExecution(
    p: Phrase[CommType],
    allocs: Set[Identifier[_ <: PhraseType]],
    managed: mutable.Set[Identifier[_ <: PhraseType]]
  ): (Phrase[CommType], Metadata) = {
    val meta = Metadata(mutable.Set(), mutable.Set(), mutable.Set(), mutable.Set())
    val p2 = VisitAndRebuild(p, Visitor(allocs, managed, meta))
    (p2, meta)
  }

  private case class Visitor(allocs: Set[Identifier[_ <: PhraseType]],
                             managed: mutable.Set[Identifier[_ <: PhraseType]],
                             metadata: Metadata) extends VisitAndRebuild.Visitor {
    override def phrase[T <: PhraseType](p: Phrase[T]): Result[Phrase[T]] =
      p match {
        case Assign(_, lhs, rhs) =>
          collectWrites(lhs, metadata.host_writes)
          collectReads(rhs, allocs, metadata.host_reads)
          Stop(p)
        case KernelCallCmd(_, _, _, out, in) =>
          in.foreach(collectReads(_, allocs, metadata.target_reads))
          collectWrites(out, metadata.target_writes)
          (out +: in).foreach {
            case i: Identifier[_] => managed += i
            case Proj1(i: Identifier[_]) => managed += i
            case Proj2(i: Identifier[_]) => managed += i
            case Natural(_) =>
            case unexpected => throw new Exception(s"did not expect $unexpected")
          }
          Stop(p)
        case Seq(a, b) =>
          val (a2, am) = analyzeAndInsertHostExecution(a, allocs, managed)
          val (b2, bm) = insertHostExecutions(am, allocs, managed, b)
          metadata.host_writes ++= bm.host_writes
          metadata.host_reads ++= bm.host_reads
          metadata.target_writes ++= bm.target_writes
          metadata.target_reads ++= bm.target_reads
          Stop(Seq(a2, b2))
        case _ => Continue(p, this)
      }
  }

  def optionallyManaged(i: Identifier[_ <: PhraseType]): Option[(Identifier[_ <: PhraseType], DataType)] = i.`type` match {
    case ExpType(_: ScalarType, _) | ExpType(_: IndexType, _) => None
    case ExpType(dt, a) => Some(Identifier("m" + i.name, ExpType(ManagedBufferType(dt), a)), dt)
    case AccType(_: ScalarType) => None
    case AccType(dt) => Some(Identifier("m" + i.name, AccType(ManagedBufferType(dt))), dt)
    case PhrasePairType(ExpType(_: ScalarType, _), AccType(_)) => None
    case PhrasePairType(ExpType(dt, a), AccType(_)) => Some(Identifier("m" + i.name,
      PhrasePairType(ExpType(ManagedBufferType(dt), a), AccType(ManagedBufferType(dt)))), dt)
    case _ => throw new Exception("this should not happen")
  }

  private def insertManagedBuffers(
    managed: mutable.Set[Identifier[_ <: PhraseType]]
  ): Phrase[CommType] => Phrase[CommType] = p => {
    VisitAndRebuild(p, Visitor2(managed.iterator.flatMap(i => optionallyManaged(i).map(i -> _._1)).toMap))
  }

  private case class Visitor2(managed: Map[Identifier[_ <: PhraseType], Identifier[_ <: PhraseType]]) extends VisitAndRebuild.Visitor {
    override def phrase[T <: PhraseType](p: Phrase[T]): Result[Phrase[T]] =
      p match {
        case i: Identifier[_] =>
          Stop(managed.getOrElse(i, p).asInstanceOf[Phrase[T]])
        case New(dt, Lambda(x, body)) if managed.contains(x) =>
          // TODO: access
          Continue(NewManagedBuffer(dt, 0, Lambda(managed(x).asInstanceOf[Identifier[VarType]], body)), this)
        case _: New | _: Lambda[_, _] | _: Seq | _: Proj2[_, _] | _: Proj1[_, _] | Natural(_) =>
          Continue(p, this)
        case _: KernelCallCmd => Continue(p, this)
        case _: HostExecution => Stop(p)
        case unexpected => throw new Exception(s"did not expect $unexpected")
      }
  }

  @tailrec
  private def collectWrites(a: Phrase[AccType],
                            writes: mutable.Set[Identifier[_ <: PhraseType]]): Unit = {
    def addIdent(i: Identifier[_ <: PhraseType]): Unit =
      writes += i

    a match {
      case i: Identifier[_] => addIdent(i)
      case Proj1(p) => projCollectIdent(p, addIdent)
      case Proj2(p) => projCollectIdent(p, addIdent)
      // TODO: collect reads in index?
      case IdxAcc(_, _, _, a) => collectWrites(a, writes)
      case MapAcc(_, _, _, _, a) => collectWrites(a, writes)
      case JoinAcc(_, _, _, a) => collectWrites(a, writes)
      case SplitAcc(_, _, _, a) => collectWrites(a, writes)
      case AsScalarAcc(_, _, _, a) => collectWrites(a, writes)
      case IdxDistributeAcc(_, _, _, _, _, a) => collectWrites(a, writes)
      case PairAcc1(_, _, a) => collectWrites(a, writes)
      case PairAcc2(_, _, a) => collectWrites(a, writes)
      case TakeAcc(_, _, _, a) => collectWrites(a, writes)
      case TransposeAcc(_, _, _, a) => collectWrites(a, writes)
      case _ => throw new Exception(s"did not expect $a")
    }
  }

  private def collectReads(e: Phrase[ExpType],
                           allocs: Set[Identifier[_ <: PhraseType]],
                           reads: mutable.Set[Identifier[_ <: PhraseType]]): Unit = {
    import shine.DPIA.primitives.functional._

    def addIdent(i: Identifier[_ <: PhraseType]): Unit =
      reads += i

    def giveUp(): Unit = reads ++= allocs

    // TODO: many patterns could be eliminated before this pass
    e match {
      case i: Identifier[_] => addIdent(i)
      case Proj1(p) => projCollectIdent(p, addIdent)
      case Proj2(p) => projCollectIdent(p, addIdent)
      case _: Literal =>
      case Natural(_) =>
      case BinOp(_, e1, e2) =>
        collectReads(e1, allocs, reads); collectReads(e2, allocs, reads)
      case Idx(_, _, e1, e2) =>
        collectReads(e1, allocs, reads); collectReads(e2, allocs, reads)
      case Slide(_, _, _, _, e) => collectReads(e, allocs, reads)
      case Map(_, _, _, _, _, e) => collectReads(e, allocs, reads)
      case IdxDistribute(_, _, _, _, _, e) => collectReads(e, allocs, reads)
      case MapRead(_, _, _, _, e) => collectReads(e, allocs, reads)
      case GenerateCont(_, _, _) => giveUp()
      case AsScalar(_, _, _, _, e) => collectReads(e, allocs, reads)
      case AsVectorAligned(_, _, _, _, e) => collectReads(e, allocs, reads)
      case AsVector(_, _, _, _, e) => collectReads(e, allocs, reads)
      case VectorFromScalar(_, _, e) => collectReads(e, allocs, reads)
      case Fst(_, _, e) => collectReads(e, allocs, reads)
      case Snd(_, _, e) => collectReads(e, allocs, reads)
      case Transpose(_, _, _, _, e) => collectReads(e, allocs, reads)
      case Join(_, _, _, _, e) => collectReads(e, allocs, reads)
      case Split(_, _, _, _, e) => collectReads(e, allocs, reads)
      case Zip(_, _, _, _, e1, e2) =>
        collectReads(e1, allocs, reads); collectReads(e2, allocs, reads)
      case Pad(_, _, _, _, e1, e2) =>
        collectReads(e1, allocs, reads); collectReads(e2, allocs, reads)
      case PadClamp(_, _, _, _, e) =>
        collectReads(e, allocs, reads)
      case Cast(_, _, e) => collectReads(e, allocs, reads)
      case ForeignFunctionCall(_, _, _, es) =>
        es.foreach {
          collectReads(_, allocs, reads)
        }
      case NatAsIndex(_, e) => collectReads(e, allocs, reads)
      case Drop(_, _, _, e) => collectReads(e, allocs, reads)
      case Take(_, _, _, e) => collectReads(e, allocs, reads)
      case Unzip(_, _, _, _, e) => collectReads(e, allocs, reads)
      case Pair(_, _, _, e1, e2) =>
        collectReads(e1, allocs, reads); collectReads(e2, allocs, reads)
      case Reorder(_, _, _, _, _, e) => collectReads(e, allocs, reads)
      case MakeArray(_, es) =>
        es.foreach {
          collectReads(_, allocs, reads)
        }
      case Gather(_, _, _, e1, e2) =>
        collectReads(e1, allocs, reads); collectReads(e2, allocs, reads)
      case _ => throw new Exception(s"did not expect $e")
    }
  }


  @tailrec
  private def projCollectIdent(e: Phrase[_ <: PhraseType],
                               addIdent: Identifier[_ <: PhraseType] => Unit): Unit = {
    e match {
      case i: Identifier[_] => addIdent(i)
      case Proj1(e) => projCollectIdent(e, addIdent)
      case Proj2(e) => projCollectIdent(e, addIdent)
      case _ => throw new Exception(s"did not expect $e")
    }
  }
}
