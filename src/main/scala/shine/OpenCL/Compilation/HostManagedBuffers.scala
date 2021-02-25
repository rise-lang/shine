package shine.OpenCL.Compilation

import shine.DPIA._
import shine.DPIA.Phrases._
import shine.DPIA.Types._
import shine.DPIA.primitives.{imperative => dpia}
import shine.OpenCL._
import shine.OpenCL.primitives.imperative.HostExecution
import shine.OpenCL.primitives.{imperative => ocl}

import scala.annotation.tailrec
import scala.collection.{immutable, mutable}

object HostManagedBuffers {
  // Given a host program which only operates on plain arrays,
  // this pass inserts host managed buffers where needed,
  // as well as host execution sections where plain arrays can still be used
  def insert(params: immutable.Seq[Identifier[ExpType]], outParam: Identifier[AccType])
  : Phrase[CommType] => Phrase[CommType] = { p =>
    def outsideParams() = mutable.Set[Identifier[_ <: PhraseType]]() ++ (params :+ outParam)
    val worstCasePrevious =
      Metadata(outsideParams(), outsideParams(), outsideParams(), outsideParams())
    val managed = mutable.Map[Identifier[_ <: PhraseType], AccessFlags]()
    insertHostExecutions(worstCasePrevious, outsideParams().toSet, managed, p)._1 |>
    insertManagedBuffers(managed)
  }


  // effects to outer scope allocations
  private case class Metadata(
    host_reads: mutable.Set[Identifier[_ <: PhraseType]],
    host_writes: mutable.Set[Identifier[_ <: PhraseType]],
    device_reads: mutable.Set[Identifier[_ <: PhraseType]],
    device_writes: mutable.Set[Identifier[_ <: PhraseType]])

  private object Metadata {
    def empty: Metadata = Metadata(mutable.Set(), mutable.Set(), mutable.Set(), mutable.Set())
  }

  private def recordManagedAccess(
    managed: mutable.Map[Identifier[_ <: PhraseType], AccessFlags],
    ident: Identifier[_ <: PhraseType],
    access: AccessFlags
  ): Unit = {
    managed.updateWith(ident)(prev => Some(prev.getOrElse(0) | access))
  }

  private def insertHostExecutions(
    previous: Metadata,
    allocs: Set[Identifier[_ <: PhraseType]],
    managed: mutable.Map[Identifier[_ <: PhraseType], AccessFlags],
    p: Phrase[CommType]
  ): (Phrase[CommType], Metadata) = {
    val (p2, current) = analyzeAndInsertHostExecution(p, allocs, managed)
    val syncAccesses = previous.device_reads.union(previous.device_writes)
      .intersect(current.host_reads.union(current.host_writes))
    if (syncAccesses.isEmpty) {
      current.host_reads ++= previous.host_reads
      current.host_writes ++= previous.host_writes
      current.device_reads ++= previous.device_reads
      current.device_writes ++= previous.device_writes
      (p2, current)
    } else {
      assert(current.device_reads.isEmpty)
      assert(current.device_writes.isEmpty)
      val env = (current.host_reads ++ current.host_writes)
        .filter(optionallyManaged(_).isDefined).map(i => {
        var access = 0
        if (current.host_reads.contains(i)) { access |= HOST_READ }
        if (current.host_writes.contains(i)) { access |= HOST_WRITE }
        i -> access
      }).toMap
      env.foreach { case (i, a) => recordManagedAccess(managed, i, a) }
      (HostExecution(env, p2), Metadata.empty)
    }
  }

  private def analyzeAndInsertHostExecution(
    p: Phrase[CommType],
    allocs: Set[Identifier[_ <: PhraseType]],
    managed: mutable.Map[Identifier[_ <: PhraseType], AccessFlags]
  ): (Phrase[CommType], Metadata) = {
    val meta = Metadata(mutable.Set(), mutable.Set(), mutable.Set(), mutable.Set())
    val p2 = VisitAndRebuild(p, InsertHostExecutionsVisitor(allocs, managed, meta))
    (p2, meta)
  }

  private case class InsertHostExecutionsVisitor(
    allocs: Set[Identifier[_ <: PhraseType]],
    managed: mutable.Map[Identifier[_ <: PhraseType], AccessFlags],
    metadata: Metadata
  ) extends VisitAndRebuild.Visitor {
    override def phrase[T <: PhraseType](p: Phrase[T]): Result[Phrase[T]] =
      p match {
        case dpia.Assign(_, lhs, rhs) =>
          collectWrites(lhs, metadata.host_writes)
          collectReads(rhs, allocs, metadata.host_reads)
          Stop(p)
        case ocl.KernelCallCmd(_, _, _, out, in) =>
          in.foreach(collectReads(_, allocs, metadata.device_reads))
          collectWrites(out, metadata.device_writes)
          ((out, DEVICE_WRITE) +: in.map(_ -> DEVICE_READ)).foreach {
            case (i: Identifier[_], a) => recordManagedAccess(managed, i, a)
            case (Proj1(i: Identifier[_]), a) => recordManagedAccess(managed, i, a)
            case (Proj2(i: Identifier[_]), a) => recordManagedAccess(managed, i, a)
            case (Natural(_), _) =>
            case (unexpected, _) => throw new Exception(s"did not expect $unexpected")
          }
          Stop(p)
        case dpia.Seq(a, b) =>
          val (a2, am) = analyzeAndInsertHostExecution(a, allocs, managed)
          val (b2, bm) = insertHostExecutions(am, allocs, managed, b)
          metadata.host_writes ++= bm.host_writes
          metadata.host_reads ++= bm.host_reads
          metadata.device_writes ++= bm.device_writes
          metadata.device_reads ++= bm.device_reads
          Stop(dpia.Seq(a2, b2))
        case _ => Continue(p, this)
      }
  }

  def optionallyManaged(i: Identifier[_ <: PhraseType])
  : Option[(Identifier[_ <: PhraseType], DataType)]
  = i.`type` match {
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
    managed: mutable.Map[Identifier[_ <: PhraseType], AccessFlags]
  ): Phrase[CommType] => Phrase[CommType] = p => {
    val managed2 = managed.iterator.flatMap { case (i, a) =>
      optionallyManaged(i).map(om => i -> (a, om._1))
    }.toMap
    VisitAndRebuild(p, InsertManagedBuffersVisitor(managed2))
  }

  private case class InsertManagedBuffersVisitor(
    managed: Map[Identifier[_ <: PhraseType], (AccessFlags, Identifier[_ <: PhraseType])]
  ) extends VisitAndRebuild.Visitor {
    override def phrase[T <: PhraseType](p: Phrase[T]): Result[Phrase[T]] =
      p match {
        case i: Identifier[_] =>
          Stop(managed.get(i).map(_._2).getOrElse(p).asInstanceOf[Phrase[T]])
        case dpia.New(dt, Lambda(x, body)) if managed.contains(x) =>
          val access = managed(x)._1
          val x2 = managed(x)._2.asInstanceOf[Identifier[VarType]]
          Continue(ocl.NewManagedBuffer(dt, access, Lambda(x2, body)), this)
        case _: dpia.New | _: Lambda[_, _] | _: dpia.Seq |
             _: Proj2[_, _] | _: Proj1[_, _] | Natural(_) =>
          Continue(p, this)
        case _: ocl.KernelCallCmd => Continue(p, this)
        case _: HostExecution => Stop(p)
        case unexpected => throw new Exception(s"did not expect $unexpected")
      }
  }

  @tailrec
  private def collectWrites(a: Phrase[AccType],
                            writes: mutable.Set[Identifier[_ <: PhraseType]]): Unit = {
    import shine.DPIA.primitives.imperative._

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
      case ocl.IdxDistributeAcc(_, _, _, _, _, a) => collectWrites(a, writes)
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
      case ocl.IdxDistribute(_, _, _, _, _, e) => collectReads(e, allocs, reads)
      case dpia.MapRead(_, _, _, _, e) => collectReads(e, allocs, reads)
      case dpia.GenerateCont(_, _, _) => giveUp()
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
      case MakePair(_, _, _, e1, e2) =>
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
