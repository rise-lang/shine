package shine.OpenCL.Compilation

import rise.core.types.DataType
import rise.core.types.DataType._
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
    val managed = mutable.Map[Identifier[_ <: PhraseType], AccessFlags]()
    val (p2, info) = analyzeAndInsertHostExecution(p, outsideParams().toSet, managed)
    val host_plain_access = info.host_plain_reads.nonEmpty || info.host_plain_writes.nonEmpty
    val p3 = if (host_plain_access) { makeHostExecution(p2, info, managed) } else { p2 }
    insertManagedBuffers(managed)(p3)
  }

  private class Metadata(
    // is this phrase reading or writing to plain arrays on the host side?
    val host_plain_reads: mutable.Set[Identifier[_ <: PhraseType]],
    val host_plain_writes: mutable.Set[Identifier[_ <: PhraseType]],
    // is this phrase using the device?
    var device_used: Boolean)

  private object ReadsAndWrites {
    def empty: Metadata = new Metadata(mutable.Set(), mutable.Set(), false)
  }

  private def recordManagedAccess(
    managed: mutable.Map[Identifier[_ <: PhraseType], AccessFlags],
    ident: Identifier[_ <: PhraseType],
    access: AccessFlags
  ): Unit = {
    managed.updateWith(ident)(prev => Some(prev.getOrElse(0) | access))
  }

  private def makeHostExecution(
    p: Phrase[CommType],
    info: Metadata,
    managed: mutable.Map[Identifier[_ <: PhraseType], AccessFlags],
  ): Phrase[CommType] = {
    // host execution sections must not use the device:
    assert(!info.device_used)

    val env = (info.host_plain_reads ++ info.host_plain_writes)
      .filter(optionallyManaged(_).isDefined).map(i => {
      var access = 0
      if (info.host_plain_reads.contains(i)) { access |= HOST_READ }
      if (info.host_plain_writes.contains(i)) { access |= HOST_WRITE }
      i -> access
    }).toMap
    env.foreach { case (i, a) => recordManagedAccess(managed, i, a) }
    HostExecution(env)(p)
  }

  private def analyzeAndInsertHostExecution(
    p: Phrase[CommType],
    allocs: Set[Identifier[_ <: PhraseType]],
    managed: mutable.Map[Identifier[_ <: PhraseType], AccessFlags]
  ): (Phrase[CommType], Metadata) = {
    val meta = ReadsAndWrites.empty
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
          collectWrites(lhs, metadata.host_plain_writes)
          collectReads(rhs, allocs, metadata.host_plain_reads)
          Stop(p)
        case k@ocl.KernelCallCmd(_, _, _, _) =>
          metadata.device_used = true
          ((k.output, DEVICE_WRITE) +: k.args.map(_ -> DEVICE_READ)).foreach {
            case (i: Identifier[_], a) => recordManagedAccess(managed, i, a)
            case (Proj1(i: Identifier[_]), a) => recordManagedAccess(managed, i, a)
            case (Proj2(i: Identifier[_]), a) => recordManagedAccess(managed, i, a)
            case (Natural(_), _) =>
            case (Literal(NatAsIntData(_)), _) =>
            case (unexpected, _) => throw new Exception(s"did not expect $unexpected")
          }
          Stop(p)
        case k@shine.GAP8.primitives.imperative.KernelCallCmd(name, cores, n) =>
          metadata.device_used = true
          ((k.output, DEVICE_WRITE) +: k.args.map(_ -> DEVICE_READ)).foreach {
            case (i: Identifier[_], a) => recordManagedAccess(managed, i, a)
            case (Proj1(i: Identifier[_]), a) => recordManagedAccess(managed, i, a)
            case (Proj2(i: Identifier[_]), a) => recordManagedAccess(managed, i, a)
            case (Natural(_), _) =>
            case (Literal(NatAsIntData(_)), _) =>
            case (unexpected, _) => throw new Exception(s"did not expect $unexpected")
          }
          Stop(p)
        case dpia.Seq(a, b) =>
          val (a2, am) = analyzeAndInsertHostExecution(a, allocs, managed)
          val (b2, bm) = analyzeAndInsertHostExecution(b, allocs, managed)
          val a_host_plain_access = am.host_plain_reads.nonEmpty || am.host_plain_writes.nonEmpty
          val b_host_plain_access = bm.host_plain_reads.nonEmpty || bm.host_plain_writes.nonEmpty
          if (!am.device_used && !bm.device_used) {
            metadata.host_plain_reads ++= am.host_plain_reads
            metadata.host_plain_reads ++= bm.host_plain_reads
            metadata.host_plain_writes ++= am.host_plain_writes
            metadata.host_plain_writes ++= bm.host_plain_writes
            Stop(dpia.Seq(a2, b2))
          } else { // am.device_used || bm.device_used
            metadata.device_used = true
            val a3 = if (a_host_plain_access) { makeHostExecution(a2, am, managed) } else { a2 }
            val b3 = if (b_host_plain_access) { makeHostExecution(b2, bm, managed) } else { b2 }
            Stop(dpia.Seq(a3, b3))
          }
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
    val managed2: Map[Identifier[_ <: PhraseType], (AccessFlags, Identifier[_ <: PhraseType])] =
      managed.iterator.flatMap { case (i, a) =>
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
          Continue(ocl.NewManagedBuffer(access)(dt, Lambda(x2, body)), this)
        case _: dpia.New | _: Lambda[_, _] | _: dpia.Seq |
             _: Proj2[_, _] | _: Proj1[_, _] | Natural(_) | Literal(NatAsIntData(_)) =>
          Continue(p, this)
        case k@ocl.KernelCallCmd(name, ls, gs, n) =>
          val newOutput = VisitAndRebuild(k.output, this)
          val newArgs = k.args.map(VisitAndRebuild(_, this))
          Stop(ocl.KernelCallCmd(name, ls, gs, n)(
            newArgs.map(_.t.dataType), newOutput.t.dataType, k.args.map(VisitAndRebuild(_, this)), newOutput))

        case k@shine.GAP8.primitives.imperative.KernelCallCmd(name, cores, n) =>
          val newOutput = VisitAndRebuild(k.output, this)
          val newArgs = k.args.map(VisitAndRebuild(_, this))
          Stop(shine.GAP8.primitives.imperative.KernelCallCmd(name, cores, n)(
            newArgs.map(_.t.dataType), newOutput.t.dataType, k.args.map(VisitAndRebuild(_, this)), newOutput
          ))
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
      case idx:ocl.IdxDistributeAcc => collectWrites(idx.array, writes)
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
      case idx: ocl.IdxDistribute => collectReads(idx.array, allocs, reads)
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
      case PadCst(_, _, _, _, e1, e2) =>
        collectReads(e1, allocs, reads); collectReads(e2, allocs, reads)
      case PadClamp(_, _, _, _, e) =>
        collectReads(e, allocs, reads)
      case Cast(_, _, e) => collectReads(e, allocs, reads)
      case ffc@ForeignFunctionCall(_, _) =>
        ffc.args.foreach {
          collectReads(_, allocs, reads)
        }
      case NatAsIndex(_, e) => collectReads(e, allocs, reads)
      case Drop(_, _, _, e) => collectReads(e, allocs, reads)
      case Take(_, _, _, e) => collectReads(e, allocs, reads)
      case Unzip(_, _, _, _, e) => collectReads(e, allocs, reads)
      case MakePair(_, _, _, e1, e2) =>
        collectReads(e1, allocs, reads); collectReads(e2, allocs, reads)
      case Reorder(_, _, _, _, _, e) => collectReads(e, allocs, reads)
      case m@MakeArray(_) =>
        m.elements.foreach {
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
