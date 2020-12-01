package shine.OpenCL.CodeGeneration

import shine.DPIA.ImperativePrimitives._
import shine.DPIA.Phrases.{VisitAndRebuild, _}
import shine.DPIA.Types._
import shine.DPIA._
import shine.OpenCL.ImperativePrimitives._
import shine._

import scala.collection.mutable

object InsertMemoryBarriers {
  def apply(p: Phrase[CommType]): Phrase[CommType] = {
    val (p2, _) = analyzeAndInsertBarriers(p, Map())
    p2
  }

  private case class Metadata(// reads from outer scope allocations
                              reads: mutable.Map[Identifier[_ <: PhraseType], AddressSpace],
                              // work-group parallel writes to outer scope allocations
                              wg_writes: mutable.Map[Identifier[_ <: PhraseType], AddressSpace])

  private def analyzeAndInsertBarriers(
    p: Phrase[CommType],
    // allocations in the current scope
    allocs: Map[Identifier[_ <: PhraseType], AddressSpace]
  ): (Phrase[CommType], Metadata) = {
    val meta = Metadata(mutable.Map(), mutable.Map())
    val p2 = VisitAndRebuild(p, Visitor(allocs, meta))
    (p2, meta)
  }

  private def visitLoopBody(
    p: Phrase[CommType],
    allocs: Map[Identifier[_ <: PhraseType], AddressSpace],
    metadata: Metadata,
    outer_wg_writes: mutable.Map[Identifier[_ <: PhraseType], AddressSpace] = mutable.Map()
  ): Phrase[CommType] = {
    val p2 = VisitAndRebuild(p, Visitor(allocs, metadata))
    val dependencies = metadata.wg_writes.filter(kv => metadata.reads.contains(kv._1))
    if (dependencies.nonEmpty) {
      metadata.reads.clear()
      metadata.wg_writes.clear()
      Seq(p2, makeBarrier(dependencies.toMap))
    } else {
      metadata.wg_writes ++= outer_wg_writes
      p2
    }
  }

  private def makeBarrier(
    allocs: Map[Identifier[_ <: PhraseType], AddressSpace]
  ): Phrase[CommType] = {
    OpenCL.DSL.barrier(
      local = allocs.exists(_._2 == AddressSpace.Local),
      global = allocs.exists(_._2 == AddressSpace.Global))
  }

  private case class Visitor(allocs: Map[Identifier[_ <: PhraseType], AddressSpace],
                             metadata: Metadata) extends VisitAndRebuild.Visitor {
    override def phrase[T <: PhraseType](p: Phrase[T]): Result[Phrase[T]] = {
      p match {
        case For(n, Lambda(x, body), unroll) =>
          Stop(For(n, Lambda(x, visitLoopBody(body, allocs, metadata)), unroll))
        case ForNat(n, DepLambda(x, body), unroll) =>
          Stop(ForNat(n, DepLambda(x, visitLoopBody(body, allocs, metadata)), unroll))
        case pf @ OpenCLParFor(n, dt, out, Lambda(x, Lambda(o, body)), init, step, unroll) =>
          pf match {
            case ParForLocal(dim) =>
              val outer_wg_writes = mutable.Map[Identifier[_ <: PhraseType], AddressSpace]()
              collectWrites(out, allocs, outer_wg_writes)
              val b2 = visitLoopBody(body, allocs, metadata, outer_wg_writes)
              Stop(ParForLocal(dim)(n, dt, out, Lambda(x, Lambda(o, b2)), init, step, unroll))
            case ParForWorkGroup(dim) =>
              Stop(ParForWorkGroup(dim)(n, dt, out,
                Lambda(x, Lambda(o, visitLoopBody(body, allocs, metadata))), init, step, unroll))
            case ParForGlobal(dim) =>
              Stop(ParForGlobal(dim)(n, dt, out,
                Lambda(x, Lambda(o, visitLoopBody(body, allocs, metadata))), init, step, unroll))
          }
        case pfn: OpenCLParForNat => ???
        case OpenCLNew(addr, _, Lambda(x, _)) if addr != AddressSpace.Private =>
          Continue(p, Visitor(allocs + (x -> addr), metadata))
        case OpenCLNewDoubleBuffer(addr, dt1, dt2, dt3, n, in, out, Lambda(x, body))
        if addr != AddressSpace.Private =>
          val (b2, m) = analyzeAndInsertBarriers(body, allocs + (x -> addr))
          collectReads(in, allocs, metadata.reads)
          metadata.reads ++= m.reads
          metadata.reads ++= m.reads
          Stop(OpenCLNewDoubleBuffer(addr, dt1, dt2, dt3, n, in, out, Lambda(x, b2)))
        case Assign(_, _, rhs) =>
          collectReads(rhs, allocs, metadata.reads)
          Stop(p)
        case Seq(a, b) =>
          val (a2, am) = analyzeAndInsertBarriers(a, allocs)
          val (b2, bm) = analyzeAndInsertBarriers(b, allocs)
          val dependencies = am.reads.keySet.intersect(bm.wg_writes.keySet)
            .union(bm.reads.keySet.intersect(am.wg_writes.keySet))
          metadata.reads ++= am.reads
          metadata.reads ++= bm.reads
          metadata.wg_writes ++= am.wg_writes
          metadata.wg_writes ++= bm.wg_writes
          if (dependencies.nonEmpty) {
            val barrier = makeBarrier(dependencies.map{ i => i -> allocs(i) }.toMap)
            Stop(Seq(a2, Seq(barrier, b2)))
          } else {
            Stop(Seq(a2, b2))
          }
        case _ => Continue(p, this)
      }
    }
  }

  private def collectWrites(
    a: Phrase[AccType],
    allocs: Map[Identifier[_ <: PhraseType], AddressSpace],
    writes: mutable.Map[Identifier[_ <: PhraseType], AddressSpace]
  ): Unit = {
    def addIdent(i: Identifier[_ <: PhraseType]): Unit = if (allocs.contains(i)) {
      writes(i) = allocs(i)
    }

    a match {
      case i: Identifier[_] => addIdent(i)
      case Proj1(p) => projCollectIdent(p, addIdent)
      case Proj2(p) => projCollectIdent(p, addIdent)
      // TODO: collect reads in index?
      case IdxAcc(_, _, _, a) => collectWrites(a, allocs, writes)
      case MapAcc(_, _, _, _, a) => collectWrites(a, allocs, writes)
      case JoinAcc(_, _, _, a) => collectWrites(a, allocs, writes)
      case SplitAcc(_, _, _, a) => collectWrites(a, allocs, writes)
      case AsScalarAcc(_, _, _, a) => collectWrites(a, allocs, writes)
      case IdxDistributeAcc(_, _, _, _, _, a) => collectWrites(a, allocs, writes)
      case PairAcc1(_, _, a) => collectWrites(a, allocs, writes)
      case PairAcc2(_, _, a) => collectWrites(a, allocs, writes)
      case TakeAcc(_, _, _, a) => collectWrites(a, allocs, writes)
      case TransposeAcc(_, _, _, a) => collectWrites(a, allocs, writes)
      case _ => throw new Exception(s"did not expect $a")
    }
  }

  private def collectReads(e: Phrase[ExpType],
                           allocs: Map[Identifier[_ <: PhraseType], AddressSpace],
                           reads: mutable.Map[Identifier[_ <: PhraseType], AddressSpace]): Unit = {
    def addIdent(i: Identifier[_ <: PhraseType]): Unit = if (allocs.contains(i)) {
      reads(i) = allocs(i)
    }

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
      case FunctionalPrimitives.Idx(_, _, e1, e2) =>
        collectReads(e1, allocs, reads); collectReads(e2, allocs, reads)
      case FunctionalPrimitives.Slide(_, _, _, _, e) => collectReads(e, allocs, reads)
      case FunctionalPrimitives.Map(_, _, _, _, _, e) => collectReads(e, allocs, reads)
      case IdxDistribute(_, _, _, _, _, e) => collectReads(e, allocs, reads)
      case MapRead(_, _, _, _, e) => collectReads(e, allocs, reads)
      case GenerateCont(_, _, _) => giveUp()
      case FunctionalPrimitives.AsScalar(_, _, _, _, e) => collectReads(e, allocs, reads)
      case FunctionalPrimitives.AsVectorAligned(_, _, _, _, e) => collectReads(e, allocs, reads)
      case FunctionalPrimitives.AsVector(_, _, _, _, e) => collectReads(e, allocs, reads)
      case FunctionalPrimitives.VectorFromScalar(_, _, e) => collectReads(e, allocs, reads)
      case FunctionalPrimitives.Fst(_, _, e) => collectReads(e, allocs, reads)
      case FunctionalPrimitives.Snd(_, _, e) => collectReads(e, allocs, reads)
      case FunctionalPrimitives.Transpose(_,_, _, _, e) => collectReads(e, allocs, reads)
      case FunctionalPrimitives.Join(_, _, _, _, e) => collectReads(e, allocs, reads)
      case FunctionalPrimitives.Split(_, _, _, _, e) => collectReads(e, allocs, reads)
      case FunctionalPrimitives.Zip(_, _, _, _, e1, e2) =>
        collectReads(e1, allocs, reads); collectReads(e2, allocs, reads)
      case FunctionalPrimitives.Pad(_, _, _, _, e1, e2) =>
        collectReads(e1, allocs, reads); collectReads(e2, allocs, reads)
      case FunctionalPrimitives.PadClamp(_, _, _, _, e) =>
        collectReads(e, allocs, reads)
      case FunctionalPrimitives.Cast(_, _, e) => collectReads(e, allocs, reads)
      case FunctionalPrimitives.ForeignFunction(_, _, _, es) =>
        es.foreach { collectReads(_, allocs, reads) }
      case FunctionalPrimitives.NatAsIndex(_, e) => collectReads(e, allocs, reads)
      case FunctionalPrimitives.Drop(_, _, _, e) => collectReads(e, allocs, reads)
      case FunctionalPrimitives.Take(_, _, _, e) => collectReads(e, allocs, reads)
      case FunctionalPrimitives.Unzip(_, _, _, _, e) => collectReads(e, allocs, reads)
      case FunctionalPrimitives.Pair(_, _, _, e1, e2) =>
        collectReads(e1, allocs, reads); collectReads(e2, allocs, reads)
      case FunctionalPrimitives.Reorder(_, _, _, _, _, e) => collectReads(e, allocs, reads)
      case FunctionalPrimitives.MakeArray(_, es) =>
        es.foreach { collectReads(_, allocs, reads) }
      case FunctionalPrimitives.Gather(_, _, _, e1, e2) =>
        collectReads(e1, allocs, reads); collectReads(e2, allocs, reads)
      case _ => throw new Exception(s"did not expect $e")
    }
  }

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
