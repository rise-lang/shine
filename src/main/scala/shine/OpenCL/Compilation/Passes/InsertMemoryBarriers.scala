package shine.OpenCL.Compilation.Passes

import shine.DPIA.{->:, NatIdentifier}
import shine.DPIA.Phrases._
import shine.DPIA.Types._
import shine.DPIA.primitives.functional
import shine.DPIA.primitives.functional.{Map => _, _}
import shine.DPIA.primitives.imperative._
import shine.OpenCL
import shine.OpenCL.Local
import shine.OpenCL.primitives.{imperative => ocl}

import scala.annotation.tailrec
import scala.collection.mutable

object InsertMemoryBarriers {
  def insert: Phrase[CommType] => Phrase[CommType] = p =>
    analyzeAndInsertBarriers(p, Map())._1

  private case class Metadata( // reads from outer scope allocations
                               reads: mutable.Map[Identifier[_ <: PhraseType], AddressSpace],
                               // work-group parallel writes to outer scope allocations
                               wg_writes: mutable.Map[Identifier[_ <: PhraseType], AddressSpace])

  private def analyzeAndInsertBarriers(p: Phrase[CommType],
                                       // allocations in the current scope
                                       allocs: Map[Identifier[_ <: PhraseType], AddressSpace]
                                      ): (Phrase[CommType], Metadata) = {
    val meta = Metadata(mutable.Map(), mutable.Map())
    val p2 = VisitAndRebuild(p, Visitor(allocs, meta))
    (p2, meta)
  }

  private def visitLoopBody(p: Phrase[CommType],
                            allocs: Map[Identifier[_ <: PhraseType], AddressSpace],
                            metadata: Metadata,
                            outer_wg_writes: mutable.Map[Identifier[_ <: PhraseType], AddressSpace]
                              = mutable.Map()
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

  private def makeBarrier(allocs: Map[Identifier[_ <: PhraseType], AddressSpace]
                         ): Phrase[CommType] = {
    OpenCL.DSL.barrier(
      local = allocs.exists(_._2 == AddressSpace.Local),
      global = allocs.exists(_._2 == AddressSpace.Global))
  }

  private case class Visitor(allocs: Map[Identifier[_ <: PhraseType], AddressSpace],
                             metadata: Metadata) extends VisitAndRebuild.Visitor {
    override def phrase[T <: PhraseType](p: Phrase[T]): Result[Phrase[T]] = {
      p match {
        case f@For(unroll) =>
          f.loopBody match {
            case Lambda(x, body) =>
              Stop(For(unroll)(f.n, Lambda(x, visitLoopBody(body, allocs, metadata))))
            case _ => throw new Exception("This should not happen")
          }
        case f@ForNat(unroll) =>
          f.loopBody match {
            case DepLambda(x, body) =>
              Stop(ForNat(unroll)(f.n,
                DepLambda[NatKind, CommType](x, visitLoopBody(body, allocs, metadata))))
            case _ => throw new Exception("This should not happen")
          }
        case pf@ocl.ParFor(Local, dim, unroll, name) =>
          pf.body match {
            case Lambda(x, Lambda(o, body)) =>
              val outer_wg_writes = mutable.Map[Identifier[_ <: PhraseType], AddressSpace]()
              collectWrites(pf.out, allocs, outer_wg_writes)
              Stop(ocl.ParFor(Local, dim, unroll, name)(pf.init, pf.n, pf.step, pf.dt, pf.out,
                Lambda(x, Lambda(o,
                  visitLoopBody(body, allocs, metadata, outer_wg_writes)))))
            case _ => throw new Exception("This should not happen")
          }
        case pf@ocl.ParFor(level, dim, unroll, name) =>
          pf.body match {
            case Lambda(x, Lambda(o, body)) =>
              Stop(ocl.ParFor(level, dim, unroll, name)(pf.init, pf.n, pf.step, pf.dt, pf.out,
                Lambda(x, Lambda(o, visitLoopBody(body, allocs, metadata)))))
            case _ => throw new Exception("This should not happen")
          }
        case pf@ocl.ParForNat(Local, dim, unroll, name) =>
          pf.body match {
            case DepLambda(i: NatIdentifier, Lambda(o, p)) =>
              val outer_wg_writes = mutable.Map[Identifier[_ <: PhraseType], AddressSpace]()
              collectWrites(pf.out, allocs, outer_wg_writes)
              Stop(ocl.ParForNat(Local, dim, unroll, name)(pf.init, pf.n, pf.step, pf.ft, pf.out,
                DepLambda[NatKind, AccType ->: CommType](i, Lambda(o,
                  visitLoopBody(p, allocs, metadata, outer_wg_writes)))))
            case _ => throw new Exception("This should not happen")
          }
        case pf@ocl.ParForNat(level, dim, unroll, name) =>
          pf.body match {
            case DepLambda(i: NatIdentifier, Lambda(o, p)) =>
              Stop(ocl.ParForNat(level, dim, unroll, name)(pf.init, pf.n, pf.step, pf.ft, pf.out,
                DepLambda[NatKind, AccType ->: CommType](i, Lambda(o,
                  visitLoopBody(p, allocs, metadata)))))
            case _ => throw new Exception("This should not happen")
          }
        case ocl.New(addr, _, Lambda(x, _)) if addr != AddressSpace.Private =>
          Continue(p, Visitor(allocs + (x -> addr), metadata))
        case ocl.NewDoubleBuffer(addr, dt1, dt2, dt3, n, in, out, Lambda(x, body))
        if addr != AddressSpace.Private =>
          val (b2, m) = analyzeAndInsertBarriers(body, allocs + (x -> addr))
          collectReads(in, allocs, metadata.reads)
          metadata.reads ++= m.reads
          metadata.reads ++= m.reads
          Stop(ocl.NewDoubleBuffer(addr, dt1, dt2, dt3, n, in, out, Lambda(x, b2)))
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
            val barrier = makeBarrier(dependencies.map { i => i -> allocs(i) }.toMap)
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
      case idx: ocl.IdxDistributeAcc => collectWrites(idx.array, allocs, writes)
      case PairAcc1(_, _, a) => collectWrites(a, allocs, writes)
      case PairAcc2(_, _, a) => collectWrites(a, allocs, writes)
      case PairAcc(_, _, a, b) =>
        collectWrites(a, allocs, writes); collectWrites(b, allocs, writes)
      case TakeAcc(_, _, _, a) => collectWrites(a, allocs, writes)
      case TransposeAcc(_, _, _, a) => collectWrites(a, allocs, writes)
      case _ => throw new Exception(s"did not expect $a")
    }
  }

  private def collectReads(e: Phrase[ExpType],
                           allocs: Map[Identifier[_ <: PhraseType], AddressSpace],
                           reads: mutable.Map[Identifier[_ <: PhraseType], AddressSpace]
                          ): Unit = {
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
      case Idx(_, _, e1, e2) =>
        collectReads(e1, allocs, reads); collectReads(e2, allocs, reads)
      case Slide(_, _, _, _, e) => collectReads(e, allocs, reads)
      case functional.Map(_, _, _, _, _, e) => collectReads(e, allocs, reads)
      case idx: ocl.IdxDistribute => collectReads(idx.array, allocs, reads)
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
