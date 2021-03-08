package shine.OpenCL.Compilation.Passes

import shine.DPIA.Nat
import shine.DPIA.Phrases._
import shine.DPIA.Semantics.OperationalSemantics.ArrayData
import shine.DPIA.Types.{CommType, PhraseType}
import shine.DPIA.primitives.functional.{Idx, NatAsIndex}
import shine.DPIA.primitives.imperative.{For, ForNat, IdxAcc}
import shine.OpenCL.AddressSpace
import shine.OpenCL.primitives.imperative.{New, ParFor, ParForNat}

import scala.collection.mutable

object FlagPrivateArrayLoops {

  def flag: Phrase[CommType] => Phrase[CommType] = p => {
    val vs = varsToEliminate(p)
    val p2 = eliminateLoopVars(p, vs)
    if (vs.nonEmpty) {
      println(s"WARNING: could not eliminate variables $vs")
    }
    p2
  }

  private def varsToEliminate(p: Phrase[CommType]): mutable.Set[String] = {
    val eliminateVars = mutable.Set[String]()

    case class Visitor(privMemIdents: Set[Identifier[_]],
                       indexingIdents: Set[String])
      extends VisitAndRebuild.Visitor {
      override def phrase[T <: PhraseType](p: Phrase[T]): Result[Phrase[T]] = p match {
        // FIXME: we would like this primitive to be eliminated before this pass?
        case shine.DPIA.primitives.imperative.MapRead(_, _, _, f, in) =>
          Stop(p)
        case New(AddressSpace.Private, _, Lambda(i: Identifier[_], _)) =>
          Continue(p, this.copy(privMemIdents = privMemIdents + i))
        case Idx(_, _, i, _) =>
          Continue(p, this.copy(indexingIdents = indexingIdents ++ collectIndexingIdents(i)))
        case IdxAcc(_, _, i, _) =>
          Continue(p, this.copy(indexingIdents = indexingIdents ++ collectIndexingIdents(i)))
        case i: Identifier[_] if privMemIdents(i) =>
          eliminateVars ++= indexingIdents
          Stop(p)
        case Literal(ArrayData(_)) =>
          eliminateVars ++= indexingIdents
          Stop(p)
        case pf: ParFor if collectIdents(pf.out).exists(privMemIdents(_)) =>
          val (i, o, _) = pf.unwrapBody
          eliminateVars += i.name
          Continue(p, this.copy(privMemIdents = privMemIdents + o))
        case _ =>
          Continue(p, this)
      }
    }

    VisitAndRebuild(p, Visitor(Set(), Set()))
    eliminateVars
  }

  private def eliminateLoopVars(p: Phrase[CommType],
                                eliminateVars: mutable.Set[String]): Phrase[CommType] = {
    VisitAndRebuild(p, new VisitAndRebuild.Visitor {
      override def phrase[T <: PhraseType](p: Phrase[T]): Result[Phrase[T]] = p match {
        case f@For(_) if (eliminateVars(f.unwrapBody._1.name)) =>
          val (i, _) = f.unwrapBody
          eliminateVars -= i.name
          Continue(For(unroll = true)(f.n, f.loopBody), this)
        case f@ForNat(_) if (eliminateVars(f.unwrapBody._1.name)) =>
          val (i, _) = f.unwrapBody
          eliminateVars -= i.name
          Continue(ForNat(unroll = true)(f.n, f.loopBody), this)
        case pf@ParFor(level, dim, _) if (eliminateVars(pf.unwrapBody._1.name)) =>
          val (i, _, _) = pf.unwrapBody
          eliminateVars -= i.name
          Continue(ParFor(level, dim, unroll = true)
            (pf.n, pf.dt, pf.out, pf.loopBody, pf.init, pf.step), this)
        case pf@ParForNat(level, dim, _) if (eliminateVars(pf.unwrapBody._1.name)) =>
          val (i, _, _) = pf.unwrapBody
          eliminateVars -= i.name
          Continue(ParForNat(level, dim, unroll = true)
            (pf.n, pf.ft, pf.out, pf.loopBody, pf.init, pf.step), this)
        case _ =>
          Continue(p, this)
      }
    })
  }

  private def collectIndexingIdents[T <: PhraseType](p: Phrase[T]): Set[String] = {
    val idents = mutable.Set[String]()

    VisitAndRebuild(p, new VisitAndRebuild.Visitor {
      override def nat[N <: Nat](n: N): N = {
        idents ++= n.varList.map(_.name)
        n
      }

      override def phrase[T2 <: PhraseType](p: Phrase[T2]): Result[Phrase[T2]] = {
        p match {
          case i: Identifier[_] =>
            idents += i.name
            return Stop(p) // do not visit the type
          case NatAsIndex(_, p) =>
            return Continue(p, this)
          case _ =>
        }
        Continue(p, this)
      }
    })

    idents.toSet
  }

  private def collectIdents[T <: PhraseType](p: Phrase[T]): Set[Identifier[_]] = {
    val idents = mutable.Set[Identifier[_]]()

    VisitAndRebuild(p, new VisitAndRebuild.Visitor {
      override def phrase[T2 <: PhraseType](p: Phrase[T2]): Result[Phrase[T2]] = {
        p match {
          case i: Identifier[_] => idents += i
          case _ =>
        }
        Continue(p, this)
      }
    })

    idents.toSet
  }
}
