package idealised.OpenCL

import idealised.DPIA._
import idealised.DPIA.Phrases._
import idealised.DPIA.Types._
import idealised.DPIA.ImperativePrimitives.{For, ForNat, IdxAcc}
import idealised.DPIA.FunctionalPrimitives.Idx
import idealised.DPIA.Semantics.OperationalSemantics.ArrayData
import idealised.OpenCL.ImperativePrimitives._

import scala.collection.mutable

object FlagPrivateArrayLoops {
  def apply(p: Phrase[CommType]): Phrase[CommType] = {
    val vs = varsToEliminate(p)
    val p2 = eliminateLoopVars(p, vs)
    if (vs.nonEmpty) {
      println(s"To eliminate: $vs")
      throw new Exception(s"could not eliminate variables $vs")
    }
    p2
  }

  private def varsToEliminate(p: Phrase[CommType]): mutable.Set[String] = {
    var eliminateVars = mutable.Set[String]()

    case class Visitor(privMemIdent: Set[Identifier[_]],
                       indexingIdent: Set[String])
      extends VisitAndRebuild.Visitor
    {
      override def phrase[T <: PhraseType](p: Phrase[T]): Result[Phrase[T]] = p match {
        case OpenCLNew(AddressSpace.Private, _, Lambda(i: Identifier[_], _)) =>
          Continue(p, this.copy(privMemIdent = privMemIdent + i))
        case Idx(_, _, i, _) =>
          Continue(p, this.copy(indexingIdent = indexingIdent ++ collectIndexingIdents(i)))
        case IdxAcc(_, _, i, _) =>
          Continue(p, this.copy(indexingIdent = indexingIdent ++ collectIndexingIdents(i)))
        case i: Identifier[_] if privMemIdent(i) =>
          eliminateVars ++= indexingIdent
          Stop(p)
        case Literal(ArrayData(_)) =>
          eliminateVars ++= indexingIdent
          Stop(p)
        //TODO Dangerous. collectIndexingIdents can find private vars that are not the acceptor (e.g. in A@i if i is private var). Instead translate ParFor to For first?
        case OpenCLParFor(_, _, out, Lambda(i: Identifier[_], Lambda(o: Identifier[_], body)), _, _, _) if collectAccIdent(out).exists(privMemIdent(_)) =>
          eliminateVars += i.name
          Continue(p, this.copy(privMemIdent = privMemIdent + o))
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
        case For(n, body @ Lambda(i: Identifier[_], _), _) if eliminateVars(i.name) =>
          eliminateVars -= i.name
          Continue(For(n, body, unroll = true), this)
        case ForNat(n, body @ DepLambda(i: NatIdentifier, _), _) if eliminateVars(i.name) =>
          eliminateVars -= i.name
          Continue(ForNat(n, body, unroll = true), this)
        case pf@OpenCLParFor(n, dt, out, body @ Lambda(i: Identifier[_], _), init, step, _)
          if eliminateVars(i.name) =>
            eliminateVars -= i.name
            pf match {
              case ParForGlobal(dim) =>
                Continue(ParForGlobal(dim)(n, dt, out, body, init, step, unroll = true), this)
              case ParForLocal(dim) =>
                Continue(ParForLocal(dim)(n, dt, out, body, init, step, unroll = true), this)
              case ParForWorkGroup(dim) =>
                Continue(ParForWorkGroup(dim)(n, dt, out, body, init, step, unroll = true), this)
            }
        case pf@OpenCLParForNat(n, dt, out, body @ DepLambda(i: NatIdentifier, _), init, step, _)
          if eliminateVars(i.name) =>
            eliminateVars -= i.name
            pf match {
              case ParForNatGlobal(dim) =>
                Continue(ParForNatGlobal(dim)(n, dt, out, body, init, step, unroll = true), this)
              case ParForNatLocal(dim) =>
                Continue(ParForNatLocal(dim)(n, dt, out, body, init, step, unroll = true), this)
              case ParForNatWorkGroup(dim) =>
                Continue(ParForNatWorkGroup(dim)(n, dt, out, body, init, step, unroll = true), this)
        }
        case _ =>
          Continue(p, this)
      }
    })
  }

  private def collectIndexingIdents[T <: PhraseType](p: Phrase[T]): Set[String] = {
    var vars = mutable.Set[String]()

    VisitAndRebuild(p, new VisitAndRebuild.Visitor {
      override def nat[N <: Nat](n: N): N = {
        vars ++= n.varList.map(_.name)
        n
      }

      override def phrase[T2 <: PhraseType](p: Phrase[T2]): Result[Phrase[T2]] = {
        p match {
          case i: Identifier[_] => vars += i.name
          case _ =>
        }
        Continue(p, this)
      }
    })

    vars.toSet
  }

  private def collectAccIdent[T <: PhraseType](p: Phrase[T]): Set[Identifier[_]] = {
    var vars = mutable.Set[Identifier[_]]()

    VisitAndRebuild(p, new VisitAndRebuild.Visitor {
      override def phrase[T2 <: PhraseType](p: Phrase[T2]): Result[Phrase[T2]] = {
        p match {
          case i: Identifier[_]  => i.`type` match {
            case _: AccType => vars += i
            case _ =>
          }
          case _ =>
        }
        Continue(p, this)
      }
    })

    vars.toSet
  }
}
