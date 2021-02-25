package shine.OpenCL.Compilation

import arithexpr.arithmetic.NamedVar
import shine.DPIA.Compilation.FunDef
import shine.DPIA.Phrases._
import shine.DPIA.Types._
import shine.DPIA._
import shine.OpenCL.primitives.functional._

import scala.language.existentials
import scala.annotation.tailrec
import scala.collection.mutable

object SeparateHostAndKernelCode {

  def separate(hostFunName: String): Phrase[_ <: PhraseType] =>
    (FunDef, Seq[KernelDef]) = p => {
    var kernelNum = 0
    var kernelDefinitions = mutable.ArrayBuffer[KernelDef]()
    val hostDefinition = VisitAndRebuild(p, new VisitAndRebuild.Visitor {
      override def phrase[T <: PhraseType](p: Phrase[T]): Result[Phrase[T]] = p match {
        case Run(localSize, globalSize, _, value) =>
          val name = s"k$kernelNum"
          kernelNum += 1
          val (closedDef, args) = closeDefinition(value)
          val kernelDef = KernelDef(name, closedDef, localSize, globalSize)
          kernelDefinitions += kernelDef
          Stop(KernelCall(name, localSize, globalSize,
            kernelDef.paramTypes.map(_.dataType),
            kernelDef.returnType.dataType,
            args).asInstanceOf[Phrase[T]])

        // on the fly beta-reduction
        case Apply(fun, arg) =>
          Stop(VisitAndRebuild(Lifting.liftFunction(fun).reducing(arg), this))
        case DepApply(fun, arg) => arg match {
          case a: Nat =>
            Stop(VisitAndRebuild(Lifting.liftDependentFunction[NatKind, ExpType](
              fun.asInstanceOf[Phrase[NatKind `()->:` ExpType]])(a)
              .asInstanceOf[Phrase[T]], this))
          case a: DataType =>
            Stop(VisitAndRebuild(Lifting.liftDependentFunction[DataKind, ExpType](
              fun.asInstanceOf[Phrase[DataKind `()->:` ExpType]])(a)
              .asInstanceOf[Phrase[T]], this))
        }

        case _ => Continue(p, this)
      }
    })
    (FunDef(hostFunName, hostDefinition), kernelDefinitions.toSeq)
  }

  private def closeDefinition(definition: Phrase[_ <: PhraseType]
                             ): (Phrase[_ <: PhraseType], Seq[Phrase[ExpType]]) = {
    @tailrec
    def iterNats(definition: Phrase[_ <: PhraseType],
                 args: Seq[Phrase[ExpType]],
                 freeNats: Seq[NamedVar]
                ): (Phrase[_ <: PhraseType], Seq[Phrase[ExpType]]) = {
      freeNats match {
        case v +: rest => iterNats(
          DepLambda[NatKind](NatIdentifier(v.name, v.range))(definition),
          Natural(v) +: args, rest)
        case Nil => (definition, args)
      }
    }

    @tailrec
    def iterVars(definition: Phrase[_ <: PhraseType],
                 args: Seq[Phrase[ExpType]],
                 freeVariables: Seq[Identifier[ExpType]]
                ): (Phrase[_ <: PhraseType], Seq[Phrase[ExpType]]) = {
      freeVariables match {
        case v +: rest => v match {
          case i: Identifier[ExpType] =>
            iterVars(Lambda(i, definition), v +: args, rest)
          case i => throw new Exception(s"${i.getClass} is not supported")
        }
        case Nil => (definition, args)
      }
    }

    val (vars, nats) = freeVariables(definition)
    val (d1, a1) = iterVars(definition, Nil, vars.toSeq)
    iterNats(d1, a1, nats.toSeq)
  }

  // TODO: collect free nat identifiers?
  private def freeVariables(p: Phrase[_ <: PhraseType])
  : (Set[Identifier[ExpType]], Set[NamedVar]) = {
    var idents = scala.collection.mutable.Set[Identifier[ExpType]]()
    var natIdents = scala.collection.mutable.Set[NamedVar]()

    case class Visitor(boundV: Set[Identifier[_]],
                       boundT: Set[DataTypeIdentifier],
                       boundN: Set[NamedVar]
                      ) extends VisitAndRebuild.Visitor {
      override def phrase[T <: PhraseType](p: Phrase[T]): Result[Phrase[T]] = p match {
        case i: Identifier[_] if !boundV(i) =>
          idents += i.asInstanceOf[Identifier[ExpType]]
          Stop(p)
        case Lambda(x, _) =>
          Continue(p, this.copy(boundV = boundV + x))
        case DepLambda(x: NatIdentifier, _) =>
          Continue(p, this.copy(boundN = boundN + x))
        case DepLambda(x: DataTypeIdentifier, _) =>
          Continue(p, this.copy(boundT = boundT + x))
        case _ => Continue(p, this)
      }

      override def nat[N <: Nat](n: N): N = {
        natIdents ++= n.varList.collect {
          case v: NamedVar if !boundN(v) => v
        }
        n
      }

      // TODO: other cases are missing
      // TODO: check that there are no free data types
    }

    VisitAndRebuild(p, Visitor(Set(), Set(), Set()))
    (idents.toSet, natIdents.toSet)
  }
}
