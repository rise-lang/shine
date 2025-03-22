package shine.GAP8.Compilation

import arithexpr.arithmetic.ArithExpr.toInt
import arithexpr.arithmetic.NamedVar
import rise.core.types.{DataKind, DataType, NatIdentifier, NatKind, NatToNat, NatToNatLambda}
import rise.core.types.DataType._
import shine.DPIA.Compilation.FunDef
import shine.DPIA.Phrases._
import shine.DPIA.Types._
import shine.DPIA._

import scala.annotation.tailrec
import scala.collection.mutable
import scala.language.existentials

object SeparateHostAndAcceleratorCode {

  def separate(hostFunName: String): Phrase[_ <: PhraseType] =>
    (FunDef, Seq[FunDef]) = p => {
    val accFunctionDefinitions = mutable.ArrayBuffer[FunDef]()
    val hostDefinition = VisitAndRebuild(p, new VisitAndRebuild.Visitor {
      override def phrase[T <: PhraseType](p: Phrase[T]): Result[Phrase[T]] = p match {
        case r@shine.GAP8.primitives.functional.Run(cores) =>
          val name = "cluster_core_task"
          val (closedDef, args) = closeDefinition(r.input)
          val funDef = FunDef(name, closedDef)
          accFunctionDefinitions += funDef
          Stop(shine.GAP8.primitives.functional.KernelCall(name, toInt(cores), args.length)(
            funDef.paramTypes.map(_.dataType),
            funDef.returnType.dataType,
            args
          ).asInstanceOf[Phrase[T]])

        // on the fly beta-reduction
        case Apply(fun, arg) =>
          Stop(VisitAndRebuild(Lifting.liftFunction(fun).reducing(arg), this))
        case DepApply(_, fun, arg) => arg match {
          case a: Nat =>
            Stop(VisitAndRebuild(Lifting.liftDependentFunction(
              fun.asInstanceOf[Phrase[`(nat)->:`[ExpType]]])(a)
              .asInstanceOf[Phrase[T]], this))
          case a: DataType =>
            Stop(VisitAndRebuild(Lifting.liftDependentFunction(
              fun.asInstanceOf[Phrase[`(dt)->:`[ExpType]]])(a)
              .asInstanceOf[Phrase[T]], this))
        }

        case _ => Continue(p, this)
      }
    })
    (FunDef(hostFunName, hostDefinition), accFunctionDefinitions.toSeq)
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
          DepLambda(NatKind, NatIdentifier(v.name, v.range))(definition),
          Literal(NatAsIntData(v)) +: args, rest)
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
        case DepLambda(NatKind, x: NatIdentifier, _) =>
          Continue(p, this.copy(boundN = boundN + x))
        case DepLambda(DataKind, x: DataTypeIdentifier, _) =>
          Continue(p, this.copy(boundT = boundT + x))
        case _ => Continue(p, this)
      }

      override def natToNat(ft: NatToNat): NatToNat = ft match {
        case NatToNatLambda(x, b) =>
          NatToNatLambda(x, this.copy(boundN = boundN + x).nat(b))
        case _ => super.natToNat(ft)
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
