package idealised.OpenCL.FunctionalPrimitives

import idealised.Compiling.RewriteToImperative
import idealised.Core
import idealised.Core.OperationalSemantics.{Data, Store}
import idealised.Core.VisitAndRebuild.Visitor
import idealised.Core._
import idealised.DSL.typed._
import idealised.OpenCL.CodeGenerator
import idealised.OpenCL.CodeGenerator.Environment
import idealised.OpenCL.Core.GeneratableExp
import opencl.generator.OpenCLAST.{Expression, FunctionCall}

import scala.language.reflectiveCalls
import scala.xml.Elem


final case class OpenCLFunction(name: String,
                                inTs: Seq[DataType],
                                outT: DataType,
                                args: Seq[Phrase[ExpType]])
  extends ExpPrimitive with GeneratableExp {

  override lazy val `type` = exp"[$outT]"

  override def typeCheck(): Unit = {
    import TypeChecker._
    (inTs zip args).foreach{
      case (inT, arg) => arg :: exp"[$inT]"
    }
  }

  override def inferTypes: OpenCLFunction = {
    val args_ = args.map(TypeInference(_))
    val dts = args_.map(_.t.dataType)
    OpenCLFunction(name, dts, outT, args_)
  }

  override def visitAndRebuild(f: Visitor): Phrase[ExpType] = {
    OpenCLFunction(name, inTs.map(f(_)), f(outT), args.map(VisitAndRebuild(_, f)))
  }

  override def codeGenExp(env: Environment): Expression = {
    FunctionCall(name, args.map(CodeGenerator.exp(_, env)).toList)
  }

  override def eval(s: Store): Data = ???

  override def prettyPrint: String = s"(__${name}__ ${args.map(PrettyPhrasePrinter(_))})"

  override def xmlPrinter: Elem =
    <OpenCLFunction name={ToString(name)} inTs={ToString(inTs)} outT={ToString(outT)}>
      {args.map(Core.xmlPrinter(_))}
    </OpenCLFunction>

  override def acceptorTranslation(A: Phrase[AccType]): Phrase[CommandType] = {
    import RewriteToImperative._

    def recurse(ts: Seq[(Phrase[ExpType], DataType)],
                exps: Seq[Phrase[ExpType]],
                inTs: Seq[DataType]): Phrase[CommandType] = {
      ts match {
        // with only one argument left to process return the assignment of the OpenCLFunction call
        case Seq( (arg, inT) ) =>
          con(arg)(λ(exp"[$inT]")(e =>
            A :=|outT| OpenCLFunction(name, inTs :+ inT, outT, exps :+ e) ))
        // with a `tail` of arguments left, recurse
        case Seq( (arg, inT), tail@_* ) =>
          con(arg)(λ(exp"[$inT]")(e => recurse(tail, exps :+ e, inTs :+ inT) ))
      }
    }

    recurse(args zip inTs, Seq(), Seq())
  }

  override def continuationTranslation(C: Phrase[->[ExpType, CommandType]]): Phrase[CommandType] = {
    import RewriteToImperative._

    def recurse(ts: Seq[(Phrase[ExpType], DataType)],
                es: Seq[Phrase[ExpType]],
                inTs: Seq[DataType]): Phrase[CommandType] = {
      ts match {
        // with only one argument left to process continue with the OpenCLFunction call
        case Seq( (arg, inT) ) =>
          con(arg)(λ(exp"[$inT]")(e => C(OpenCLFunction(name, inTs :+ inT, outT, es :+ e)) ))
        // with a `tail` of arguments left, recurse
        case Seq( (arg, inT), tail@_* ) =>
          con(arg)(λ(exp"[$inT]")(e => recurse(tail, es :+ e, inTs :+ inT) ))
      }
    }

    recurse(args zip inTs, Seq(), Seq())
  }

}

