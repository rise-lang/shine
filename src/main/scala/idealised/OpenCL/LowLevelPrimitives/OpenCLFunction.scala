package idealised.OpenCL.LowLevelPrimitives

import idealised.Compiling.RewriteToImperative
import idealised.Core
import idealised.Core.OperationalSemantics.{Data, Store}
import idealised.Core.VisitAndRebuild.Visitor
import idealised.Core._
import idealised.DSL.typed._
import idealised.IntermediatePrimitives.MapI
import idealised.OpenCL.Core.{GeneratableExp, ToOpenCL}
import idealised.OpenCL.Core.ToOpenCL.Environment
import opencl.generator.OpenCLAST.{Expression, FunctionCall}

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

  override def toOpenCL(env: Environment): Expression = {
    FunctionCall(name, args.map(ToOpenCL.exp(_, env)).toList)
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
                es: Seq[Phrase[ExpType]],
                inTs: Seq[DataType]): Phrase[CommandType] = {
      ts match {
        case Seq( (arg, inT) ) =>
          con(arg)(λ(exp"[$inT]")(e =>
            outT match {
              case b: BasicType => A `:=` OpenCLFunction(name, inTs :+ inT, outT, es :+ e)
              case ArrayType(n, dt) =>
                MapI(n, dt, dt, λ(ExpType(dt))(e => λ(AccType(dt))(a => acc(e)(a))),
                  OpenCLFunction(name, inTs :+ inT, outT, es :+ e), A)
              case RecordType(dt11, dt12) =>
                acc(fst(OpenCLFunction(name, inTs :+ inT, outT, es :+ e)))(recordAcc1(dt11, dt12, A)) `;`
                  acc(snd(OpenCLFunction(name, inTs :+ inT, outT, es :+ e)))(recordAcc2(dt11, dt12, A))
              case _: DataTypeIdentifier => throw new Exception("This should not happen")
            }
          ))
        case Seq( (arg, inT), tail@_* ) =>
          con(arg)(λ(exp"[$inT]")(e => recurse(tail, es :+ e, inTs :+ inT) ))
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
        case Seq( (arg, inT) ) =>
          con(arg)(λ(exp"[$inT]")(e =>
            C(OpenCLFunction(name, inTs :+ inT, outT, es :+ e))
          ))
        case Seq( (arg, inT), tail@_* ) =>
          con(arg)(λ(exp"[$inT]")(e => recurse(tail, es :+ e, inTs :+ inT) ))
      }
    }

    recurse(args zip inTs, Seq(), Seq())
  }

}

