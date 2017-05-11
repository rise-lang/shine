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


final case class UnaryOpenCLFunction(name: String,
                                     inT: DataType,
                                     outT: DataType,
                                     arg: Phrase[ExpType])
  extends ExpPrimitive with GeneratableExp {

  override lazy val `type` = exp"[$outT]"

  override def typeCheck(): Unit = {
    import TypeChecker._
    (inT: DataType) -> (outT: DataType) ->
      (arg :: exp"[$inT]") -> `type`
  }

  override def inferTypes: UnaryOpenCLFunction = {
    val arg_ = TypeInference(arg)
    UnaryOpenCLFunction(name, arg_.t.dataType, outT, arg_)
  }

  override def visitAndRebuild(f: Visitor): Phrase[ExpType] = {
    UnaryOpenCLFunction(name, f(inT), f(outT), VisitAndRebuild(arg, f))
  }

  override def toOpenCL(env: Environment): Expression = {
    FunctionCall(name, List(ToOpenCL.exp(arg, env)))
  }

  override def eval(s: Store): Data = ???

  override def prettyPrint: String = s"(__${name}__ ${PrettyPhrasePrinter(arg)})"

  override def xmlPrinter: Elem =
    <UnaryOpenCLFunction name={ToString(name)} inT={ToString(inT)} outT={ToString(outT)}>
      {Core.xmlPrinter(arg)}
    </UnaryOpenCLFunction>

  override def acceptorTranslation(A: Phrase[AccType]): Phrase[CommandType] = {
    import RewriteToImperative._
    con(arg)(λ(exp"[$inT]")(e =>
      outT match {
        case b: BasicType => A `:=` UnaryOpenCLFunction(name, inT, outT, e)
        case ArrayType(n, dt) =>
          MapI(n, dt, dt, λ(ExpType(dt))(e => λ(AccType(dt))(a => acc(e)(a))), UnaryOpenCLFunction(name, inT, outT, e), A)
        case RecordType(dt11, dt12) =>
          acc(fst(UnaryOpenCLFunction(name, inT, outT, e)))(recordAcc1(dt11, dt12, A)) `;`
            acc(snd(UnaryOpenCLFunction(name, inT, outT, e)))(recordAcc2(dt11, dt12, A))
        case _: DataTypeIdentifier => throw new Exception("This should not happen")
      }
    ))
  }

  override def continuationTranslation(C: Phrase[->[ExpType, CommandType]]): Phrase[CommandType] = {
    RewriteToImperative.con(arg)(λ(exp"[$inT]")(e =>
      C(UnaryOpenCLFunction(name, inT, outT, e))
    ))
  }

}
