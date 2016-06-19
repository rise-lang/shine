package ExpPatterns

import Core.OperationalSemantics.{Data, Store}
import Core.PhraseType.->
import Core._
import Compiling.RewriteToImperative
import DSL._
import apart.arithmetic.ArithExpr
import opencl.generator.OpenCLAST.Expression

abstract class To(f: Phrase[ExpType -> ExpType],
                  input: Phrase[ExpType],
                  addressSpace: AddressSpace,
                  makeTo: (Phrase[ExpType -> ExpType], Phrase[ExpType]) => To) extends ExpPattern {

  private var dt1: DataType = null
  private var dt2: DataType = null

  override def typeCheck(): ExpType = {
    import TypeChecker._
    TypeChecker(input) match {
      case ExpType(dt1_) =>
        dt1 = dt1_
        setParamType(f, ExpType(dt1))
        TypeChecker(f) match {
          case FunctionType(ExpType(t1_), ExpType(dt2_)) =>
            dt2 = dt2_
            if (dt1 == t1_) {
              ExpType(dt2)
            } else {
              error(dt1.toString + " and " + t1_.toString, expected = "them to match")
            }
          case x => error(x.toString, "FunctionType")
        }
      case x => error(x.toString, "ExpType")
    }
  }

  override def eval(s: Store): Data = OperationalSemantics.eval(s, input)

  override def visitAndRebuild(fun: VisitAndRebuild.fun): Phrase[ExpType] = {
    val tl = makeTo(VisitAndRebuild(f, fun), VisitAndRebuild(input, fun))
    tl.dt1 = dt1
    tl.dt2 = dt2
    tl
  }

  override def prettyPrint: String = s"(toLocal ${PrettyPrinter(input)})"

  override def rewriteToImperativeAcc(A: Phrase[AccType]): Phrase[CommandType] = {
    assert(dt1 != null && dt2 != null)

    RewriteToImperative.exp(this, λ(ExpType(dt2)) { x =>
      RewriteToImperative.acc(x, A)
    })
  }

  override def rewriteToImperativeExp(C: Phrase[->[ExpType, CommandType]]): Phrase[CommandType] = {
    assert(dt1 != null && dt2 != null)

    `new`(dt2, addressSpace, tmp =>
      RewriteToImperative.acc(f(input), tmp.wr) `;` C(tmp.rd)
    )
  }
}
