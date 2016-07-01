package HighLevelCombinators

import Compiling.RewriteToImperative
import Core.OperationalSemantics.{Data, Store}
import Core._
import DSL._

import scala.xml.Elem

abstract class To(dt1: DataType,
                  dt2: DataType,
                  f: Phrase[ExpType -> ExpType],
                  input: Phrase[ExpType],
                  addressSpace: AddressSpace,
                  makeTo: (DataType, DataType, Phrase[ExpType -> ExpType], Phrase[ExpType]) => To)
  extends HighLevelCombinator {

  override lazy val `type` = exp"[$dt2]"

  override def typeCheck(): Unit = {
    import TypeChecker._
    f checkType t"exp[$dt1] -> exp[$dt2]"
    input checkType exp"[$dt1]"
  }

  override def inferTypes: To = {
    import TypeInference._
    val input_ = TypeInference(input)
    input_.t match {
      case ExpType(dt1_) =>
        val f_ = TypeInference.setParamAndInferType(f, exp"[$dt1_]")
        f_.t match {
          case FunctionType(ExpType(t1_), ExpType(dt2_)) =>
            if (dt1_ == t1_) {
              makeTo(dt1_, dt2_, f_, input_)
            } else {
              error(dt1_.toString + " and " + t1_.toString, expected = "them to match")
            }
          case x => error(x.toString, "FunctionType")
        }
      case x => error(x.toString, "ExpType")
    }
  }

  override def eval(s: Store): Data = OperationalSemantics.eval(s, input)

  override def visitAndRebuild(fun: VisitAndRebuild.fun): Phrase[ExpType] = {
    makeTo(fun(dt1), fun(dt2), VisitAndRebuild(f, fun), VisitAndRebuild(input, fun))
  }

  override def prettyPrint: String =
    s"(to$addressSpace ${PrettyPrinter(f)} ${PrettyPrinter(input)})"

  override def xmlPrinter: Elem =
    <to dt1={ToString(dt1)} dt2={ToString(dt2)}>
      <f type={ToString(ExpType(dt1) -> ExpType(dt2))}>
        {Core.xmlPrinter(f)}
      </f>
      <input type={ToString(ExpType(dt1))}>
        {Core.xmlPrinter(input)}
      </input>
    </to>.copy(label = {
      val name = this.getClass.getSimpleName
      Character.toLowerCase(name.charAt(0)) + name.substring(1)
    })

  override def rewriteToImperativeAcc(A: Phrase[AccType]): Phrase[CommandType] = {
    assert(dt1 != null && dt2 != null)
    import RewriteToImperative._

    exp(this)(λ( exp"[$dt2]" )( x =>
      acc(x)(A)
    ))
  }

  override def rewriteToImperativeExp(C: Phrase[->[ExpType, CommandType]]): Phrase[CommandType] = {
    assert(dt1 != null && dt2 != null)
    import RewriteToImperative._

    `new`(dt2, addressSpace, tmp =>
      acc(f(input))(tmp.wr) `;` C(tmp.rd)
    )
  }
}
