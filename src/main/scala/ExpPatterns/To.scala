package ExpPatterns

import Core.OperationalSemantics.{Data, Store}
import Core.PhraseType._
import Core._
import Compiling.RewriteToImperative
import DSL._

import scala.xml.Elem

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
    tl.dt1 = fun(dt1)
    tl.dt2 = fun(dt2)
    tl
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

    exp(this)(λ(ExpType(dt2)) { x =>
      acc(x)(A)
    })
  }

  override def rewriteToImperativeExp(C: Phrase[->[ExpType, CommandType]]): Phrase[CommandType] = {
    assert(dt1 != null && dt2 != null)
    import RewriteToImperative._

    `new`(dt2, addressSpace, tmp =>
      acc(f(input))(tmp.wr) `;` C(tmp.rd)
    )
  }
}
