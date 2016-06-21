package ExpPatterns

import CommandPatterns._
import Core._
import Core.PhraseType._
import Core.OperationalSemantics._
import DSL._
import Compiling.RewriteToImperative
import apart.arithmetic.ArithExpr
import opencl.generator.OpenCLAST.Expression

abstract class AbstractMap(f: Phrase[ExpType -> ExpType],
                           array: Phrase[ExpType],
                           makeMap: (Phrase[ExpType -> ExpType], Phrase[ExpType]) => AbstractMap,
                           makeMapI: (Phrase[AccType], Phrase[AccType -> (ExpType -> CommandType)], Phrase[ExpType]) => AbstractMapI)
  extends ExpPattern {

  protected var n: ArithExpr = null
  protected var dt1: DataType = null
  protected var dt2: DataType = null

  override def typeCheck(): ExpType = {
    import TypeChecker._
    TypeChecker(array) match {
      case ExpType(ArrayType(n_, dt1_)) =>
        n = n_; dt1 = dt1_
        setParamType(f, ExpType(dt1))
        TypeChecker(f) match {
          case FunctionType(ExpType(t_), ExpType(dt2_)) =>
            dt2 = dt2_
            if (dt1 == t_) {
              ExpType(ArrayType(n, dt2))
            } else {
              error(dt1.toString + " and " + t_.toString, expected = "them to match")
            }
          case t_ => error(t_.toString, "FunctionType")
        }
      case t_ => error(t_.toString, "ArrayType")
    }
  }

  override def visitAndRebuild(fun: VisitAndRebuild.fun): Phrase[ExpType] = {
    val m = makeMap(VisitAndRebuild(f, fun), VisitAndRebuild(array, fun))
    m.n = n
    m.dt1 = dt1
    m.dt2 = dt2
    m
  }

  override def eval(s: Store): Data = {
    import OperationalSemantics._
    val fE = OperationalSemantics.eval(s, f)
    OperationalSemantics.eval(s, array) match {
      case ArrayData(xs) =>
        ArrayData(xs.map { x => OperationalSemantics.eval(s, fE(LiteralPhrase(x))) })

      case _ => throw new Exception("This should not happen")
    }
  }

  override def rewriteToImperativeAcc(A: Phrase[AccType]): Phrase[CommandType] = {
    assert(n != null && dt1 != null && dt2 != null)
    import RewriteToImperative._

    exp(array)(λ( ExpType(ArrayType(n, dt1)) ) { x =>
      makeMapI(A,
        λ( AccType(dt2) ) { o =>
          λ( ExpType(dt1) ) { x => acc(f(x))(o) } },
        x
      )
    })

  }

  override def rewriteToImperativeExp(C: Phrase[->[ExpType, CommandType]]): Phrase[CommandType] = {
    assert(n != null && dt1 != null && dt2 != null)
    import RewriteToImperative._

    `new`(ArrayType(n, dt2), GlobalMemory, tmp =>
      acc(this)(tmp.wr) `;`
      C(tmp.rd)
    )
  }

  override def prettyPrint: String = s"(${this.getClass.getSimpleName} ${PrettyPrinter(f)} ${PrettyPrinter(array)})"

}

case class Map(f: Phrase[ExpType -> ExpType], array: Phrase[ExpType]) extends AbstractMap(f, array, Map, MapI)