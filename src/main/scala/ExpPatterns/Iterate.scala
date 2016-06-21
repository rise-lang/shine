package ExpPatterns

import Core._
import Core.PhraseType._
import Core.OperationalSemantics._
import apart.arithmetic.ArithExpr
import opencl.generator.OpenCLAST.Expression

case class Iterate(n: ArithExpr,
                   f: Phrase[ExpType -> ExpType],
                   array: Phrase[ExpType]) extends ExpPattern {

  override def typeCheck(): ExpType = {
    import TypeChecker._
    TypeChecker(array) match {
      case t@ExpType(ArrayType(m, dt)) =>
        setParamType(f, t)
        TypeChecker(f) match {
          case FunctionType(t1, t2) if (t1 == t2) && (t == t1) =>
            t // improve and capture effect on array size
          case ft => error(ft.toString, "FunctionType")
        }
      case t_ => error(t_.toString, "ArrayType")
    }
  }

  override def visitAndRebuild(fun: VisitAndRebuild.fun): Phrase[ExpType] = {
    Iterate(n, VisitAndRebuild(f, fun), VisitAndRebuild(array, fun))
  }

  override def eval(s: Store): Data = {
    val fE = OperationalSemantics.eval(s, f)
    OperationalSemantics.eval(s, array) match {
      case ArrayData(xs) =>
        var a = array
        for (_ <- 0 until n.eval) {
          a = fE(a)
        }
        OperationalSemantics.eval(s, a)
      case _ => throw new Exception("This should not happen")
    }
  }

  override def prettyPrint: String = s"(iterate ${n.toString} ${PrettyPrinter(f)})"

  override def rewriteToImperativeAcc(A: Phrase[AccType]): Phrase[CommandType] = {
    import Compiling.RewriteToImperative._

    ???

    /*
  new \(output =>
    new \(tmp =>

    )
  )
  */
  }

  override def rewriteToImperativeExp(C: Phrase[->[ExpType, CommandType]]): Phrase[CommandType] = {
    import Compiling.RewriteToImperative._

    ???
  }

/*
  exp(array)(λ( ExpType(ArrayType(n, dt1)) ) { x =>
      makeMapI(A,
        λ( AccType(dt2) ) { o =>
          λ( ExpType(dt1) ) { x => acc(f(x))(o) } },
        x
      )
    })

  `new`(ArrayType(n, dt2), GlobalMemory, tmp =>
      acc(this)(tmp.wr) `;`
      C(tmp.rd)
    )
   */
}