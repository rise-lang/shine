package CommandPatterns

import Core._
import Core.PhraseType._
import Core.OperationalSemantics._
import DSL._
import AccPatterns._
import ExpPatterns._
import Compiling.SubstituteImplementations
import Core.VisitAndRebuild.fun
import apart.arithmetic._

case class IterateIExp(n: ArithExpr,
                       m: ArithExpr,
                       k: ArithExpr,
                       dt: DataType,
                       out: Phrase[ExpType -> CommandType],
                       f: Phrase[AccType -> (ExpType -> CommandType)],
                       in: Phrase[ExpType])
  extends IntermediateCommandPattern {

  override def typeCheck(): CommandType = {
    import TypeChecker._
    TypeChecker(in) match {
      case ExpType(ArrayType(nkm_, dt1_))
        if nkm_ == (n.pow(k) * m) && dt1_ == dt =>

        setParamType(out, ExpType(ArrayType(m, dt)))
        TypeChecker(out) match {
          case FunctionType(ExpType(ArrayType(m_, dt2_)), CommandType()) =>
            if (m_ != m || dt2_ != dt)
              error(m_.toString + " and " + m.toString + "; " +
                dt2_.toString + " and " + dt.toString, expected = "them to match")
          case x => error(x.toString, "FunctionType")
        }

        val l = NamedVar("l")
        setParamType(f, AccType(ArrayType(l, dt)))
        setSecondParamType(f, ExpType(ArrayType(l * n, dt)))
        TypeChecker(f) match {
          case FunctionType(AccType(ArrayType(l_, dt3_)),
          FunctionType(ExpType(ArrayType(ln_, dt4_)), CommandType()))
            if l_ == l && dt3_ == dt && ln_ == l*n && dt4_ == dt =>
            CommandType()
          case ft => error(ft.toString, "FunctionType")
        }
      case t_ => error(t_.toString, "ArrayType")
    }
  }

  override def eval(s: Store): Store = ???

  override def visitAndRebuild(fun: VisitAndRebuild.fun): Phrase[CommandType] = {
    IterateIExp(n, m, k, dt,
      VisitAndRebuild(out, fun),
      VisitAndRebuild(f, fun),
      VisitAndRebuild(in, fun))
  }

  override def substituteImpl: Phrase[CommandType] = {
    ???
  }

  override def prettyPrint: String =
    s"(iterateIExp ${PrettyPrinter(out)} ${PrettyPrinter(f)} ${PrettyPrinter(in)}"

}
