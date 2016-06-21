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

case class IterateI(n: ArithExpr,
                    m: ArithExpr,
                    k: ArithExpr,
                    dt: DataType,
                    out: Phrase[AccType],
                    f: Phrase[AccType -> (ExpType -> CommandType)],
                    in: Phrase[ExpType])
  extends IntermediateCommandPattern {

  override def typeCheck(): CommandType = {
    import TypeChecker._
    (TypeChecker(out), TypeChecker(in)) match {
      case (AccType(ArrayType(m_, dt1_)), ExpType(ArrayType(nkm_, dt2_)))
        if m_ == m && nkm_ == (n.pow(k) * m) && dt1_ == dt && dt2_ == dt =>

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
    IterateI(n, m, k, dt,
      VisitAndRebuild(out, fun),
      VisitAndRebuild(f, fun),
      VisitAndRebuild(in, fun))
  }

  override def substituteImpl: Phrase[CommandType] = {
    ???
  }

  override def prettyPrint: String =
    s"(iterateI ${PrettyPrinter(out)} ${PrettyPrinter(f)} ${PrettyPrinter(in)}"

}
