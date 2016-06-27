package CommandPatterns

import Core._
import Core.PhraseType._
import Core.OperationalSemantics._
import DSL._
import Compiling.SubstituteImplementations
import apart.arithmetic.ArithExpr
import opencl.generator.OpenCLAST.Block

import scala.xml.Elem

case class ReduceIExp(n: ArithExpr,
                      dt1: DataType,
                      dt2: DataType,
                      out: Phrase[ExpType -> CommandType],
                      f: Phrase[AccType -> (ExpType -> (ExpType -> CommandType))],
                      init: Phrase[ExpType],
                      in: Phrase[ExpType]) extends IntermediateCommandPattern {

  override def typeCheck(): CommandType = {
    import TypeChecker._
    (TypeChecker(init), TypeChecker(in)) match {
      case (ExpType(dt2_), ExpType(ArrayType(n_, dt1_)))
        if n == n_ && dt1 == dt1_ && dt2 == dt2_ =>

        setParamType(out, ExpType(dt2))
        TypeChecker(out) match {
          case FunctionType(ExpType(dt), CommandType()) =>
            if (dt2 != dt) error(dt2.toString + " and " + dt.toString, expected = "them to match")
          case ty => error(ty.toString, "FunctionType")
        }

        setParamType(f, AccType(dt2))
        setSecondParamType(f, ExpType(dt1))
        setThirdParamType(f, ExpType(dt2))
        TypeChecker(f) match {
          case FunctionType(AccType(t1), FunctionType(ExpType(t2), FunctionType(ExpType(t3), CommandType()))) =>
            if (dt2 == t1 && dt1 == t2 && dt2 == t3) CommandType()
            else {
              error(dt2.toString + ", " + t1.toString + " as well as " +
                dt1.toString + ", " + t2.toString + " and " + dt2.toString + ", " + t3.toString,
                expected = "them to match")
            }
          case x => error(x.toString, "FunctionType")
        }
      case x => error(x.toString, "(AccType, ExpType, ArrayType)")
    }
  }

  override def visitAndRebuild(fun: VisitAndRebuild.fun): Phrase[CommandType] = {
    ReduceIExp(fun(n), fun(dt1), fun(dt2),
      VisitAndRebuild(out, fun),
      VisitAndRebuild(f, fun),
      VisitAndRebuild(init, fun),
      VisitAndRebuild(in, fun))
  }

  override def eval(s: Store): Store = {
    val outE = OperationalSemantics.eval(s, out)
    OperationalSemantics.eval(s, `new`(init.t.dataType, PrivateMemory, accum => {
      ReduceIAcc(n, dt1, dt2, π2(accum), f, init, in) `;`
        outE(π1(accum))
    }))
  }

  override def prettyPrint =
    s"(reduceIExp ${PrettyPrinter(out)} ${PrettyPrinter(f)} ${PrettyPrinter(init)} ${PrettyPrinter(in)})"

  override def xmlPrinter: Elem =
    <reduceIExp n={ToString(n)} dt1={ToString(dt1)} dt2={ToString(dt2)}>
      <output type={ToString(ExpType(dt2) -> CommandType())}>{Core.xmlPrinter(out)}</output>
      <f type={ToString(AccType(dt2) -> (ExpType(dt1) -> (ExpType(dt2) -> CommandType())))}>{Core.xmlPrinter(f)}</f>
      <init type={ToString(ExpType(dt2))}>{Core.xmlPrinter(init)}</init>
      <input type={ToString(ExpType(ArrayType(n, dt1)))}>{Core.xmlPrinter(in)}</input>
    </reduceIExp>

  override def substituteImpl(env: SubstituteImplementations.Environment): Phrase[CommandType] = {
    `new`(init.t.dataType, PrivateMemory, accum => {
      (accum.wr `:=` init) `;`
        `for`(n, i => {
          SubstituteImplementations( f(accum.wr)(in `@` i)(accum.rd), env )
        }) `;`
        out(accum.rd)
    } )
  }

}
