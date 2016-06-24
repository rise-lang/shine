package CommandPatterns

import AccPatterns._
import Compiling.SubstituteImplementations
import Core.OperationalSemantics._
import Core.PhraseType._
import Core._
import DSL._
import ExpPatterns._
import apart.arithmetic.ArithExpr

abstract class AbstractMapI(n: ArithExpr,
                            dt1: DataType,
                            dt2: DataType,
                            out: Phrase[AccType],
                            f: Phrase[AccType -> (ExpType -> CommandType)],
                            in: Phrase[ExpType])
  extends IntermediateCommandPattern {

  override def typeCheck(): CommandType = {
    import TypeChecker._
    (TypeChecker(out), TypeChecker(in)) match {
      case (AccType(ArrayType(n_, dt2_)), ExpType(ArrayType(m_, dt1_))) =>
        if (n_ == n && m_ == n && dt2_ == dt2 && dt1_ == dt1) {
          setParamType(f, AccType(dt2))
          setSecondParamType(f, ExpType(dt1))
          TypeChecker(f) match {
            case FunctionType(AccType(t1), FunctionType(ExpType(t2), CommandType())) =>
              if (dt2 == t1 && dt1 == t2) CommandType()
              else {
                error(dt2.toString + " and " + t1.toString +
                  ", " + dt1.toString + " and " + t2.toString,
                  expected = "them to match")
              }
            case x => error(x.toString, "FunctionType")
          }
        } else {
          error(s"([$dt1_]_$m_ -> [$dt2_]_$n_)", s"[$dt1]_$n -> [$dt2]_$n")
        }
      case x => error(x.toString, "(ArrayType, ArrayType)")
    }
  }

  override def eval(s: Store): Store = {
    val fE = OperationalSemantics.eval(s, f)(BinaryFunctionEvaluator)
    val n = TypeChecker(in) match {
      case ExpType(ArrayType(len, _)) => len
    }

    (0 until n.eval).foldLeft(s)((sOld, i) => {
      val comm = fE(IdxAcc(out, LiteralPhrase(i)))(Idx(in, LiteralPhrase(i)))
      OperationalSemantics.eval(sOld, comm)
    })
  }

  override def visitAndRebuild(fun: VisitAndRebuild.fun): Phrase[CommandType] = {
    makeMapI(fun(n), fun(dt1), fun(dt2),
      VisitAndRebuild(out, fun),
      VisitAndRebuild(f, fun),
      VisitAndRebuild(in, fun))
  }

  def makeMapI: (ArithExpr, DataType, DataType, Phrase[AccType], Phrase[AccType -> (ExpType -> CommandType)], Phrase[ExpType]) => AbstractMapI

  override def prettyPrint: String = s"${this.getClass.getSimpleName} ${PrettyPrinter(out)} ${PrettyPrinter(f)} ${PrettyPrinter(in)}"

}

case class MapI(n: ArithExpr,
                dt1: DataType,
                dt2: DataType,
                out: Phrase[AccType],
                f: Phrase[AccType -> (ExpType -> CommandType)],
                in: Phrase[ExpType]) extends AbstractMapI(n, dt1, dt2, out, f, in) {

  override def makeMapI = MapI

  override def substituteImpl(env: SubstituteImplementations.Environment): Phrase[CommandType] = {
    `parFor`(n, dt2, out, i => o => {
      SubstituteImplementations(f(o)(in `@` i), env)
    })
  }

}
