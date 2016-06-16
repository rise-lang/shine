package CommandPatterns

import Core._
import Core.PhraseType._
import Core.OperationalSemantics._
import DSL._
import AccPatterns._
import ExpPatterns._
import Rewriting.SubstituteImplementations
import opencl.generator.OpenCLAST.Block

abstract class AbstractMapI(out: Phrase[AccType],
                            f: Phrase[AccType -> (ExpType -> CommandType)],
                            in: Phrase[ExpType])
  extends CommandPattern {

  override def typeCheck(): CommandType = {
    import TypeChecker._
    (TypeChecker(out), TypeChecker(in)) match {
      case (AccType(ArrayType(n, dt2)), ExpType(ArrayType(m, dt1))) if n == m =>
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

  override def toOpenCL(b: Block): Block = ???

  override def visitAndRebuild(fun: VisitAndRebuild.fun): Phrase[CommandType] = {
    makeMapI(VisitAndRebuild(out, fun), VisitAndRebuild(f, fun), VisitAndRebuild(in, fun))
  }

  def makeMapI: (Phrase[AccType], Phrase[AccType -> (ExpType -> CommandType)], Phrase[ExpType]) => AbstractMapI

  override def prettyPrint: String = s"${this.getClass.getSimpleName} ${PrettyPrinter(out)} ${PrettyPrinter(f)} ${PrettyPrinter(in)}"

}

case class MapI(out: Phrase[AccType],
                f: Phrase[AccType -> (ExpType -> CommandType)],
                in: Phrase[ExpType]) extends AbstractMapI(out, f, in) {

  override def makeMapI = MapI

  override def substituteImpl: Phrase[CommandType] = {
    val l = length(in)
    TypeChecker(l)
    `parFor`(l, out, i => o => {
      SubstituteImplementations( f(o)(in `@` i) )
    })
  }

}
