package HighLevelCombinators

import Compiling.RewriteToImperative
import Core.OperationalSemantics._
import Core._
import DSL.typed._
import LowLevelCombinators.SplitAcc
import apart.arithmetic.ArithExpr
import opencl.generator.OpenCLAST.Expression

import scala.xml.Elem

case class Split(n: ArithExpr,
                 m: ArithExpr,
                 dt: DataType,
                 array: Phrase[ExpType])
  extends HighLevelCombinator with ViewExp {

  override lazy val `type` = exp"[$m.$n.$dt]"

  override def typeCheck(): Unit = {
    import TypeChecker._
    array checkType exp"[${m * n}.$dt]"
  }

  override def inferTypes: Split = {
    import TypeInference._
    val array_ = TypeInference(array)
    array_.t match {
      case ExpType(ArrayType(mn_, dt_)) =>
        Split(n, mn_ /^ n, dt_, array_)
      case x => error(x.toString, "ArrayType")
    }
  }

  override def visitAndRebuild(fun: VisitAndRebuild.fun): Phrase[ExpType] = {
    Split(fun(n), fun(m), fun(dt), VisitAndRebuild(array, fun))
  }

  override def eval(s: Store): Data = {
    OperationalSemantics.eval(s, array) match {
      case ArrayData(arrayE) =>

        def split[T](n: ArithExpr, vector: Vector[T]): Vector[Vector[T]] = {
          val builder = Vector.newBuilder[Vector[T]]
          var vec = vector
          for (i <- 0 until vector.length / n.eval) {
            val (head, tail) = vec splitAt n.eval
            vec = tail
            builder += head
          }
          builder.result()
        }

        ArrayData(split(n, arrayE).map(ArrayData))

      case _ => throw new Exception("This should not happen")
    }
  }

  override def toOpenCL(env: ToOpenCL.Environment,
                        arrayAccess: List[(ArithExpr, ArithExpr)],
                        tupleAccess: List[ArithExpr],
                        dt: DataType): Expression = {

    val (firstTwo, rest) = arrayAccess.splitAt(2)

    val chunkId = firstTwo.head
    val chunkElemId = firstTwo.tail.head

    val newIdx = chunkId._1 * n + chunkElemId._1

    ToOpenCL.exp(array, env, (newIdx, chunkElemId._2) :: rest, tupleAccess, dt)
  }

  override def prettyPrint: String = s"(split $n ${PrettyPrinter(array)})"

  override def xmlPrinter: Elem =
    <split n={ToString(n)} m={ToString(m)} dt={ToString(dt)}>
      {Core.xmlPrinter(array)}
    </split>

  override def rewriteToImperativeAcc(A: Phrase[AccType]): Phrase[CommandType] = {
    import RewriteToImperative._
    acc(array)(SplitAcc(n, m, dt, A))
  }

  override def rewriteToImperativeExp(C: Phrase[->[ExpType, CommandType]]): Phrase[CommandType] = {
    import RewriteToImperative._
    exp(array)(λ(array.t) { x =>
      C(Split(n, m, dt, x))
    })
  }
}