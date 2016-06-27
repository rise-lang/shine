package ExpPatterns

import AccPatterns.SplitAcc
import Core._
import Core.OperationalSemantics._
import Core.PhraseType.->
import Compiling.RewriteToImperative
import Core.PrettyPrinter.Indent
import DSL._
import apart.arithmetic.ArithExpr
import opencl.generator.OpenCLAST.Expression

case class Split(n: ArithExpr, array: Phrase[ExpType]) extends ExpPattern with ViewExpPattern {

  private var m: ArithExpr = null
  private var dt: DataType = null

  override def typeCheck(): ExpType = {
    import TypeChecker._
    TypeChecker(array) match {
      case ExpType(ArrayType(mn_, dt_)) =>
        m = mn_ /^ n; dt = dt_
        ExpType(ArrayType(m, ArrayType(n, dt)))
      case x => error(x.toString, "ArrayType")
    }
  }

  override def visitAndRebuild(fun: VisitAndRebuild.fun): Phrase[ExpType] = {
    var s = Split(n, VisitAndRebuild(array, fun))
    s.m = fun(m)
    s.dt = fun(dt)
    s
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

  override def toOpenCL(ocl: ToOpenCL, arrayAccess: List[(ArithExpr, ArithExpr)], tupleAccess: List[ArithExpr]): Expression = {

    val (firstTwo, rest) = arrayAccess.splitAt(2)

    val chunkId = firstTwo.head
    val chunkElemId = firstTwo.tail.head

    val newIdx = chunkId._1 * n + chunkElemId._1

    ToOpenCL.exp(array, ocl, (newIdx, chunkElemId._2) :: rest, tupleAccess)
  }

  override def prettyPrint(indent: Indent): String =
    indent + s"(split\n" +
      indent.more + s"$n : nat\n" +
      s"${PrettyPrinter(array, indent.more)} : exp[$n$m.$dt]\n" +
      indent + s") : exp[$n.$m.$dt]"

  override def rewriteToImperativeAcc(A: Phrase[AccType]): Phrase[CommandType] = {
    import RewriteToImperative._
    acc(array)(SplitAcc(n, m, dt, A))
  }

  override def rewriteToImperativeExp(C: Phrase[->[ExpType, CommandType]]): Phrase[CommandType] = {
    import RewriteToImperative._
    exp(array)(λ(array.t) { x =>
      C(Split(n, x))
    })
  }
}