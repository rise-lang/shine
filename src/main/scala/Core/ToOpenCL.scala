package Core

import Core.OperationalSemantics._
import Core.PhraseType.->
import Compiling.{RewriteToImperative, SubstituteImplementations}
import apart.arithmetic.{ArithExpr, Cst, NamedVar, Var}
import ir.{Type, UndefType}
import opencl.generator.OpenCLAST._
import DSL._

import scala.collection.immutable.List

import scala.collection._

class ToOpenCL(val localSize: ArithExpr, val globalSize: ArithExpr) {

  val env = mutable.Map[String, apart.arithmetic.Range]()

  def apply(p: Phrase[ExpType -> ExpType],
            arg: IdentPhrase[ExpType]): Function =
    make(p, arg, List())

  def apply(p: Phrase[ExpType -> (ExpType -> ExpType)],
            arg0: IdentPhrase[ExpType],
            arg1: IdentPhrase[ExpType]): Function =
    make(p, arg0, arg1, List())

  def apply(p: Phrase[ExpType -> (ExpType -> (ExpType -> ExpType))],
            arg0: IdentPhrase[ExpType],
            arg1: IdentPhrase[ExpType],
            arg2: IdentPhrase[ExpType]): Function =
    make(p, arg0, arg1, arg2, List())

  def apply(p: Phrase[ExpType -> (ExpType -> (ExpType -> (ExpType -> ExpType)))],
            arg0: IdentPhrase[ExpType],
            arg1: IdentPhrase[ExpType],
            arg2: IdentPhrase[ExpType],
            arg3: IdentPhrase[ExpType]): Function =
    make(p, arg0, arg1, arg2, arg3, List())

  def apply(p: Phrase[ExpType -> (ExpType -> (ExpType -> (ExpType -> (ExpType -> ExpType))))],
            arg0: IdentPhrase[ExpType],
            arg1: IdentPhrase[ExpType],
            arg2: IdentPhrase[ExpType],
            arg3: IdentPhrase[ExpType],
            arg4: IdentPhrase[ExpType]): Function =
    make(p, arg0, arg1, arg2, arg3, arg4, List())

  private def make(p: Phrase[ExpType],
                   args: List[IdentPhrase[ExpType]]): Function = {
    val outT = TypeChecker(p)
    val out = identifier("output", AccType(outT.dataType))
    val params = makeParams(out, args: _*)

    val p2 = RewriteToImperative.acc(p, out)
    TypeChecker(p2)
    val p3 = SubstituteImplementations(p2)
    TypeChecker(p3)

    println(PrettyPrinter(p3))

    val p4 = AdjustMemoryAllocation(p3)

    println("--------------------")

    println(PrettyPrinter(p4))

    val body = ToOpenCL.cmd(p4, Block(), this)

    Function(name = "KERNEL", ret = UndefType, params = params, body = body, kernel = true)
  }

  private def make(p: Phrase[ExpType -> ExpType],
                   arg: IdentPhrase[ExpType],
                   args: List[IdentPhrase[ExpType]]): Function = {
    make(p(arg), arg +: args)
  }

  private def make(p: Phrase[ExpType -> (ExpType -> ExpType)],
                   arg0: IdentPhrase[ExpType],
                   arg1: IdentPhrase[ExpType],
                   args: List[IdentPhrase[ExpType]]): Function = {
    make(p(arg0), arg1, arg0 +: args)
  }

  private def make(p: Phrase[ExpType -> (ExpType -> (ExpType -> ExpType))],
                   arg0: IdentPhrase[ExpType],
                   arg1: IdentPhrase[ExpType],
                   arg2: IdentPhrase[ExpType],
                   args: List[IdentPhrase[ExpType]]): Function = {
    make(p(arg0), arg1, arg2, arg0 +: args)
  }

  private def make(p: Phrase[ExpType -> (ExpType -> (ExpType -> (ExpType -> ExpType)))],
                   arg0: IdentPhrase[ExpType],
                   arg1: IdentPhrase[ExpType],
                   arg2: IdentPhrase[ExpType],
                   arg3: IdentPhrase[ExpType],
                   args: List[IdentPhrase[ExpType]]): Function = {
    make(p(arg0), arg1, arg2, arg3, arg0 +: args)
  }

  private def make(p: Phrase[ExpType -> (ExpType -> (ExpType -> (ExpType -> (ExpType -> ExpType))))],
                   arg0: IdentPhrase[ExpType],
                   arg1: IdentPhrase[ExpType],
                   arg2: IdentPhrase[ExpType],
                   arg3: IdentPhrase[ExpType],
                   arg4: IdentPhrase[ExpType],
                   args: List[IdentPhrase[ExpType]]): Function = {
    make(p(arg0), arg1, arg2, arg3, arg4, arg0 +: args)
  }

  private def makeParams(out: IdentPhrase[AccType], args: IdentPhrase[ExpType]*): List[ParamDecl] = {
    val output = ParamDecl(out.name, DataType.toType(out.t.dataType), opencl.ir.GlobalMemory, const = false)

    val inputs = args.map(arg =>
      ParamDecl(arg.name, DataType.toType(arg.t.dataType), opencl.ir.GlobalMemory, const = true)
    )

    val types = args.map(_.t.dataType).+:(out.t.dataType).map(DataType.toType)
    val lengths = types.flatMap(Type.getLengths)
    val vars = lengths.filter(_.isInstanceOf[Var]).distinct

    val varDecls = vars.map(v =>
      ParamDecl(v.toString, opencl.ir.Int)
    )

    List(output) ++ inputs ++ varDecls
  }

}

object ToOpenCL {

  def cmd(p: Phrase[CommandType], block: Block, opencl: ToOpenCL): Block = {
    p match {
      case IfThenElsePhrase(condP, thenP, elseP) =>
        val trueBlock = cmd(thenP, Block(), opencl)
        val falseBlock = cmd(elseP, Block(), opencl)
        (block: Block) += IfThenElse(exp(condP, opencl), trueBlock, falseBlock)

      case c: CommandPattern => c.toOpenCL(block, opencl)

      case ApplyPhrase(_, _) | IdentPhrase(_) | Proj1Phrase(_) | Proj2Phrase(_) =>
        throw new Exception("This should not happen")
    }
  }

  def exp(p: Phrase[ExpType], opencl: ToOpenCL): Expression = {
    p match {
      case BinOpPhrase(op, lhs, rhs) =>
        BinaryExpression(op.toString, exp(lhs, opencl), exp(rhs, opencl))
      case IdentPhrase(name) => VarRef(name)
      case LiteralPhrase(d) =>
        d match {
          case i: IntData => Literal(i.i.toString)
          case b: BoolData => Literal(b.b.toString)
          case f: FloatData => Literal(f.f.toString)
          case i: IndexData => Literal(i.i.toString)
          case i: Int4Data => Literal(s"(int4)(${i.i0.toString}, ${i.i1.toString}, ${i.i2.toString}, ${i.i3.toString})")
          case _: RecordData => ???
          case _: ArrayData => ???
        }
      case p: Proj1Phrase[ExpType, _] => exp(Lift.liftPair(p.pair)._1, opencl)
      case p: Proj2Phrase[_, ExpType] => exp(Lift.liftPair(p.pair)._2, opencl)
      case UnaryOpPhrase(op, x) =>
        UnaryExpression(op.toString, exp(x, opencl))
      case e: ExpPattern => e.toOpenCL(opencl)

      case ApplyPhrase(_, _) | IfThenElsePhrase(_, _, _) =>
        throw new Exception("This should not happen")
    }
  }

  def acc(p: Phrase[AccType], opencl: ToOpenCL): VarRef = {
    p match {
      case IdentPhrase(name) => VarRef(name)
      case p: Proj1Phrase[AccType, _] => acc(Lift.liftPair(p.pair)._1, opencl)
      case p: Proj2Phrase[_, AccType] => acc(Lift.liftPair(p.pair)._2, opencl)
      case a: AccPattern => a.toOpenCL(opencl)

      case ApplyPhrase(_, _) | IfThenElsePhrase(_, _, _) =>
        throw new Exception("This should not happen")
    }
  }

  def exp(p: Phrase[ExpType], opencl: ToOpenCL, arrayAccess: List[(ArithExpr, ArithExpr)], tupleAccess: List[ArithExpr]): Expression = {
    p match {
      case IdentPhrase(name) =>
        val i = arrayAccess.map(x => x._1 * x._2).foldLeft(0: ArithExpr)((x, y) => x + y)
        val index = if (i != Cst(0)) {
          i
        } else {
          null
        }

        val s = tupleAccess.map(x => if (x == Cst(1)) {
          "._1"
        } else if (x == Cst(2)) {
          "._2"
        }).foldLeft("")(_ + _)
        val suffix = if (s != "") {
          s
        } else {
          null
        }

        VarRef(name, suffix, ArithExpression(index))

      case p: Proj1Phrase[ExpType, _] => exp(Lift.liftPair(p.pair)._1, opencl, arrayAccess, tupleAccess)
      case p: Proj2Phrase[_, ExpType] => exp(Lift.liftPair(p.pair)._2, opencl, arrayAccess, tupleAccess)

      case e: ExpPattern => e.toOpenCL(opencl, arrayAccess, tupleAccess)

      case ApplyPhrase(_, _) | BinOpPhrase(_, _, _) | UnaryOpPhrase(_, _) | IfThenElsePhrase(_, _, _) | LiteralPhrase(_) =>
        throw new Exception("This should not happen")
    }
  }

  def acc(p: Phrase[AccType], opencl: ToOpenCL, arrayAccess: List[(ArithExpr, ArithExpr)], tupleAccess: List[ArithExpr]): VarRef = {
    p match {
      case IdentPhrase(name) =>
        val i = arrayAccess.map(x => x._1 * x._2).foldLeft(0: ArithExpr)((x, y) => x + y)
        val index = if (i != Cst(0)) {
          i
        } else {
          null
        }

        val s = tupleAccess.map(x => if (x == Cst(1)) {
          "._1"
        } else if (x == Cst(2)) {
          "._2"
        }).foldLeft("")(_ + _)
        val suffix = if (s != "") {
          s
        } else {
          null
        }

        VarRef(name, suffix, ArithExpression(index))

      case a: AccPattern => a.toOpenCL(opencl, arrayAccess, tupleAccess)

      case p: Proj1Phrase[AccType, _] => acc(Lift.liftPair(p.pair)._1, opencl, arrayAccess, tupleAccess)
      case p: Proj2Phrase[_, AccType] => acc(Lift.liftPair(p.pair)._2, opencl, arrayAccess, tupleAccess)

      case ApplyPhrase(_, _) | IfThenElsePhrase(_, _, _) =>
        throw new Exception("This should not happen")
    }
  }

}
