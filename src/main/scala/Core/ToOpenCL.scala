package Core

import Core.OperationalSemantics._
import Compiling.{RewriteToImperative, SubstituteImplementations}
import apart.arithmetic.{ArithExpr, Cst, Var}
import ir.{Type, UndefType}
import opencl.generator.OpenCLAST._
import DSL._

import scala.collection.immutable.List

import scala.collection._

class ToOpenCL(val localSize: ArithExpr, val globalSize: ArithExpr) {

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
    val p1 = TypeInference(p)
    xmlPrinter.toFile("/tmp/p1.xml", p1)
    TypeChecker(p1)
    val outT = p1.t
    val out = identifier("output", AccType(outT.dataType))
    val params = makeParams(out, args: _*)

    val p2 = RewriteToImperative.acc(p1)(out)
    xmlPrinter.toFile("/tmp/p2.xml", p2)
    TypeChecker(p2)

    val p3 = SubstituteImplementations(p2,
      SubstituteImplementations.Environment(immutable.Map[String, AddressSpace]( ("output", GlobalMemory) )))
    xmlPrinter.toFile("/tmp/p3.xml", p3)
    TypeChecker(p3)

    val body = ToOpenCL.cmd(p3, Block(), ToOpenCL.Environment(localSize, globalSize))

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

  private def makeParams(out: IdentPhrase[AccType],
                         args: IdentPhrase[ExpType]*): List[ParamDecl] = {
    val output = ParamDecl(
      out.name,
      DataType.toType(out.t.dataType),
      opencl.ir.GlobalMemory,
      const = false)

    val inputs = args.map(arg =>
      ParamDecl(
        arg.name,
        DataType.toType(arg.t.dataType),
        opencl.ir.GlobalMemory,
        const = true)
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

  case class Environment(localSize: ArithExpr,
                         globalSize: ArithExpr,
                         ranges: mutable.Map[String, apart.arithmetic.Range])

  object Environment {
    def apply(localSize: ArithExpr, globalSize: ArithExpr): Environment = {
      Environment(localSize, globalSize,
        mutable.Map[String, apart.arithmetic.Range]())
    }
  }

  def cmd(p: Phrase[CommandType], block: Block, env: Environment): Block = {
    p match {
      case IfThenElsePhrase(condP, thenP, elseP) =>
        val trueBlock = cmd(thenP, Block(), env)
        val falseBlock = cmd(elseP, Block(), env)
        (block: Block) += IfThenElse(exp(condP, env), trueBlock, falseBlock)

      case c: IntermediateCommandPattern => c match {
        case fc: CommandPattern => fc.toOpenCL(block, env)
        case _ => throw new Exception(s"This should not happen $c")
      }

      case ApplyPhrase(_, _) | NatDependentApplyPhrase(_, _) |
           IdentPhrase(_, _) | Proj1Phrase(_) | Proj2Phrase(_) =>
        throw new Exception("This should not happen")
    }
  }

  def exp(p: Phrase[ExpType], env: Environment): Expression = {
    p match {
      case BinOpPhrase(op, lhs, rhs) =>
        BinaryExpression(op.toString, exp(lhs, env), exp(rhs, env))
      case IdentPhrase(name, _) => VarRef(name)
      case LiteralPhrase(d) =>
        d match {
          case i: IntData => Literal(i.i.toString)
          case b: BoolData => Literal(b.b.toString)
          case f: FloatData => Literal(f.f.toString)
          case i: IndexData => Literal(i.i.toString)
          case v: VectorData => Literal(Data.toString(v))
          case _: RecordData => ???
          case _: ArrayData => ???
        }
      case p: Proj1Phrase[ExpType, _] => exp(Lift.liftPair(p.pair)._1, env)
      case p: Proj2Phrase[_, ExpType] => exp(Lift.liftPair(p.pair)._2, env)
      case UnaryOpPhrase(op, x) =>
        UnaryExpression(op.toString, exp(x, env))
      case e: ExpPattern => e match {
        case g: GeneratableExpPattern => g.toOpenCL(env)
        case _ => throw new Exception("This should not happen")
      }

      case ApplyPhrase(_, _) | NatDependentApplyPhrase(_, _) |
           IfThenElsePhrase(_, _, _) =>
        throw new Exception("This should not happen")
    }
  }

  def acc(p: Phrase[AccType], env: Environment): VarRef = {
    p match {
      case IdentPhrase(name, _) => VarRef(name)
      case p: Proj1Phrase[AccType, _] => acc(Lift.liftPair(p.pair)._1, env)
      case p: Proj2Phrase[_, AccType] => acc(Lift.liftPair(p.pair)._2, env)
      case a: AccPattern => a.toOpenCL(env)

      case ApplyPhrase(_, _) | NatDependentApplyPhrase(_, _) |
           IfThenElsePhrase(_, _, _) =>
        throw new Exception("This should not happen")
    }
  }

  def exp(p: Phrase[ExpType],
          env: Environment,
          arrayAccess: List[(ArithExpr, ArithExpr)],
          tupleAccess: List[ArithExpr],
          dt: DataType): Expression = {
    p match {
      case IdentPhrase(name, t) =>
        val i = arrayAccess.map(x => x._1 * x._2).foldLeft(0: ArithExpr)((x, y) => x + y)
        val index = if (i != Cst(0)) { i } else { null }

        val s = tupleAccess.map {
            case Cst(1) => "._1"
            case Cst(2) => "._2"
            case _ => throw new Exception("This should not happen")
          }.foldLeft("")(_ + _)

        val suffix = if (s != "") { s } else { null }

        val originalType = t.dataType
        val currentType = dt

        (originalType, currentType) match {
          case (ArrayType(_, st1), VectorType(n, st2)) if st1 == st2 && st1.isInstanceOf[BasicType] =>
            VLoad(
              VarRef(name), DataType.toType(VectorType(n, st2)).asInstanceOf[ir.VectorType],
              ArithExpression(index))
          case _ =>
            VarRef(name, suffix, ArithExpression(index))
        }

      case p: Proj1Phrase[ExpType, _] => exp(Lift.liftPair(p.pair)._1, env, arrayAccess, tupleAccess, dt)
      case p: Proj2Phrase[_, ExpType] => exp(Lift.liftPair(p.pair)._2, env, arrayAccess, tupleAccess, dt)

      case e: ExpPattern => e match {
        case v: ViewExpPattern => v.toOpenCL(env, arrayAccess, tupleAccess, dt)
        case _ => throw new Exception(s"This should not happen: $e")
      }

      case ApplyPhrase(_, _) | NatDependentApplyPhrase(_, _) |
           BinOpPhrase(_, _, _) | UnaryOpPhrase(_, _) |
           IfThenElsePhrase(_, _, _) | LiteralPhrase(_) =>
        throw new Exception("This should not happen")
    }
  }

  def acc(p: Phrase[AccType],
          env: Environment,
          arrayAccess: List[(ArithExpr, ArithExpr)],
          tupleAccess: List[ArithExpr],
          dt: DataType): VarRef = {
    p match {
      case IdentPhrase(name, t) =>
        val i = arrayAccess.map(x => x._1 * x._2).foldLeft(0: ArithExpr)((x, y) => x + y)
        val index = if (i != Cst(0)) { i } else { null }

        val s = tupleAccess.map {
          case Cst(1) => "._1"
          case Cst(2) => "._2"
          case _ => throw new Exception("This should not happen")
        }.foldLeft("")(_ + _)

        val suffix = if (s != "") { s } else { null }

        val originalType = t.dataType
        val currentType = dt

        (originalType, currentType) match {
          case (ArrayType(_, st1), VectorType(n, st2)) if st1 == st2 && st1.isInstanceOf[BasicType] =>
            // TODO: can we turn this into a vload => need the value for this ...
            // TODO: figure out addressspace of identifier name
            VarRef(s"((/*the addressspace is hardcoded*/global $currentType*)$name)", suffix, ArithExpression(index))
          case _ =>
            VarRef(name, suffix, ArithExpression(index))
        }

      case a: AccPattern => a.toOpenCL(env, arrayAccess, tupleAccess, dt)

      case p: Proj1Phrase[AccType, _] => acc(Lift.liftPair(p.pair)._1, env, arrayAccess, tupleAccess, dt)
      case p: Proj2Phrase[_, AccType] => acc(Lift.liftPair(p.pair)._2, env, arrayAccess, tupleAccess, dt)

      case ApplyPhrase(_, _) | NatDependentApplyPhrase(_, _) |
           IfThenElsePhrase(_, _, _) =>
        throw new Exception("This should not happen")
    }
  }

}
