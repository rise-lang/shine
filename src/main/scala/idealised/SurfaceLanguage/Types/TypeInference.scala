package idealised.SurfaceLanguage.Types

import idealised.SurfaceLanguage.DSL.Macros
import idealised.SurfaceLanguage._

import scala.language.existentials

class TypeInferenceException(expr: String, msg: String)
  extends TypeException(s"Failed to infer type for `$expr'. $msg.")

object TypeInference {
  def error(expr: String, found: String, expected: String): Nothing = {
    throw new TypeInferenceException(expr, s"Found $found but expected $expected")
  }

  def error(expr: String, msg: String): Nothing = {
    throw new TypeInferenceException(expr, msg)
  }

  type SubstitutionMap = scala.collection.Map[IdentifierExpr, IdentifierExpr]

  def apply(expr: Expr, subs: SubstitutionMap): Expr = {
    inferType(expr, subs)
  }

  private def inferType(expr: Expr, subs: SubstitutionMap): Expr = {
    expr match {
      case i@IdentifierExpr(name, _) =>
        val identifier =
          if (subs.contains(i)) {
            subs(i)
          } else {
            i
          }
        identifier.t match {
          case Some(_) => identifier
          case None => error(identifier.toString, s"Found Identifier $name without type")
        }

      case LambdaExpr(param, body) =>
        inferType(param, subs) match {
          case newParam: IdentifierExpr =>
            LambdaExpr(newParam, inferType(body, subs))
          case _ => throw new Exception("")
        }

      case ApplyExpr(fun, arg) => ApplyExpr(inferType(fun, subs), inferType(arg, subs))

      case NatDependentLambdaExpr(x, e) =>
        NatDependentLambdaExpr(x, inferType(e, subs))

      case NatDependentApplyExpr(f, x) =>
        NatDependentApplyExpr(inferType(f, subs), x)

      case TypeDependentLambdaExpr(x, e) =>
        TypeDependentLambdaExpr(x, inferType(e, subs))

      case TypeDependentApplyExpr(f, x) =>
        TypeDependentApplyExpr(inferType(f, subs), x)

      case IfThenElseExpr(cond, thenE, elseE) =>
        IfThenElseExpr(inferType(cond, subs), inferType(thenE, subs), inferType(elseE, subs))

      case UnaryOpExpr(op, e) => UnaryOpExpr(op, inferType(e, subs))

      case BinOpExpr(op, lhs, rhs) => BinOpExpr(op, inferType(lhs, subs), inferType(rhs, subs))

      case LiteralExpr(d) => LiteralExpr(d)

      case p: PrimitiveExpr => VisitAndRebuild(p.inferType(subs), GetLengthVisitor(subs))
    }
  }

  def setParamAndInferType(f: Expr,
                                      t: DataType,
                                      subs: SubstitutionMap): Expr = {
    f match {
      case LambdaExpr(x, e) =>
        val newX = IdentifierExpr(newName(), Some(t))
        val newE = apply(e, subs.updated(x, newX))
        LambdaExpr(newX, newE)
      case _ => throw new Exception("This should not happen")
    }
  }

  def setParamsAndInferTypes(f: Expr,
                                        t1: DataType,
                                        t2: DataType,
                                        subs: SubstitutionMap): Expr = {
    f match {
      case LambdaExpr(x, e1) =>
        val newX = IdentifierExpr(newName(), Some(t1))
        e1 match {
          case LambdaExpr(y, e2) =>
            val newY = IdentifierExpr(newName(), Some(t2))
            val newE2 = apply(e2, subs.updated(x, newX).updated(y, newY))
            LambdaExpr(newX, LambdaExpr(newY, newE2))
          case _ => throw new Exception("This should not happen")
        }
      case _ => throw new Exception("This should not happen")
    }
  }

  def setParamsAndInferTypes(f: Expr,
                                        makeDt: NatIdentifier => DataType,
                                        subs: SubstitutionMap): Expr = {
    f match {
      case NatDependentLambdaExpr(i, LambdaExpr(x, e)) =>
        val newX = IdentifierExpr(newName(), Some(makeDt(i)))
        val newE = inferType(e, subs.updated(x, newX))
        NatDependentLambdaExpr(i, LambdaExpr(newX, newE))
      case _ => throw new Exception("This should not happen")
    }
  }

  private case class GetLengthVisitor(substitutionMap: SubstitutionMap) extends VisitAndRebuild.Visitor {


    override def apply(ae: Nat): Nat = ae match {
      case expr@Macros.GetLength(x) => substitutionMap(x).t match {
        case Some(ArrayType(size, _)) => size
        case Some(t) => error(expr.toString, t.toString, expected = "ArrayType(size, _)")
        case None => error(expr.toString, s"Unknown identifier ${expr.x}")
      }
      case _ => ae
    }

    override def apply[T <: DataType](dt: T): T = Type.rebuild(this.apply, dt)
  }
}
