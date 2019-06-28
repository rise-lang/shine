package idealised.SurfaceLanguage.Types

import idealised.SurfaceLanguage.DSL.Macros
import idealised.SurfaceLanguage._

import scala.language.existentials

class TypeInferenceException(expr: String, msg: String)
  extends TypeException(s"Failed to infer type for `$expr'. $msg.")

//noinspection TypeAnnotation
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

      case LambdaExpr(param, body, _) =>
        inferType(param, subs) match {
          case newParam: IdentifierExpr =>
            lambdaWithType(newParam, inferType(body, subs))
          case _ => throw new Exception("")
        }

      case ApplyExpr(fun, arg, _) =>
        val f = inferType(fun, subs)
        val a = inferType(arg, subs)
        (f.t: @unchecked) match {
          case Some(FunctionType(_, outT)) => ApplyExpr(f, a, Some(outT))
        }

      case NatDependentLambdaExpr(x, body, _) =>
        natDepLambdaWithType(x, inferType(body, subs))

      case NatDependentApplyExpr(fun, x, _) =>
        val f = inferType(fun, subs)
        (f.t: @unchecked) match {
          case Some(DependentFunctionType(_: NatIdentifier, bodyT)) =>
            NatDependentApplyExpr(f, x, Some(bodyT))
        }

      case TypeDependentLambdaExpr(x, body, _) =>
        val e = inferType(body, subs)
        TypeDependentLambdaExpr(x, e, Some(TypeDependentFunctionType(x, e.t.get)))

      case TypeDependentApplyExpr(fun, x, _) =>
        val f = inferType(fun, subs)
        (f.t: @unchecked) match {
          case Some(DependentFunctionType(_: DataTypeIdentifier, bodyT)) =>
            TypeDependentApplyExpr(f, x, Some(bodyT))
        }

      case LetNat(binder, definition, body, _) =>
        val defn = inferType(definition, subs)
        defn.t match {
          case None => error(defn.toString, "Cannot infer definition type in LetNat")
          case Some(_) => //TODO: Are there rules on which types should be allowed here?
        }

        val bodyExpr = inferType(body, subs)
        bodyExpr.t match {
          case Some(t) =>  LetNat(binder, defn, bodyExpr, Some(t))
        }

      case IfThenElseExpr(cond, thenE, elseE, _) =>
        val c = inferType(cond, subs)
        val te = inferType(thenE, subs)
        val ee = inferType(elseE, subs)
        (te.t, ee.t) match {
          case (Some(tT), Some(eT)) if tT == eT =>
            IfThenElseExpr(c, te, ee, Some(tT))
        }

      case BinOpExpr(op, lhs, rhs, _) =>
        val l = inferType(lhs, subs)
        val r = inferType(rhs, subs)
        (l.t, r.t) match {
          case (Some(lT), Some(rT)) if lT == rT =>
            BinOpExpr(op, l, r, Some(lT))
        }

      case UnaryOpExpr(op, expr, _) =>
        val e = inferType(expr, subs)
        (e.t: @unchecked) match {
          case Some(eT) => UnaryOpExpr(op, e, Some(eT))
        }

      case LiteralExpr(d, _) => LiteralExpr(d, Some(d.dataType))

      case NatExpr(n) => NatExpr(n)

      case p: PrimitiveExpr => VisitAndRebuild(p.inferType(subs), GetLengthVisitor(subs))
    }
  }

  def lambdaWithType(x: IdentifierExpr, e: Expr): Expr = {
    LambdaExpr(x, e, Some(FunctionType(x.t.get, e.t.get)))
  }

  def natDepLambdaWithType(x: NatIdentifier, e: Expr): Expr = {
    NatDependentLambdaExpr(x, e, Some(NatDependentFunctionType(x, e.t.get)))
  }

  def setParamAndInferType(f: Expr,
                                      t: DataType,
                                      subs: SubstitutionMap): Expr = {
    f match {
      case LambdaExpr(x, e, _) =>
        val newX = IdentifierExpr(newName(), Some(t))
        val newE = apply(e, subs.updated(x, newX))
        lambdaWithType(newX, newE)
      case _ => throw new Exception("This should not happen")
    }
  }

  def setParamsAndInferTypes(f: Expr,
                                        t1: DataType,
                                        t2: DataType,
                                        subs: SubstitutionMap): Expr = {
    f match {
      case LambdaExpr(x, e1, _) =>
        val newX = IdentifierExpr(newName(), Some(t1))
        e1 match {
          case LambdaExpr(y, e2, _) =>
            val newY = IdentifierExpr(newName(), Some(t2))
            val newE2 = apply(e2, subs.updated(x, newX).updated(y, newY))
            lambdaWithType(newX, lambdaWithType(newY, newE2))
          case _ => throw new Exception("This should not happen")
        }
      case _ => throw new Exception("This should not happen")
    }
  }

  def setParamsAndInferTypes(f: Expr,
                                        makeDt: NatIdentifier => DataType,
                                        subs: SubstitutionMap): Expr = {
    f match {
      case NatDependentLambdaExpr(i, LambdaExpr(x, e, _), _) =>
        val newX = IdentifierExpr(newName(), Some(makeDt(i)))
        val newE = inferType(e, subs.updated(x, newX))
        natDepLambdaWithType(i, lambdaWithType(newX, newE))
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

    override def apply[T <: Type](dt: T): T = Type.rebuild(this.apply, dt)
  }
}
