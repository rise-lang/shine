package rise.core

import scala.collection.mutable.ListBuffer

import rise.core.semantics._
import rise.core.types._

/*
Following "Bidirectional Typing", by Jana Dunfield (http://export.arxiv.org/pdf/1908.05839)
The section on polymorphism is especially relevant if we do not want to have type annotations everywhere.
 */

object bidirectional {
  type Trace = ListBuffer[String]

  case class InferenceException(msg: String, trace: Trace) extends Exception {
    override def toString: String =
      s"inference exception: $msg\n${trace.mkString("---- trace ----\n",
        "\n", "\n---------------")}"
  }
  def error(msg: String)(implicit trace: Trace): Nothing = throw InferenceException(msg, trace)

  type TermCtx = Map[Identifier, Type]
  type TypeCtx = Solution // FIXME: types should be TypeIdentifier => Type

  def checkKinded[K <: Kind](typeCtx : TypeCtx, termCtx : TermCtx, expr : K#T, `type` : K#I)(implicit trace : Trace) : TypeCtx =
    expr match {
      case e : Expr => check(typeCtx, termCtx, e, `type`.asInstanceOf[Type])
    }

  def unify(a : Type, b : Type)(implicit trace : Trace) : TypeCtx =
    Constraint.solve(Seq(TypeConstraint(a, b)), Seq())(Flags.ExplicitDependence.Off)

  def check(typeCtx : TypeCtx, termCtx : TermCtx, expr : Expr, `type` : Type)(implicit trace : Trace) : TypeCtx = {
    trace += s"check $typeCtx ; $termCtx |- $expr : ${`type`}"

    expr match {
      case Lambda(x, e) =>
        `type` match {
          case FunType(a, b) => check(typeCtx, termCtx ++ Map(x -> a), e, b)
          case _ => error(s"${`type`} is not a function type")
        }
      case DepLambda(x, e) =>
        `type` match {
          case DepFunType(x, t) => check(typeCtx ++ ???, termCtx, e, t)
          case _ => error(s"${`type`} is not a dependent function type")
        }
      case Literal(d) => unify(d.dataType, `type`)
      case p: Primitive => unify(p.typeScheme, `type`)
      case _ =>
        val t = infer(typeCtx, termCtx, expr)
        unify(t, `type`)
    }
  }

  def infer(typeCtx : TypeCtx, termCtx : TermCtx, expr : Expr)(implicit trace : Trace) : Type = {
    trace += s"infer $typeCtx ; $termCtx |- $expr"

    expr match {
      case x: Identifier =>
        termCtx.get(x) match {
          case Some(t) => t
          case None => error(s"could not find $x in $termCtx")
        }
      case Lambda(x, e) =>
        val xT = TypeIdentifier(freshName("x"))
        val termCtx1 = termCtx ++ Map(x -> xT)
        val eT = infer(typeCtx, termCtx1, e)
        FunType(xT, eT)
      case DepLambda(x, e) => ???
      case App(f, e) =>
        infer(typeCtx, termCtx, f) match {
          case FunType(a, b) =>
            val typeCtx1 = check(typeCtx, termCtx, e, a)
            typeCtx1(b)
          case _ => error(s"$typeCtx ; $termCtx |- $f is not a function type")
        }
      case DepApp(f, x) =>
        infer(typeCtx, termCtx, f) match {
          case DepFunType(xT, eT) =>
            val typeCtx1 = checkKinded(typeCtx, termCtx, x, xT)
            typeCtx1(eT)
          case _ => error(s"$typeCtx ; $termCtx |- $f is not a dependent function type")
        }
      case TypeAnnotation(e, t) =>
        val typeCtx1 = check(typeCtx, termCtx, e, t)
        typeCtx1(t)
      case TypeAssertion(e, t) =>
        val typeCtx1 = check(typeCtx, termCtx, e, t)
        typeCtx1(t)
      case Literal(d) => d.dataType
      case p : Primitive => p.typeScheme
    }
  }

  implicit val trace : Trace = new Trace()
  val x = Identifier(freshName("x"))(TypePlaceholder)
  val example : Expr = App(TypeAnnotation(Lambda(x, x)(TypePlaceholder), FunType(int, int)), Literal(IntData(1)))(TypePlaceholder)
  val example1 : Expr = Lambda(x, x)(TypePlaceholder)
  val example2 : Expr = App(Lambda(x, x)(TypePlaceholder) , Literal(IntData(1)))(TypePlaceholder)
}
