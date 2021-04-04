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

  def unify(a : Type, b : Type)(implicit trace : Trace) : Solution =
    Constraint.solve(Seq(TypeConstraint(a, b)), Seq())(Flags.ExplicitDependence.Off)

  def checkKind[K <: Kind](termCtx : TermCtx, expr : K#T, `type` : K#I)(implicit trace : Trace) : Solution = {
    `type` match {
      case t : TypeIdentifier => Solution.subs(`type`.asInstanceOf[Type], t)
      case t : Nat => Solution.subs(`type`.asInstanceOf[NatIdentifier], t)
    }
  }

  def check(termCtx : TermCtx, expr : Expr, `type` : Type)(implicit trace : Trace) : Solution = {
    trace += s"check $termCtx |- $expr : ${`type`}"

    expr match {
      case Lambda(x, e) =>
        `type` match {
          case FunType(a, b) => check(termCtx ++ Map(x -> a), e, b)
          case _ => error(s"${`type`} is not a function type")
        }
      case DepLambda(x, e) =>
        `type` match {
          case DepFunType(x, t) => check(termCtx, e, t)
          case _ => error(s"${`type`} is not a dependent function type")
        }
      case Literal(d) => unify(d.dataType, `type`)
      case p: Primitive => unify(p.typeScheme, `type`)
      case _ => unify(infer(termCtx, expr), `type`)
    }
  }

  def infer(termCtx : TermCtx, expr : Expr)(implicit trace : Trace) : Type = {
    trace += s"infer $termCtx |- $expr"

    expr match {
      case x: Identifier =>
        termCtx.get(x) match {
          case Some(t) => t
          case None => error(s"could not find $x in $termCtx")
        }
      case Lambda(x, e) =>
        val xT = TypeIdentifier(freshName("x"))
        val eT = infer(termCtx ++ Map(x -> xT), e)
        FunType(xT, eT)
      case DepLambda(x, e) => x match {
        case x : NatIdentifier =>
          val eT = infer(termCtx, e)
          DepFunType[NatKind, Type](x, eT)
      }
      case App(f, e) =>
        infer(termCtx, f) match {
          case FunType(a, b) =>
            val typeCtx1 = check(termCtx, e, a)
            typeCtx1(b)
          case _ => error(s"$termCtx |- $f is not a function type")
        }
      case DepApp(f, x) =>
        infer(termCtx, f) match {
          case DepFunType(xT, eT) =>
            val typeCtx1 = checkKind(termCtx, x, xT)
            typeCtx1(eT)
          case _ => error(s"$termCtx |- $f is not a dependent function type")
        }
      case TypeAnnotation(e, t) =>
        val typeCtx1 = check(termCtx, e, t)
        typeCtx1(t)
      case TypeAssertion(e, t) =>
        val typeCtx1 = check(termCtx, e, t)
        typeCtx1(t)
      case Literal(d) => d.dataType
      case p : Primitive => p.typeScheme
    }
  }

  implicit val trace : Trace = new Trace()
  val x = Identifier(freshName("x"))(TypePlaceholder)
  val n = NatIdentifier(freshName("x"))
  val example0 : Expr = App(TypeAnnotation(Lambda(x, x)(TypePlaceholder), FunType(int, int)), Literal(IntData(1)))(TypePlaceholder)
  val example1 : Expr = Lambda(x, x)(TypePlaceholder)
  val example2 : Expr = App(Lambda(x, x)(TypePlaceholder) , Literal(IntData(1)))(TypePlaceholder)
  val example3 : Expr = DepLambda[NatKind](n, Literal(NatData(n)))(TypePlaceholder)
  val example4 : Expr = DepApp[NatKind](example3, 2)(TypePlaceholder)
  val example5 : Expr = DepApp[TypeKind](example3, bool)(TypePlaceholder)
}
