package rise.core

import scala.collection.mutable.ListBuffer
import rise.core.semantics._
import rise.core.types._
import util.PatternMatching

import scala.annotation.tailrec

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

  def unify(a : Type, b : Type)(implicit trace : Trace) : Solution =
    Constraint.solve(Seq(TypeConstraint(a, b)), Seq())(Flags.ExplicitDependence.Off)

  def substitute[K <: Kind](x : K#I, `for` : K#T)(implicit trace : Trace) : Solution = {
    x match {
      case x : TypeIdentifier => `for` match {
        case `for` : Type => Solution.subs(x, `for`)
        case _ => error(s"$x is a TypeIdentifier but ${`for`} is not a Type")
      }
      case x : NatIdentifier => `for` match {
        case `for` : Nat => Solution.subs(x, `for`)
        case _ => error(s"$x is a NatIdentifier but ${`for`} is not a Nat")
      }
      case x : DataTypeIdentifier => `for` match {
        case `for` : DataType => Solution.subs(x, `for`)
        case _ => error(s"$x is a DataTypeIdentifier but ${`for`} is not a DataType")
      }
      case x : AddressSpaceIdentifier => `for` match {
        case `for` : AddressSpace => Solution.subs(x, `for`)
        case _ => error(s"$x is a AddressSpaceIdentifier but ${`for`} is not an AddressSpace")
      }
      case x : NatToNatIdentifier => `for` match {
        case `for` : NatToNat => Solution.subs(x, `for`)
        case _ => error(s"$x is a NatToNatIdentifier but ${`for`} is not a NatToNat")
      }
      case x : NatToDataIdentifier => `for` match {
        case `for` : NatToData => Solution.subs(x, `for`)
        case _ => error(s"$x is a NatToDataIdentifier but ${`for`} is not a NatToData")
      }
      case x : NatCollectionIdentifier => `for` match {
        case `for` : NatCollection => Solution.subs(x, `for`)
        case _ => error(s"$x is a NatCollectionIdentifier but ${`for`} is not a NatCollection")
      }
    }
  }

  type TermCtx = Map[Identifier, Type]

  def check(expr : Expr, `type`: Type) : Solution = check(Map(), expr, `type`)(implicitly(new Trace()))
  @tailrec
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

  def infer(expr : Expr) : Type = infer(Map(), expr)(implicitly(new Trace()))
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

      case DepLambda(x, e) =>
        val eT = infer(termCtx, e)
        x match {
          case x : NatIdentifier => DepFunType[NatKind, Type](x, eT)
          case x : DataTypeIdentifier => DepFunType[DataKind, Type](x, eT)
          case x : AddressSpaceIdentifier => DepFunType[AddressSpaceKind, Type](x, eT)
          case x : NatToNatIdentifier => DepFunType[NatToNatKind, Type](x, eT)
          case x : NatToDataIdentifier => DepFunType[NatToDataKind, Type](x, eT)
          case x : NatCollectionIdentifier => DepFunType[NatCollectionKind, Type](x, eT)
        }

      case App(f, e) =>
        infer(termCtx, f) match {
          case FunType(a, b) =>
            val subs = check(termCtx, e, a)
            subs(b)
          case _ => error(s"$termCtx |- $f is not a function type")
        }

      case DepApp(f, x) =>
        infer(termCtx, f) match {
          case DepFunType(xT, eT) =>
            val subs = substitute(x = xT, `for` = x)
            subs(eT)
          case _ => error(s"$termCtx |- $f is not a dependent function type")
        }

      case TypeAnnotation(e, t) =>
        check(termCtx, e, t)
        t

      case TypeAssertion(e, t) =>
        val subs = check(termCtx, e, t)
        subs(t)

      case Literal(d) => d.dataType

      case p : Primitive => p.typeScheme
    }
  }

  val x = Identifier(freshName("x"))(TypePlaceholder)
  val n2d = NatToDataIdentifier(freshName("x"))
  val n = NatIdentifier(freshName("x"))
  val example0 : Expr = App(TypeAnnotation(Lambda(x, x)(TypePlaceholder), FunType(int, int)), Literal(IntData(1)))(TypePlaceholder)
  val example1 : Expr = Lambda(x, x)(TypePlaceholder)
  val example2 : Expr = App(Lambda(x, x)(TypePlaceholder) , Literal(IntData(1)))(TypePlaceholder)
  val example3 : Expr = DepLambda[NatKind](n, Literal(NatData(n)))(TypePlaceholder)
  val example4 : Expr = DepApp[NatKind](example3, 2)(TypePlaceholder)
  val example5 : Expr = DepApp[TypeKind](example3, bool)(TypePlaceholder)
  val example6 : Expr = DepLambda[NatToDataKind](n2d, DepApp[NatToDataKind](DepLambda[NatToDataKind](n2d, Literal(IntData(1)))(TypePlaceholder), n2d)(TypePlaceholder))(TypePlaceholder)
  val mapId : Expr = App(primitives.map.primitive, Lambda(x, x)(TypePlaceholder))(TypePlaceholder)
  val mapIdArray : Expr = App(mapId, Literal(ArrayData(Seq(BoolData(false)))))(TypePlaceholder)
}
