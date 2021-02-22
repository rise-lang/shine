package rise.eqsat

import rise.{core => rc}
import rise.core.{primitives => rcp}
import rise.core.types._
import rise.core.DSL.Type._
import rise.core.DSL.TypeAssertionHelper
import shine.Pipe

import scala.language.implicitConversions
import scala.collection.mutable

object rules {
  case class Environment(visitedES: mutable.Set[ExprSet],
                         visitedE: mutable.Set[Expr])

  // An expression trace that matched a rule
  // TODO: should the trace be a tree instead of a sequence?
  type Match = Seq[Expr]
  type Result = Seq[(Match, ExprSet)]
  type Rule = (Expr, Type) => Result

/* TODO: generic rules?
  type GRule[P, R[_]] = P => R[P]
  type ExprSetResult[T] = Seq[(Match, T)]
  type ExprSetRule = GRule[ExprSet, ExprSetResult]
  type ExprRule = GRule[Expr, elevate.core.RewriteResult]
*/

  private trait IntoResult {
    def withMatch(m: Expr): Result
  }

  private implicit def seqm(r: Result): IntoResult = new IntoResult {
    def withMatch(m: Expr): Result =
      r.map { case (ms, r) => (m +: ms, r) }
  }
  private implicit def resm(r: ExprSet): IntoResult = new IntoResult {
    def withMatch(m: Expr): Result =
      Seq((Seq(m), r))
  }

  private type SubRule = ExprMaybeSet => Result
  private trait ExprMaybeSet {
    def alternatives(): Iterator[Expr]
  }

  private implicit def exprEMS(e: Expr): ExprMaybeSet = new ExprMaybeSet {
    override def alternatives(): Iterator[Expr] = Iterator(e)
  }
  private implicit def setEMS(s: ExprSet): ExprMaybeSet = new ExprMaybeSet {
    override def alternatives(): Iterator[Expr] = s.alternatives.iterator
  }

  private implicit def topRule1(f: Expr => Result): Rule = { case (e, _) => f(e) }
  private implicit def topRule2(f: SubRule): Rule = { case (e, _) => f(e) }
  private implicit def topRule3(f: SubRule): Expr => Result = e => f(e)
  private implicit def subRule(f: ExprMaybeSet => Result): ExprSet => IntoResult = es => f(es)

  private def when(f: Expr => IntoResult): SubRule = { ems =>
    ems.alternatives().flatMap { e =>
      f(e).withMatch(e)
    }.toSeq
  }

  private def whenLambda(f: (Identifier, Type, ExprSet) => IntoResult): SubRule = when {
    case Lambda(x, t, e) => f(x, t, e)
    case _ => Nil
  }

  private def whenApp(f: (ExprSet, ExprSet) => IntoResult): SubRule = when {
    case App(g, e) => f(g, e)
    case _ => Nil
  }

  private def whenDepLambda(f: (Kind#I with Kind.Explicitness, ExprSet) => IntoResult): SubRule =
    when {
      case DepLambda(x, e) => f(x, e)
      case _ => Nil
    }

  private def whenDepApp(f: (ExprSet, Kind#T) => IntoResult): SubRule = when {
    case DepApp(g, e) => f(g, e)
    case _ => Nil
  }

  private def whenNatApp(f: (ExprSet, Nat) => IntoResult): SubRule = when {
    case DepApp(g, e: Nat) => f(g, e)
    case _ => Nil
  }

  private def whenPrim(p: rc.Primitive)(f: => IntoResult): SubRule = when {
    case Primitive(p2) if p == p2 => f
    case _ => Nil
  }

  private def whenAny(rs: SubRule*): SubRule = es =>
    rs.flatMap(r => r(es))

  private def matchIf(b: Boolean)(f: => IntoResult): IntoResult =
    if (b) { f } else { Nil }

  def betaReduction: Rule = whenAny(
    whenApp { case (f, v) =>
      f |> whenLambda { case (x, _, b) =>
        b.substituteIdent(x, v)
      }},
    whenDepApp { case (f, v) =>
      f |> whenDepLambda { case (x, b) =>
        b.substituteTypeIdent(x, v)
      }}
  )

  def etaReduction: Rule =
    whenLambda { case (x1, t1, b) =>
      b |> whenApp { case (f, x2) =>
        matchIf (x2.represents(rc.DSL.identifier(x1.name).toUntypedExpr)) {
          // val f2 = f.filtered(!_.containsIdent(x1.name))
          // if (f2.alternatives.nonEmpty) {
          matchIf (!f.containsIdent(x1.name)) {
            f
          }
        }}}

  private implicit class FunCall(f: ExprSet) {
    def apply(e: ExprSet): ExprSet =
      ExprSet.one(App(f, e), f.t.asInstanceOf[FunType[_, _ <: Type]].outT)
    def apply(n: Nat): ExprSet =
      ExprSet.one(DepApp[NatKind](f, n), f.t.asInstanceOf[DepFunType[_, _ <: Type]].t)
  }

  private def lambda(inT: Type, f: ExprSet => ExprSet): ExprSet = {
    val id = Identifier(rc.freshName("x"))
    val body = f(ExprSet.one(id, inT))
    ExprSet.one(Lambda(id, inT, body), inT ->: body.t)
  }

  def mapFusion: Rule = { case (e, t) =>
    e |> whenApp { case (e1, e2) =>
    e1 |> whenApp { case (map1, f) =>
    e2 |> whenApp { case (e3, arg) =>
    e3 |> whenApp { case (map2, g) => matchIf (
      map1.represents(rcp.map.primitive) &&
      map2.represents(rcp.map.primitive)
    ) {
      (arg.t, t) match {
        case (ArrayType(n, s), ArrayType(_, u)) =>
          ExprSet.init(rcp.map !: (s ->: u) ->: (n`.`s) ->: (n`.`u))(lambda(s, x => f(g(x))))(arg)
        case _ => ???
      }
    }}}}}
  }

  // "mapLastFission"
  def mapFission: Rule = { case (e, t) =>
    e |> whenApp { case (map, e1) =>
    e1 |> whenLambda { case (x, xt, e2) =>
    e2 |> whenApp { case (f, gx) =>
      matchIf (map.represents(rcp.map.primitive)) {
        (t, f.t, xt) match {
          case (FunType(_, ArrayType(n, dt3)), FunType(dt2: DataType, _), dt1: DataType) =>
            val notIdent: Expr => Boolean = {
              case Identifier(_) => false
              case _ => true
            }
            /*
            // TODO: we should filter `f` deeper than one level?
            val f2 = f.filtered(!_.containsIdent(x.name))
            val gx2 = gx.filtered(notIdent)
            matchIf (f2.alternatives.nonEmpty && gx2.alternatives.nonEmpty) {
             */
            matchIf (!f.containsIdent(x.name) && gx.alternatives.exists(notIdent)) {
              lambda(n`.`dt1, input =>
                ExprSet.init(rcp.map !: (dt2 ->: dt3) ->: (n`.`dt2) ->: (n`.`dt3))(f)(
                  ExprSet.init(rcp.map !: (dt1 ->: dt2) ->: (n`.`dt1) ->: (n`.`dt2))(
                    ExprSet.one(Lambda(x, dt1, gx), dt1 ->: dt2))(input)))
            }
          case _ => ???
        }
      }
    }}}
  }

  private def extractSplitOrSlide: ExprSet => Option[rc.DSL.ToBeTyped[rc.Expr]] = { es =>
    es |> whenAny(
      whenNatApp { case (e1, _) =>
        e1 |> whenNatApp { case (slide, _) =>
          matchIf (slide.represents(rcp.slide.primitive)) {
            es
          }}},
      whenNatApp { case (split, _) =>
        matchIf (split.represents(rcp.split.primitive)) {
          es
        }}
    )
    ??? // TODO
  }

  def mapSlideBeforeTranspose: Rule = { case (e, t) =>
    e |> whenApp { case (transpose, e1) =>
    e1 |> whenApp { case (e2, y) =>
    e2 |> whenApp { case (map, s) =>
      val sopt = extractSplitOrSlide(s)
      matchIf (
        transpose.represents(rcp.transpose.primitive) &&
        map.represents(rcp.map.primitive) &&
        sopt.isDefined
      ) {
        (y.t, t) match {
          case (ArrayType(n, ArrayType(m, t)), ArrayType(mo, ArrayType(mi, ArrayType(_, _)))) =>
            val mt = (mi`.`n`.`t) ->: (n`.`mi`.`t)
            ExprSet.init(rcp.map !: mt ->: (mo`.`mi`.`n`.`t) ->: (mo`.`n`.`mi`.`t))(
              ExprSet.init(rcp.transpose !: (mi`.`n`.`t) ->: (n`.`mi`.`t))(
                ExprSet.init(sopt.get !: (m`.`n`.`t) ->: (mo`.`mi`.`n`.`t))(
                  ExprSet.init(rcp.transpose !: (n`.`m`.`t) ->: (m`.`n`.`t))(y))))
          case (_, _) => ???
        }
    }}}}}

  def slideBeforeMapMapF: Rule = { case (e, t) =>
    e |> whenApp { case (e1, e2) =>
      e1 |> whenApp { case (map1, e3) =>
       e3 |> whenApp { case (map2, f) =>
         e2 |> whenApp { case (s, y) =>
           val sopt = extractSplitOrSlide(s)
           matchIf (
             map1.represents(rcp.map.primitive) &&
             map2.represents(rcp.map.primitive) &&
             sopt.isDefined
           ) {
             (y.t, t) match {
               case (ArrayType(n, s), ArrayType(no, ArrayType(ni, t))) =>
                 ExprSet.init(sopt.get !: (n`.`t) ->: (no`.`ni`.`t))(
                   ExprSet.init(rcp.map !: (s ->: t) ->: (n`.`s) ->: (n`.`t))(f)(y))
               case (_, _) => ???
             }
           }
         }}}}
  }

  def removeTransposePair: Rule = { case (e, _) =>
    e |> whenApp { case (trans1, e1) =>
      e1 |> whenApp { case (trans2, x) =>
        matchIf (
          trans1.represents(rcp.transpose.primitive) &&
          trans2.represents(rcp.transpose.primitive)
        ) {
          x
        }
      }}
  }

  def slideBeforeMap: Rule = { case (e, t) =>
    e |> whenApp { case (s, e1) =>
    s |> whenNatApp { case (e2, sp) =>
    e2 |> whenNatApp { case (slide, sz) =>
    e1 |> whenApp { case (e3, y) =>
    e3 |> whenApp { case (map, f) =>
      matchIf (
        slide.represents(rcp.slide.primitive) &&
        map.represents(rcp.map.primitive)
      ) {
        (y.t, t) match {
          case (ArrayType(_, s), ArrayType(n, ArrayType(m, t))) =>
            ExprSet.init(rcp.map !: ((m`.`s) ->: (m`.`t)) ->: (n`.`m`.`s) ->: (n`.`m`.`t))(
              ExprSet.init(rcp.map !: (s ->: t) ->: (m`.`s) ->: (m`.`t))(f))(
              ExprSet.init(rcp.slide !: rc.substitute.typeInType(s, t, slide.t))(sz)(sp)(y)
          )
          case _ => ???
        }
      }
  }}}}}}

  private val mulT = rc.DSL.fun(x => rcp.fst(x) * rcp.snd(x))
  private val sum = rcp.reduce(rcp.add)(rc.DSL.l(0.0f))
  private def dot(n: Nat, dt: DataType, a: ExprSet, b: ExprSet) =
    ExprSet.init(sum !: (n`.`dt) ->: dt)(
      ExprSet.init(rcp.map(mulT) !: (n`.`(dt x dt)) ->: (n`.`dt))(
        ExprSet.init(rcp.zip !: (n`.`dt) ->: (n`.`dt) ->: (n`.`(dt x dt)))(a)(b)))
  def separateDotVH(weights2d: rc.Expr, wV: rc.Expr, wH: rc.Expr): Rule = { case (e, t) =>
    e |> whenApp { case (e1, e2) =>
    e1 |> whenApp { case (e3, init) =>
    e3 |> whenApp { case (reduce, add) =>
    e2 |> whenApp { case (e4, e5) =>
    e4 |> whenApp { case (map, mf) =>
    e5 |> whenApp { case (e6, e7) =>
    e6 |> whenApp { case (zip, e8) =>
    e8 |> whenApp { case (join1, weights) =>
    e7 |> whenApp { case (join2, nbh) =>
      matchIf (
        reduce.represents(rcp.reduce.primitive) &&
        add.represents(rcp.add.primitive) &&
        map.represents(rcp.map.primitive) &&
        zip.represents(rcp.zip.primitive) &&
        join1.represents(rcp.join.primitive) &&
        join2.represents(rcp.join.primitive) &&
        init.represents(rc.DSL.l(0.0f)) &&
        mf.represents(mulT.toUntypedExpr) &&
        weights.represents(weights2d)
      ) {
        nbh.t match {
          case ArrayType(n, ArrayType(m, dt)) =>
            dot(m, dt, ExprSet.init(wH),
              ExprSet.init(rcp.map !: ((n`.`dt) ->: dt) ->: (m`.`n`.`dt) ->: (m`.`dt))(
                lambda(n`.`dt, x => dot(n, dt, ExprSet.init(wV), x)))(
                  ExprSet.init(rcp.transpose !: (n`.`m`.`dt) ->: (m`.`n`.`dt))(nbh))
          )
          case _ => ???
        }
      }
    }}}}}}}}}}
  def separateDotHV(weights2d: rc.Expr, wV: rc.Expr, wH: rc.Expr): Rule = { case (e, t) =>
    e |> whenApp { case (e1, e2) =>
    e1 |> whenApp { case (e3, init) =>
    e3 |> whenApp { case (reduce, add) =>
    e2 |> whenApp { case (e4, e5) =>
    e4 |> whenApp { case (map, mf) =>
    e5 |> whenApp { case (e6, e7) =>
    e6 |> whenApp { case (zip, e8) =>
    e8 |> whenApp { case (join1, weights) =>
    e7 |> whenApp { case (join2, nbh) =>
      matchIf (
        reduce.represents(rcp.reduce.primitive) &&
        add.represents(rcp.add.primitive) &&
        map.represents(rcp.map.primitive) &&
        zip.represents(rcp.zip.primitive) &&
        join1.represents(rcp.join.primitive) &&
        join2.represents(rcp.join.primitive) &&
        init.represents(rc.DSL.l(0.0f)) &&
        mf.represents(mulT.toUntypedExpr) &&
        weights.represents(weights2d)
      ) {
        nbh.t match {
          case ArrayType(n, ArrayType(m, dt)) =>
            dot(n, dt, ExprSet.init(wV),
              ExprSet.init(rcp.map !: ((m`.`dt) ->: dt) ->: (n`.`m`.`dt) ->: (n`.`dt))(
                lambda(m`.`dt, x => dot(m, dt, ExprSet.init(wH), x)))(nbh)
          )
          case _ => ???
        }
      }
    }}}}}}}}}}

  object lowering {
    def reduceSeqUnroll: Rule = { case (e, t) =>
      e |> whenPrim(rcp.reduce.primitive) {
        ExprSet.init(rcp.reduceSeqUnroll !: t)
      }
    }

    def mapSeq: Rule = { case (e, t) =>
      e |> whenPrim(rcp.map.primitive) {
        ExprSet.init(rcp.mapSeq !: t)
      }
    }

    def iterateStream: Rule = { case (e, t) =>
      e |> whenPrim(rcp.map.primitive) {
        ExprSet.init(rcp.iterateStream !: t)
      }
    }

    def toMemAfterMapSeq: Rule = { case (e, t) =>
      e |> whenApp { case (e1, _) =>
        e1 |> whenApp { case (mapSeq, _) =>
          matchIf (mapSeq.represents(rcp.mapSeq.primitive)) {
            ExprSet.init(rcp.toMem !: t ->: t)(ExprSet.one(e, t))
          }
        }}}

    def rotateValues(write: rc.DSL.ToBeTyped[rc.Expr]): Rule = { case (e, t) =>
      e |> whenNatApp { case (e1, sp) =>
       e1 |> whenNatApp { case (slide, sz) =>
         matchIf (slide.represents(rcp.slide.primitive) && sp == (1: Nat)) {
           t match {
             case FunType(ArrayType(_, s), _) =>
               ExprSet.init(rcp.rotateValues(sz) !: (s ->: s) ->: t)(
                 ExprSet.init(write !: s ->: s)
             )
             case _ => ???
           }
         }
       }}
    }
  }
}
