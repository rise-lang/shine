package rise.eqsat

import rise.{core => rc}
import rise.core.{primitives => rcp}
import rise.core.types._
import rise.core.DSL.Type._
import rise.core.DSL.TypeAssertionHelper
import shine.Pipe
import scala.collection.mutable

object rules {
  case class Environment(visitedES: mutable.Set[ExprSet],
                         visitedE: mutable.Set[Expr])

  // a rule matches over a set of expressions,
  // potentially producing equivalent expressions
  type Rule = ExprSet => Unit

  private def whenLambda(f: (Identifier, Type, ExprSet) => Unit): Rule = { es =>
    es.alternatives.iterator.foreach {
      case Lambda(x, t, e) => f(x, t, e)
      case _ => Nil
    }
  }

  private def whenApp(f: (ExprSet, ExprSet) => Unit): Rule = { es =>
    es.alternatives.iterator.foreach {
      case App(g, e) => f(g, e)
      case _ => Nil
    }
  }

  private def whenDepLambda(f: (Kind#I, ExprSet) => Unit): Rule = { es =>
    es.alternatives.iterator.foreach {
      case DepLambda(x, e) => f(x, e)
      case _ => Nil
    }
  }

  private def whenDepApp(f: (ExprSet, Kind#T) => Unit): Rule = { es =>
    es.alternatives.iterator.foreach {
      case DepApp(g, e) => f(g, e)
      case _ => Nil
    }
  }

  private def whenNatApp(f: (ExprSet, Nat) => Unit): Rule = { es =>
    es.alternatives.iterator.foreach {
      case DepApp(g, e: Nat) => f(g, e)
      case _ => Nil
    }
  }
  //def

  def betaReduction(rewritesTo: (ExprSet, ExprSet) => Unit): Rule = { es =>
    es |> whenApp { case (f, v) =>
      f |> whenLambda { case (x, _, b) =>
        rewritesTo(es, b.substituteIdent(x, v))
      }}
    es |> whenDepApp { case (f, v) =>
      f |> whenDepLambda { case (x, b) =>
        rewritesTo(es, b.substituteTypeIdent(x, v))
      }}
  }

  def additiveBetaReduction: Rule = betaReduction { case (a, b) => a.add(b) }
  def destructiveBetaReduction: Rule = betaReduction { case (a, b) => a.replace(b) }

  def etaReduction(rewritesTo: (ExprSet, ExprSet) => Unit): Rule = { es =>
    es |> whenLambda { case (x1, _, b) =>
      b |> whenApp { case (f, x2) =>
        if (x2.represents(rc.DSL.identifier(x1.name).toUntypedExpr)) {
          rewritesTo(es, f.filtered(!_.containsIdent(x1.name)))
        }}}}

  def additiveEtaReduction: Rule = etaReduction { case (a, b) => a.add(b) }
  def destructiveEtaReduction: Rule = etaReduction { case (a, b) => a.replace(b) }

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

  def mapFusion: Rule = { es =>
    es |> whenApp { case (e1, e2) =>
    e1 |> whenApp { case (map1, f) =>
    e2 |> whenApp { case (e3, arg) =>
    e3 |> whenApp { case (map2, g) => if (
      map1.represents(rcp.map.primitive) &&
      map2.represents(rcp.map.primitive)
    ) {
      (arg.t, es.t) match {
        case (ArrayType(n, s), ArrayType(_, u)) => es.add(
          ExprSet.init(rcp.map !: (s ->: u) ->: (n`.`s) ->: (n`.`u))(lambda(s, x => f(g(x))))(arg)
        )
        case _ => ???
      }
    }}}}}
  }

  // "mapLastFission"
  def mapFission: Rule = { es =>
    es |> whenApp { case (map, e1) =>
      e1 |> whenLambda { case (x, xt, e2) =>
        e2 |> whenApp { case (f, gx) =>
          if (map.represents(rcp.map.primitive)) {
            (es.t, f.t, xt) match {
              case (FunType(_, ArrayType(n, dt3)), FunType(dt2: DataType, _), dt1: DataType) =>
                val f2 = f.filtered(!_.containsIdent(x.name))
                val gx2 = gx.filtered {
                  case Identifier(name) if name == x.name => false
                  case _ => true
                }
                es.add(lambda(n`.`dt1, input =>
                  ExprSet.init(rcp.map !: (dt2 ->: dt3) ->: (n`.`dt2) ->: (n`.`dt3))(f2)(
                    ExprSet.init(rcp.map !: (dt1 ->: dt2) ->: (n`.`dt1) ->: (n`.`dt2))(
                      ExprSet.one(Lambda(x, dt1, gx2), dt1 ->: dt2))(input))))
              case _ => ???
            }
          }
        }}}
  }

  private def extractSplitOrSlide(es: ExprSet): Option[rc.DSL.ToBeTyped[rc.Expr]] = {
    import rc.DSL._
    var r: Option[ToBeTyped[rc.Expr]] = None
    es |> whenNatApp { case (e1, sp) =>
      e1 |> whenNatApp { case (slide, sz) =>
        if (slide.represents(rcp.slide.primitive)) {
          r = Some(rcp.slide(sz)(sp))
        }
      }}
    es |> whenNatApp { case (split, n) =>
        if (split.represents(rcp.split.primitive)) {
          r = Some(rcp.split(n))
        }
      }
    r
  }

  def mapSlideBeforeTranspose: Rule = { es =>
    es |> whenApp { case (transpose, e1) =>
    e1 |> whenApp { case (e2, y) =>
    e2 |> whenApp { case (map, s) => if (
      transpose.represents(rcp.transpose.primitive) &&
      map.represents(rcp.map.primitive)
    ) {
      (extractSplitOrSlide(s), y.t, es.t) match {
        case (Some(s2),
        ArrayType(n, ArrayType(m, t)), ArrayType(mo, ArrayType(mi, ArrayType(_, _)))) =>
          val mt = (mi`.`n`.`t) ->: (n`.`mi`.`t)
          es.add(ExprSet.init(rcp.map !: mt ->: (mo`.`mi`.`n`.`t) ->: (mo`.`n`.`mi`.`t))(
            ExprSet.init(rcp.transpose !: (mi`.`n`.`t) ->: (n`.`mi`.`t))(
              ExprSet.init(s2 !: (m`.`n`.`t) ->: (mo`.`mi`.`n`.`t))(
                ExprSet.init(rcp.transpose !: (n`.`m`.`t) ->: (m`.`n`.`t))(y)))))
        case (Some(_), _, _) => ???
        case (None, _, _) => ()
      }
    }}}}}

  def slideBeforeMapMapF: Rule = { es =>
    es |> whenApp { case (e1, e2) =>
      e1 |> whenApp { case (map1, e3) =>
       e3 |> whenApp { case (map2, f) =>
         e2 |> whenApp { case (s, y) =>
           if (
             map1.represents(rcp.map.primitive) &&
             map2.represents(rcp.map.primitive)
           ) {
             (extractSplitOrSlide(s), y.t, es.t) match {
               case (Some(s2), ArrayType(n, s), ArrayType(no, ArrayType(ni, t))) =>
                 es.add(ExprSet.init(s2 !: (n`.`t) ->: (no`.`ni`.`t))(
                   ExprSet.init(rcp.map !: (s ->: t) ->: (n`.`s) ->: (n`.`t))(f)(y)))
               case (Some(_), _, _) => ???
               case (None, _, _) => ()
             }
           }
         }}}}
  }

  def removeTransposePair: Rule = { es =>
    es |> whenApp { case (trans1, e1) =>
      e1 |> whenApp { case (trans2, x) =>
        if (
          trans1.represents(rcp.transpose.primitive) &&
          trans2.represents(rcp.transpose.primitive)
        ) {
          es.add(x)
        }
      }}
  }

  def slideBeforeMap: Rule = { es =>
    es |> whenApp { case (s, e1) =>
    s |> whenNatApp { case (e2, sp) =>
    e2 |> whenNatApp { case (slide, sz) =>
    e1 |> whenApp { case (e3, y) =>
    e3 |> whenApp { case (map, f) =>
      if (
        slide.represents(rcp.slide.primitive) &&
        map.represents(rcp.map.primitive)
      ) {
        (y.t, es.t) match {
          case (ArrayType(_, s), ArrayType(n, ArrayType(m, t))) => es.add(
            ExprSet.init(rcp.map !: ((m`.`s) ->: (m`.`t)) ->: (n`.`m`.`s) ->: (n`.`m`.`t))(
              ExprSet.init(rcp.map !: (s ->: t) ->: (m`.`s) ->: (m`.`t))(f))(
              ExprSet.init(rcp.slide !: rc.substitute.typeInType(s, t, slide.t))(sz)(sp)(y))
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
  def separateDotVH(weights2d: rc.Expr, wV: rc.Expr, wH: rc.Expr): Rule = { es =>
    es |> whenApp { case (e1, e2) =>
    e1 |> whenApp { case (e3, init) =>
    e3 |> whenApp { case (reduce, add) =>
    e2 |> whenApp { case (e4, e5) =>
    e4 |> whenApp { case (map, mf) =>
    e5 |> whenApp { case (e6, e7) =>
    e6 |> whenApp { case (zip, e8) =>
    e8 |> whenApp { case (join1, weights) =>
    e7 |> whenApp { case (join2, nbh) =>
      if (
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
          case ArrayType(n, ArrayType(m, dt)) => es.add(
            dot(m, dt, ExprSet.init(wH),
              ExprSet.init(rcp.map !: ((n`.`dt) ->: dt) ->: (m`.`n`.`dt) ->: (m`.`dt))(
                lambda(n`.`dt, x => dot(n, dt, ExprSet.init(wV), x)))(
                  ExprSet.init(rcp.transpose !: (n`.`m`.`dt) ->: (m`.`n`.`dt))(nbh)))
          )
          case _ => ???
        }
      }
    }}}}}}}}}}
  def separateDotHV(weights2d: rc.Expr, wV: rc.Expr, wH: rc.Expr): Rule = { es =>
    es |> whenApp { case (e1, e2) =>
    e1 |> whenApp { case (e3, init) =>
    e3 |> whenApp { case (reduce, add) =>
    e2 |> whenApp { case (e4, e5) =>
    e4 |> whenApp { case (map, mf) =>
    e5 |> whenApp { case (e6, e7) =>
    e6 |> whenApp { case (zip, e8) =>
    e8 |> whenApp { case (join1, weights) =>
    e7 |> whenApp { case (join2, nbh) =>
      if (
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
          case ArrayType(n, ArrayType(m, dt)) => es.add(
            dot(n, dt, ExprSet.init(wV),
              ExprSet.init(rcp.map !: ((m`.`dt) ->: dt) ->: (n`.`m`.`dt) ->: (n`.`dt))(
                lambda(m`.`dt, x => dot(m, dt, ExprSet.init(wH), x)))(nbh))
          )
          case _ => ???
        }
      }
    }}}}}}}}}}

  object lowering {
    def reduceSeqUnroll: Rule = { es =>
      if (es.represents(rcp.reduce.primitive)) {
        es.add(ExprSet.init(rcp.reduceSeqUnroll !: es.t))
      }
    }

    def mapSeq: Rule = { es =>
      if (es.represents(rcp.map.primitive)) {
        es.add(ExprSet.init(rcp.mapSeq !: es.t))
      }
    }

    def iterateStream: Rule = { es =>
      if (es.represents(rcp.map.primitive)) {
        es.add(ExprSet.init(rcp.iterateStream !: es.t))
      }
    }

    def toMemAfterMapSeq: Rule = { es =>
      es |> whenApp { case (e1, _) =>
        e1 |> whenApp { case (mapSeq, _) =>
          if (mapSeq.represents(rcp.mapSeq.primitive)) {
            es.add(ExprSet.init(rcp.toMem !: es.t ->: es.t)(es))
          }
        }}}

    def rotateValues(write: rc.DSL.ToBeTyped[rc.Expr]): Rule = { es =>
      es |> whenNatApp { case (e1, sp) =>
       e1 |> whenNatApp { case (slide, sz) =>
         if (slide.represents(rcp.slide.primitive) && sp == (1: Nat)) {
           es.t match {
             case FunType(ArrayType(_, s), _) => es.add(
               ExprSet.init(rcp.rotateValues(sz) !: (s ->: s) ->: es.t)(
                 ExprSet.init(write !: s ->: s))
             )
             case _ => ???
           }
         }
       }}
    }
  }
}
