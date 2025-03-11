package rise.eqsat

import rise.eqsat.ematching.MNode

trait RewriteDirected {
  def name: String
  def requiredAnalyses(): (Set[Analysis], Set[TypeAnalysis])
  def search(egraph: EGraph): Vec[(RewriteDirected.Match, RewriteDirected.Applier)]
}

object RewriteDirected {
  sealed trait Match
  case class MatchNode(mnode: Node[Match, Unit, Unit, Unit], enode: ENode, id: EClassId) extends Match
  case class MatchClass(id: EClassId) extends Match

  // returns whether the right-hand side creates a new alternative,
  // and whether the graph changed (new union)
  type Applier = () => (Boolean, Boolean)

  // returns amount of nodes removed
  def greedyRemoval(egraph: EGraph,
                    withAlternative: Vec[RewriteDirected.Match]): Int = {
    // avoid deleting overlapping matches
    val attemptedDeletion = HashSet.empty[(EClassId, ENode)]

    def overlapsAttemptedDeletion(m: Match): Boolean = {
      m match {
        case MatchNode(mnode, enode, id) =>
          attemptedDeletion(id -> enode) ||
            mnode.children().exists(overlapsAttemptedDeletion)
        case MatchClass(_) => false
      }
    }

    def extendAttemptedDeletion(m: Match): Unit = {
      m match {
        case MatchNode(mnode, enode, id) =>
          attemptedDeletion += id -> enode
          mnode.children().foreach(extendAttemptedDeletion)
        case MatchClass(_) => ()
      }
    }

    // returns whether there exists shared non-matching possibilities
    def sharesNonMatching(m: Match): Boolean = {
      m match {
        case MatchNode(mnode, _, id) =>
          mnode.children().exists(sharesNonMatching) ||
            (egraph.get(id).parents.size > 1)
        case MatchClass(_) => false
      }
    }

    val toRemove = HashMap.empty[EClassId, HashSet[ENode]]
    var removed = 0
    def remove(id: EClassId, enode: ENode): Unit = {
      if (toRemove.getOrElseUpdate(id, HashSet.empty).add(enode)) {
        removed += 1
      }
    }

    def tryToRemove(m: Match, atRoot: Boolean): Unit = {
      m match {
        case MatchNode(mnode, enode, id) =>
          val eclass = egraph.get(id)
          val safeToProceed = atRoot || (eclass.parents.size == 1)
          if (safeToProceed) {
            // following code is inefficient, but good enough for small matches
            val snms = mnode.children().map(sharesNonMatching).toSeq
            mnode.children().zipWithIndex.foreach { case (c, i) =>
              val safeToRec = snms.zipWithIndex.forall { case (x, j) => i == j || !x }
              if (safeToRec) { tryToRemove(c, atRoot = false) }
            }

            val safeToRemove = snms.forall(x => !x)
            if (safeToRemove) { remove(eclass.id, enode) }
          }
        case MatchClass(_) => ()
      }
    }

    withAlternative.foreach { m =>
      val removedBackup = removed
      if (!overlapsAttemptedDeletion(m)) {
        tryToRemove(m, atRoot = true)
      }
      if (removed > removedBackup) {
        extendAttemptedDeletion(m)
      }
    }

    val toRemoveCano = HashSet.empty[(EClassId, ENode)]
    val parentsToRemove = HashMap.empty[EClassId, HashSet[(EClassId, ENode)]]
    toRemove.foreach { case (id, enodesToRemove) =>
      val eclass = egraph.getMut(id)
      val prevSize = eclass.nodes.size
      eclass.nodes.filterInPlace(!enodesToRemove(_))
      assert(eclass.nodes.size + enodesToRemove.size == prevSize)
      enodesToRemove.foreach { enode =>
        val cano = (eclass.id, enode.mapChildren(egraph.findMut))
        toRemoveCano += cano
        enode.children().foreach { c =>
          parentsToRemove.getOrElseUpdate(c, HashSet.empty) += cano
        }
      }
    }

    egraph.memo.filterInPlace { case ((n, _), id) =>
      !toRemoveCano((egraph.findMut(id), n.mapChildren(egraph.findMut)))
    }

    parentsToRemove.foreach { case (id, ps) =>
      egraph.get(id).parents.filterInPlace { case (n, id) =>
        !ps.contains((egraph.findMut(id), n.mapChildren(egraph.findMut)))
      }
    }

    egraph.clean = false
    removed
  }

  private def foreachClassByMatch(egraph: EGraph, mnode: MNode)(f: EClassId => Unit): Unit =
    egraph.classesByMatch.get(mnode.matchHash()).foreach(_.foreach(f))

  private def foreachMatchingNode(eclass: EClass, mnode: MNode)
                                 (f: ENode => Unit): Unit =
    ematching.forEachMatchingNode(eclass, mnode, f)

  // FIXME: redundancy with e-matching and extended pattern search
  // TODO: algorithm could be generalized for any rewrite rule

  object Eta extends RewriteDirected {
    override def name: String = "eta"

    override def requiredAnalyses(): (Set[Analysis], Set[TypeAnalysis]) =
      (Set(FreeAnalysis, BENF.extractAnalysis), Set())

    override def search(egraph: EGraph): Vec[(RewriteDirected.Match, RewriteDirected.Applier)] = {
      assert(egraph.clean)
      val res = Vec.empty[(RewriteDirected.Match, RewriteDirected.Applier)]

      val appNode = App((), ())
      val lamNode = Lambda(())
      val freeOf = egraph.getAnalysis(FreeAnalysis)
      val smallestOf = egraph.getAnalysis(BENF.extractAnalysis)

      foreachClassByMatch(egraph, lamNode){ id =>
        val lamEClass = egraph.get(id)
        foreachMatchingNode(lamEClass, lamNode){ matchingLam =>
          val matchingLamBody = egraph.get(matchingLam.children().next())
          foreachMatchingNode(matchingLamBody, appNode) { matchingApp =>
            val matchingAppChildren = matchingApp.children().toSeq
            val matchingAppFun = egraph.get(matchingAppChildren(0))
            if (!freeOf(matchingAppFun.id).free.contains(0)) {
              val matchingAppArg = egraph.get(matchingAppChildren(1))
              foreachMatchingNode(matchingAppArg, Var(0)) { matchingVar0 =>
                val m = MatchNode(Lambda(
                  MatchNode(App(
                    MatchClass(matchingAppFun.id),
                    MatchNode(Var(0), matchingVar0, matchingAppArg.id)),
                    matchingApp,
                    matchingLamBody.id)),
                  matchingLam,
                  lamEClass.id)
                res += ((m, () => {
                  val extract = smallestOf(matchingAppFun.id)._1
                  val shifted = extract.shifted(egraph, (-1, 0, 0, 0), (1, 0, 0, 0))
                  val (resultNode, resultId) = egraph.addExpr2(shifted)
                  (matchingLam != resultNode, egraph.union(lamEClass.id, resultId)._2)
                }))
              }
            }
          }
        }
      }

      res
    }
  }

  // TODO: parallel bottom-up application?
  object BetaExtract extends RewriteDirected {
    override def name: String = "beta-extract"

    override def requiredAnalyses(): (Set[Analysis], Set[TypeAnalysis]) =
      (Set(BENF.extractAnalysis), Set())

    override def search(egraph: EGraph): Vec[(RewriteDirected.Match, RewriteDirected.Applier)] = {
      assert(egraph.clean)
      val res = Vec.empty[(RewriteDirected.Match, RewriteDirected.Applier)]

      val appNode = App((), ())
      val lamNode = Lambda(())
      val smallestOf = egraph.getAnalysis(BENF.extractAnalysis)

      foreachClassByMatch(egraph, appNode){ id =>
        val appEClass = egraph.get(id)
        foreachMatchingNode(appEClass, appNode){ matchingApp =>
          val matchingAppChildren = matchingApp.children().toSeq
          val matchingAppFun = egraph.get(matchingAppChildren(0))
          foreachMatchingNode(matchingAppFun, lamNode){ matchingLam =>
            val body = matchingLam.children().next()
            val subs = matchingAppChildren(1)
            val m = MatchNode(App(
              MatchNode(Lambda(
                MatchClass(body)
              ), matchingLam, matchingAppFun.id),
              MatchClass(subs)),
              matchingApp, appEClass.id)
            res += ((m, () => {
              val bodyEx = smallestOf(body)._1
              val subsEx = smallestOf(subs)._1
              // FIXME: next two lines are expensive
              val expr = bodyEx.withArgument(egraph, subsEx)
              val (resultNode, resultId) = egraph.addExpr2(expr)
              (matchingApp.mapChildren(egraph.find) != resultNode.mapChildren(egraph.find),
                egraph.union(appEClass.id, resultId)._2)
            }))
          }
        }
      }

      res
    }
  }

  // TODO: parallel bottom-up application?
  object BetaNatExtract extends RewriteDirected {
    override def name: String = "beta-nat-extract"

    override def requiredAnalyses(): (Set[Analysis], Set[TypeAnalysis]) =
      (Set(BENF.extractAnalysis), Set())

    override def search(egraph: EGraph): Vec[(RewriteDirected.Match, RewriteDirected.Applier)] = {
      assert(egraph.clean)
      val res = Vec.empty[(RewriteDirected.Match, RewriteDirected.Applier)]

      val nAppNode = NatApp((), ())
      val nLamNode = NatLambda(())
      val smallestOf = egraph.getAnalysis(BENF.extractAnalysis)

      foreachClassByMatch(egraph, nAppNode){ id =>
        val nAppEClass = egraph.get(id)
        foreachMatchingNode(nAppEClass, nAppNode){ matchingNApp =>
          val matchingNAppFun = egraph.get(matchingNApp.children().next())
          foreachMatchingNode(matchingNAppFun, nLamNode){ matchingNLam =>
            val body = matchingNLam.children().next()
            val subs = matchingNApp.nats().next()
            val m = MatchNode(NatApp(
              MatchNode(NatLambda(
                MatchClass(body)
              ), matchingNLam, matchingNAppFun.id),
              ()),
              matchingNApp, nAppEClass.id)
            res += ((m, () => {
              val bodyEx = smallestOf(body)._1
              val expr = bodyEx.withNatArgument(egraph, subs)
              val (resultNode, resultId) = egraph.addExpr2(expr)
              (matchingNApp.mapChildren(egraph.find) != resultNode.mapChildren(egraph.find),
                egraph.union(nAppEClass.id, resultId)._2)
            }))
          }
        }
      }

      res
    }
  }

  object CompositionIntro extends RewriteDirected {
    override def name: String = "composition-intro"

    override def requiredAnalyses(): (Set[Analysis], Set[TypeAnalysis]) =
      (Set(), Set())

    override def search(egraph: EGraph): Vec[(RewriteDirected.Match, RewriteDirected.Applier)] = {
      assert(egraph.clean)
      val res = Vec.empty[(RewriteDirected.Match, RewriteDirected.Applier)]

      val appNode = App((), ())

      foreachClassByMatch(egraph, appNode){ id =>
        val appEClass = egraph.get(id)
        foreachMatchingNode(appEClass, appNode){ matchingApp =>
          val matchingAppChildren = matchingApp.children().toSeq
          val matchingAppFun = egraph.get(matchingAppChildren(0))
          val matchingAppArg = egraph.get(matchingAppChildren(1))
          foreachMatchingNode(matchingAppArg, appNode){ matchingApp2 =>
            val matchingApp2Children = matchingApp2.children().toSeq
            val matchingApp2Fun = egraph.get(matchingApp2Children(0))
            val matchingApp2Arg = egraph.get(matchingApp2Children(1))

            val f = matchingAppFun
            val g = matchingApp2Fun
            val x = matchingApp2Arg

            egraph(f.t) match {
              case FunType(dt2: DataTypeId, dt3: DataTypeId) =>
                egraph(g.t) match {
                  case FunType(dt1: DataTypeId, dt2_ : DataTypeId) =>
                    assert(dt2 == dt2_)

                    val m = MatchNode(App(
                      MatchClass(matchingAppFun.id),
                      MatchNode(App(
                        MatchClass(matchingApp2Fun.id),
                        MatchClass(matchingApp2Arg.id)
                      ),
                      matchingApp2, matchingAppArg.id)),
                      matchingApp, appEClass.id)
                    res += ((m, () => {
                      val compGF = egraph.add(Composition(g.id, f.id), egraph.add(FunType(dt1, dt3)))
                      val resultNode = App(compGF, x.id)
                      val resultId = egraph.add(resultNode, dt3)
                      (matchingApp.mapChildren(egraph.find) != resultNode.mapChildren(egraph.find),
                        egraph.union(appEClass.id, resultId)._2)
                    }))
                  case _ =>
                }
              case _ =>
            }
          }
        }
      }

      res
    }
  }

  object CompositionAssoc2 extends RewriteDirected {
    override def name: String = "composition-assoc-2"

    override def requiredAnalyses(): (Set[Analysis], Set[TypeAnalysis]) =
      (Set(), Set())

    override def search(egraph: EGraph): Vec[(RewriteDirected.Match, RewriteDirected.Applier)] = {
      assert(egraph.clean)
      val res = Vec.empty[(RewriteDirected.Match, RewriteDirected.Applier)]

      val compNode = Composition((), ())

      foreachClassByMatch(egraph, compNode){ id =>
        val compEClass = egraph.get(id)
        foreachMatchingNode(compEClass, compNode){ matchingComp =>
          val matchingCompChildren = matchingComp.children().toSeq
          val matchingComp_1 = egraph.get(matchingCompChildren(0))
          val matchingComp_2 = egraph.get(matchingCompChildren(1))
          foreachMatchingNode(matchingComp_2, compNode){ matchingComp2 =>
            val matchingComp2Children = matchingComp2.children().toSeq
            val matchingComp2_1 = egraph.get(matchingComp2Children(0))
            val matchingComp2_2 = egraph.get(matchingComp2Children(1))

            val f = matchingComp_1
            val g = matchingComp2_1
            val h = matchingComp2_2

            egraph(f.t) match {
              case FunType(t1, t2) =>
                egraph(g.t) match {
                  case FunType(_, t3) =>
                    egraph(h.t) match {
                      case FunType(_, t4) =>
                        val m = MatchNode(Composition(
                          MatchClass(matchingComp_1.id),
                          MatchNode(Composition(
                            MatchClass(matchingComp2_1.id),
                            MatchClass(matchingComp2_2.id)
                          ),
                          matchingComp2, matchingComp_2.id)),
                          matchingComp, compEClass.id)
                        res += ((m, (() => {
                          val compFG = egraph.add(Composition(f.id, g.id), egraph.add(FunType(t1, t3)))
                          val resultNode = Composition(compFG, h.id)
                          val resultId = egraph.add(resultNode, egraph.add(FunType(t1, t4)))
                          (matchingComp.mapChildren(egraph.find) != resultNode.mapChildren(egraph.find),
                            egraph.union(compEClass.id, resultId)._2)
                        })))
                      case _ => ()
                    }
                  case _ => ()
                }
              case _ => ()
            }
          }
        }
      }

      res
    }
  }
}
