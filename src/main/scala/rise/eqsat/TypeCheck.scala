package rise.eqsat

object TypeCheck {
  case class Error(msg: String) extends scala.Exception {
    override def toString: String = s"type error: $msg"
  }

  def apply[D](egraph: EGraph[D]) = {
    def assertSameType(t1: Type, t2: Type): Unit =
      if (t1 != t2) {
        egraph.dot().toSVG("/tmp/typeErrorEgraph.svg")
        throw Error(s"$t1 != $t2")
      }

    def unwrapFunType(t: Type): (Type, Type) =
      t.node match {
        case FunType(inT, outT) => (inT, outT)
        case _ => throw Error(s"expected function type, found $t")
      }
    def unwrapNatFunType(t: Type): Type =
      t.node match {
        case NatFunType(t) => t
        case _ => throw Error(s"expected nat function type, found $t")
      }
    def unwrapDataFunType(t: Type): Type =
      t.node match {
        case DataFunType(t) => t
        case _ => throw Error(s"expected data function type, found $t")
      }

    for (eclass <- egraph.classes.values) {
      val t = eclass.t
      for (node <- eclass.nodes) {
        node match {
          case Var(index) =>
            val visited = HashSet[(ENode, EClassId, Int)]()
            def checkVarTy[D](eclass: EClass[D], index: Int, ty: Type): Unit = {
              for ((node, pid) <- eclass.parents) {
                if (!visited.contains((node, pid, index))) {
                  visited += ((node, pid, index))
                  val p = egraph.get(pid)
                  node match {
                    case Lambda(_) =>
                      if (index == 0) {
                        val (inT, _) = unwrapFunType(p.t)
                        assertSameType(inT, ty)
                      } else {
                        checkVarTy(p, index - 1, ty)
                      }
                    case _ => checkVarTy(p, index, ty)
                  }
                }
              }
            }

            if (index < 0) {
              throw Error("negative variable index")
            }
            // this is expensive:
            // checkVarTy(eclass, index, eclass.t)
          case App(f, e) =>
            val (inT, outT) = unwrapFunType(egraph.get(f).t)
            assertSameType(inT, egraph.get(e).t)
            assertSameType(outT, t)
          case Lambda(e) =>
            val (_, outT) = unwrapFunType(t)
            assertSameType(egraph.get(e).t, outT)
          case NatApp(f, x) =>
            val ft = unwrapNatFunType(egraph.get(f).t)
            assertSameType(t, Type.simplifyNats(ft.withNatArgument(x)))
          case DataApp(f, x) =>
            val ft = unwrapDataFunType(egraph.get(f).t)
            assertSameType(t, ft.withDataArgument(x))
          case NatLambda(e) =>
            val ft = unwrapNatFunType(t)
            assertSameType(ft, egraph.get(e).t)
          case DataLambda(e) =>
            val ft = unwrapDataFunType(t)
            assertSameType(ft, egraph.get(e).t)
          case Literal(d) =>
            assertSameType(t, Type.fromNamed(d.dataType))
          case Primitive(_) =>
            // TODO: check p.typeScheme consistency?

          case Composition(f, g) =>
            val (fInT, fOutT) = unwrapFunType(egraph.get(f).t)
            val (gInT, gOutT) = unwrapFunType(egraph.get(g).t)
            assertSameType(fOutT, gInT)
            assertSameType(t, Type(FunType(fInT, gOutT)))
        }
      }
    }
  }
}
