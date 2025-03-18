package rise.eqsat

object TypeCheck {
  case class Error(msg: String) extends scala.Exception {
    override def toString: String = s"type error: $msg"
  }

  def apply(egraph: EGraph) = {
    def assertSameType(t1: TypeId, t2: TypeId): Unit =
      if (t1 != t2) {
        // egraph.dot().toSVG("/tmp/typeErrorEgraph.svg")
        val humanReadable = ExprWithHashCons.`type`(egraph) _
        throw Error(s"$t1 != $t2 (${humanReadable(t1)} != ${humanReadable(t2)})")
      }

    def unwrapNotDataTypeId(t: TypeId): NotDataTypeId =
      t match {
        case _: DataTypeId => throw Error("did not expect data type")
        case i: NotDataTypeId => i
      }
    def unwrapFunType(t: TypeId): (TypeId, TypeId) =
      egraph(unwrapNotDataTypeId(t)) match {
        case FunType(inT, outT) => (inT, outT)
        case node => throw Error(s"expected function type, found $node")
      }
    def unwrapNatFunType(t: TypeId): TypeId =
      egraph(unwrapNotDataTypeId(t)) match {
        case NatFunType(t) => t
        case node => throw Error(s"expected nat function type, found $node")
      }
    def unwrapDataFunType(t: TypeId): TypeId =
      egraph(unwrapNotDataTypeId(t)) match {
        case DataFunType(t) => t
        case node => throw Error(s"expected data function type, found $node")
      }
    def unwrapAddrFunType(t: TypeId): TypeId =
      egraph(unwrapNotDataTypeId(t)) match {
        case AddrFunType(t) => t
        case node => throw Error(s"expected address function type, found $node")
      }

    for (eclass <- egraph.classes.values) {
      val t = eclass.t
      for (node <- eclass.nodes) {
        node match {
          case Var(index) =>
            val visited = HashSet[(ENode, EClassId, Int)]()
            def checkVarTy[D](eclass: EClass, index: Int, ty: TypeId): Unit = {
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
            assertSameType(t, NodeSubs.Type.withNatArgument(egraph, ft, x))
          case DataApp(f, x) =>
            val ft = unwrapDataFunType(egraph.get(f).t)
            assertSameType(t, NodeSubs.Type.withDataArgument(egraph, ft, x))
          case AddrApp(f, x) =>
            val ft = unwrapAddrFunType(egraph.get(f).t)
            assertSameType(t, ft) // identity: NodeSubs.Address.withAddrArgument(egraph, ft, x)
          case NatLambda(e) =>
            val ft = unwrapNatFunType(t)
            assertSameType(ft, egraph.get(e).t)
          case DataLambda(e) =>
            val ft = unwrapDataFunType(t)
            assertSameType(ft, egraph.get(e).t)
          case AddrLambda(e) =>
            val ft = unwrapAddrFunType(t)
            assertSameType(ft, egraph.get(e).t)
          case Literal(d) =>
            // TODO: more efficient egraph.addTypeFromNamed?
            assertSameType(t, egraph.addType(Type.fromNamed(d.dataType)))
          case NatLiteral(_) =>
            ()
            // TODO: assertSameType(t, egraph.addType(NatType))
          case IndexLiteral(_, _) =>
            ()
            // TODO: assertSameType(t, egraph.addType(IndexType(?)))
          case Primitive(_) =>
            // TODO: check p.typeScheme consistency?

          case Composition(f, g) =>
            val (fInT, fOutT) = unwrapFunType(egraph.get(f).t)
            val (gInT, gOutT) = unwrapFunType(egraph.get(g).t)
            assertSameType(fOutT, gInT)
            assertSameType(t, egraph.add(FunType(fInT, gOutT)))
        }
      }
    }
  }
}
