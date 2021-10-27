package rise.eqsat

object Analyser {
  // FIXME: this is highly similar to the other Analysis trait and CostFunction trait
  // TODO: think about a proper interface, maybe there are more optimal analyses algorithms
  //       depending on the analysis properties.
  trait Analysis[Data] {
    def make(enode: ENode, t: TypeId, egraph: EGraph, analysisOf: EClassId => Data): Data

    // `a` and `b` can be mutated and returned
    def merge(a: Data, b: Data): Data

    def update(existing: Data, computed: Data): Data
  }

  def init[D](egraph: EGraph, costFunction: CostFunction[D]): Analyser[D] = {
    init(egraph, new Analysis[D] {
      override def make(enode: ENode, t: TypeId, egraph: EGraph, analysisOf: EClassId => D): D =
        costFunction.cost(enode, analysisOf)

      override def merge(a: D, b: D): D =
        costFunction.ordering.min(a, b)

      override def update(existing: D, computed: D): D = {
        assert(costFunction.ordering.gteq(existing, computed))
        computed
      }
    })
  }

  def init[D](egraph: EGraph, analysis: Analysis[D],
              data: HashMap[EClassId, D] = HashMap.empty[EClassId, D]): Analyser[D] = {
    assert(egraph.clean)
    val e = new Analyser(analysis, data, egraph)
    e.run()
    e
  }
}

class Analyser[Data](val analysis: Analyser.Analysis[Data],
                     val data: HashMap[EClassId, Data],
                     val egraph: EGraph) {
  def analysisOf(eclass: EClassId): Data =
    data(egraph.find(eclass))

  private def run(): Unit = {
    val analysisPending = HashSetQueuePop.empty[EClassId]

    egraph.classes.values.foreach { eclass =>
      analysisPending += eclass.id
    }

    while (analysisPending.nonEmpty) {
      val id = analysisPending.pop()
      val cid = egraph.findMut(id)
      assert(cid == id)

      val eclass = egraph.classes(cid)
      val didSomething = makePass(eclass)
      if (didSomething) {
        analysisPending ++= eclass.parents.map(p => egraph.findMut(p._2))
      }
    }

    for (eclass <- egraph.classes.values) {
      assert(data.contains(eclass.id))
    }
  }

  private def makePass(eclass: EClass): Boolean = {
    val existingData = data.get(eclass.id)
    val computedData = eclass.nodes.flatMap(n => analyseNode(n, eclass.t))
      .reduceOption(analysis.merge)
    (existingData, computedData) match {
      case (None, Some(newData)) =>
        data += eclass.id -> newData
        true
      case (Some(oldData), Some(newData)) =>
        val updated = analysis.update(oldData, newData)
        val changed = oldData != updated
        if (changed) {
          data += eclass.id -> updated
        }
        changed
      case (_, None) =>
        false
    }
  }


  private def analyseNode(node: ENode, t: TypeId): Option[Data] = {
    val hasData = node.children().forall(id => data.contains(egraph.find(id)))
    if (hasData) {
      Some(analysis.make(node, t, egraph, analysisOf))
    } else {
      None
    }
  }
}

case class CountProgramsUpToSize(limit: Int) extends Analyser.Analysis[HashMap[Int, Long]] {
  override def make(enode: ENode, t: TypeId, egraph: EGraph,
                    analysisOf: EClassId => HashMap[Int, Long]): HashMap[Int, Long] = {
    val counts = HashMap.empty[Int, Long]
    val childrenCounts = enode.children().map(analysisOf).toSeq

    def rec(remaining: Seq[HashMap[Int, Long]], size: Int, count: Long): Unit = {
      if (size > limit) {
        return
      }
      remaining match {
        case Nil =>
          val total = counts.getOrElse(size, 0L) + count
          counts += (size -> total)
        case childCounts +: rest =>
          childCounts.foreach { case (s, c) =>
            rec(rest, size + s, count * c)
          }
      }
    }

    rec(childrenCounts, 1, 1)
    counts
  }

  override def merge(a: HashMap[Int, Long], b: HashMap[Int, Long]): HashMap[Int, Long] = {
    b.foreach { case (size, count) =>
      val total = a.getOrElse(size, 0L) + count
      a += size -> total
    }
    a
  }

  override def update(existing: HashMap[Int, Long], computed: HashMap[Int, Long]): HashMap[Int, Long] =
    computed
}

case class AvoidCompositionAssoc1ExtractData[Cost](
  best: (Int, Cost, ExprWithHashCons),
  bestNoComp: Option[(Int, Cost, ExprWithHashCons)])

case class AvoidCompositionAssoc1Extract[Cost](cf: CostFunction[Cost])
  extends Analyser.Analysis[AvoidCompositionAssoc1ExtractData[Cost]] {

  override def make(enode: ENode, t: TypeId, egraph: EGraph,
                    analysisOf: EClassId => AvoidCompositionAssoc1ExtractData[Cost]
                   ): AvoidCompositionAssoc1ExtractData[Cost] = {
    val childrenAnalysis = enode.children().map(c => c -> analysisOf(c)).toMap
    enode match {
      case Composition(f, g) =>
        // g may be a Composition
        var avoidCount = 1 + childrenAnalysis.values.map(v => v.best._1).sum
        var cost = cf.cost(enode, c => childrenAnalysis(c).best._2)
        var expr = ExprWithHashCons(enode.mapChildren(c => childrenAnalysis(c).best._3), t)
        // g may not be a Composition
        childrenAnalysis(g).bestNoComp.foreach { gbnc =>
          val childrenAvoidCountNoComp = childrenAnalysis(f).best._1 + gbnc._1
          if (childrenAvoidCountNoComp < avoidCount) {
            avoidCount = childrenAvoidCountNoComp
            cost = cf.cost(enode, Map(
              f -> childrenAnalysis(f).best._2,
              g -> gbnc._2
            ))
            expr = ExprWithHashCons(enode.mapChildren(Map(
              f -> childrenAnalysis(f).best._3,
              g -> gbnc._3
            )), t)
          }
        }
        val compound = (avoidCount, cost, expr)
        AvoidCompositionAssoc1ExtractData(compound, None)
      case _ =>
        val avoidCount = childrenAnalysis.values.map(v => v.best._1).sum
        val cost = cf.cost(enode, c => childrenAnalysis(c).best._2)
        val expr = ExprWithHashCons(enode.mapChildren(c => childrenAnalysis(c).best._3), t)
        val compound = (avoidCount, cost, expr)
        AvoidCompositionAssoc1ExtractData(compound, Some(compound))
    }
  }

  override def merge(a: AvoidCompositionAssoc1ExtractData[Cost],
                     b: AvoidCompositionAssoc1ExtractData[Cost]): AvoidCompositionAssoc1ExtractData[Cost] = {
    implicit val costCmp: Ordering[Cost] = cf.ordering
    AvoidCompositionAssoc1ExtractData(
      Seq(a.best, b.best).minBy { case (avoidCount, cost, _) => (avoidCount, cost) },
      (a.bestNoComp ++ b.bestNoComp).minByOption { case (avoidCount, cost, _) => (avoidCount, cost) },
    )
  }

  override def update(existing: AvoidCompositionAssoc1ExtractData[Cost],
                      computed: AvoidCompositionAssoc1ExtractData[Cost]): AvoidCompositionAssoc1ExtractData[Cost] =
    computed
}

case class BeamExtract[Cost](beamSize: Int, cf: CostFunction[Cost])
  extends Analyser.Analysis[Seq[(Cost, ExprWithHashCons)]]
{
  override def make(enode: ENode, t: TypeId, egraph: EGraph,
                    analysisOf: EClassId => Seq[(Cost, ExprWithHashCons)]): Seq[(Cost, ExprWithHashCons)] = {
    val childrenBeams = enode.children().map(c => (c, analysisOf(c))).toSeq

    def rec(remaining: Seq[(EClassId, Seq[(Cost, ExprWithHashCons)])],
            selected: Map[EClassId, (Cost, ExprWithHashCons)]): Seq[(Cost, ExprWithHashCons)] = {
      remaining match {
        case Nil =>
          Seq((
            cf.cost(enode, c => selected(c)._1),
            ExprWithHashCons(enode.mapChildren(c => selected(c)._2), t)))
        case (child, childBeam) +: rest =>
          childBeam.flatMap { x =>
            rec(rest, selected + (child -> x))
          }
      }
    }

    val tmp = rec(childrenBeams, Map.empty).sortBy(_._1)(cf.ordering).take(beamSize)
    assert(tmp == tmp.distinct)
    tmp
  }

  override def merge(a: Seq[(Cost, ExprWithHashCons)], b: Seq[(Cost, ExprWithHashCons)]): Seq[(Cost, ExprWithHashCons)] =
    Beam.merge(beamSize, cf, a, b)

  override def update(existing: Seq[(Cost, ExprWithHashCons)], computed: Seq[(Cost, ExprWithHashCons)]): Seq[(Cost, ExprWithHashCons)] =
    Beam.merge(beamSize, cf, existing, computed)
}

object BeamExtract {
  def print[Cost](beamSize: Int, cf: CostFunction[Cost], egraph: EGraph, id: EClassId): Unit = {
    val analyser = Analyser.init(egraph, BeamExtract(beamSize, cf))
    analyser.analysisOf(id).foreach { case (cost, expr) =>
      println(s"Cost of $cost:")
      println(Expr.toNamed(ExprWithHashCons.expr(egraph)(expr)))
    }
  }
}

object BeamExtractRW {
  sealed trait TypeAnnotation
  case class NotDataTypeAnnotation(node: TypeNode[TypeAnnotation, (), shine.DPIA.Types.AccessType])
    extends TypeAnnotation
  case class DataTypeAnnotation(access: shine.DPIA.Types.AccessType)
    extends TypeAnnotation

  type Data[Cost] = Map[(TypeAnnotation, Map[Int, TypeAnnotation]), Seq[(Cost, ExprWithHashCons)]]

  def merge[Cost](beamSize: Int, cf: CostFunction[Cost],
                  a: Data[Cost], b: Data[Cost]): Data[Cost] = {
    (a.keySet union b.keySet).map { ta =>
      ta -> ((a.get(ta), b.get(ta)) match {
        case (Some(x), Some(y)) => Beam.merge(beamSize, cf, x, y)
        case (None, Some(x)) => x
        case (Some(x), None) => x
        case (None, None) => throw new Exception("this should not happen")
      })
    }.toMap
  }

  def mergeEnv(a: Map[Int, TypeAnnotation], b: Map[Int, TypeAnnotation]): Option[Map[Int, TypeAnnotation]] = {
    def rec(keys: Seq[Int], acc: Map[Int, TypeAnnotation]): Option[Map[Int, TypeAnnotation]] = {
      keys match {
        case Nil => Some(acc)
        case i +: rest =>
          (a.get(i), b.get(i)) match {
            case (None, None) => throw new Exception("this should not happen")
            case (None, Some(x)) => rec(rest, acc + (i -> x))
            case (Some(x), None) => rec(rest, acc + (i -> x))
            case (Some(x), Some(y)) =>
              if (x == y) {
                rec(rest, acc + (i -> x))
              } else {
                None
              }
          }
      }
    }

    rec((a.keySet union b.keySet).toSeq, Map.empty)
  }

  def subtype(a: TypeAnnotation, at: TypeId, b: TypeAnnotation, bt: TypeId, egraph: EGraph): Boolean = {
    assert(at == bt)
    (a, b) match {
      case (DataTypeAnnotation(x), DataTypeAnnotation(y)) =>
        (x == y) || (x == shine.DPIA.Types.read || notContainingArrayType(bt.asInstanceOf[DataTypeId], egraph))
      case (NotDataTypeAnnotation(x), NotDataTypeAnnotation(y)) =>
        (x, egraph(at), y, egraph(bt)) match {
          case (FunType(aIn, aOut), FunType(aInT, aOutT), FunType(bIn, bOut), FunType(bInT, bOutT)) =>
            subtype(bIn, bInT, aIn, aInT, egraph) && subtype(aOut, aOutT, bOut, bOutT, egraph)
          case (NatFunType(aOut), NatFunType(aOutT), NatFunType(bOut), NatFunType(bOutT)) =>
            subtype(aOut, aOutT, bOut, bOutT, egraph)
          case (DataFunType(aOut), DataFunType(aOutT), DataFunType(bOut), DataFunType(bOutT)) =>
            subtype(aOut, aOutT, bOut, bOutT, egraph)
          case _ => throw new Exception("this should not happen")
        }
      case _ => throw new Exception("this should not happen")
    }
  }

  // TODO: could hash-cons this
  def notContainingArrayType(t: DataTypeId, egraph: EGraph): Boolean = {
    egraph(t) match {
      case DataTypeVar(_) => false
      case ScalarType(_) => true
      case NatType => true
      case VectorType(_, _) => true
      case IndexType(_) => true
      case PairType(dt1, dt2) => notContainingArrayType(dt1, egraph) && notContainingArrayType(dt2, egraph)
      case ArrayType(_, _) => false
    }
  }
}

object RWAnnotationDSL {
  import BeamExtractRW._
  val read = DataTypeAnnotation(shine.DPIA.Types.read)
  val write = DataTypeAnnotation(shine.DPIA.Types.write)

  implicit final class RWAnnotationOps(private val a: TypeAnnotation) extends AnyVal {
    @inline def ->:(b: TypeAnnotation): TypeAnnotation = NotDataTypeAnnotation(FunType(b, a))
  }

  def nFunT(a: TypeAnnotation): TypeAnnotation = NotDataTypeAnnotation(NatFunType(a))
  def dtFunT(a: TypeAnnotation): TypeAnnotation = NotDataTypeAnnotation(DataFunType(a))
}

// TODO: this procedure could actually extract DPIA terms directly?
case class BeamExtractRW[Cost](beamSize: Int, cf: CostFunction[Cost])
  extends Analyser.Analysis[BeamExtractRW.Data[Cost]]
{
  import BeamExtractRW._
  import RWAnnotationDSL._

  override def make(enode: ENode, t: TypeId, egraph: EGraph,
                    analysisOf: EClassId => Data[Cost]): Data[Cost] = {
    // val childrenBeams = enode.children().map(c => (c, analysisOf(c))).toSeq
    // TODO: write more generic code here
    val generatedData: Data[Cost] = enode match {
      case Var(index) =>
        val cost = cf.cost(egraph, enode, t, Map.empty)
        val expr = ExprWithHashCons(enode.mapChildren(Map.empty), t)
        Seq(read, write).map { annotation =>
          (annotation, Map(index -> annotation)) -> Seq((cost, expr))
        }.toMap
      case App(f, e) =>
        val fInT = egraph(egraph.get(f).t) match {
          case FunType(inT, _) => inT
          case _ => throw new Exception("this should not happen")
        }
        val eT = egraph.get(e).t

        val fBeams = analysisOf(f)
        val eBeams = analysisOf(e)
        var newBeams: Data[Cost] = Map.empty
        fBeams.foreach { case ((fAnnotation, fEnv), fBeam) =>
          fAnnotation match {
            case NotDataTypeAnnotation(FunType(fIn, fOut)) =>
              eBeams.foreach { case ((eAnnotation, eEnv), eBeam) =>
                mergeEnv(fEnv, eEnv).foreach { mergedEnv =>
                 if (subtype(fIn, fInT, eAnnotation, eT, egraph)) {
                    val newBeam = fBeam.flatMap { x => eBeam.flatMap { y =>
                      Seq((
                        cf.cost(egraph, enode, t, Map(f -> x._1, e -> y._1)),
                        ExprWithHashCons(enode.mapChildren(Map(f -> x._2, e -> y._2)), t)
                      ))
                    }}
                    newBeams = newBeams.updatedWith((fOut, mergedEnv)) {
                      case None => Some(newBeam)
                      case Some(prevBeam) => Some(prevBeam ++ newBeam)
                    }
                  }
                }
              }
            case _ => throw new Exception("this should not happen")
          }
        }
        newBeams
      case Lambda(e) =>
        val eBeams = analysisOf(e)
        var newBeams: Data[Cost] = Map.empty
        eBeams.foreach { case ((annotation, env), beam) =>
          val newEnv = env.filter(kv => kv._1 != 0).map(kv => (kv._1 - 1, kv._2))
          val newBeam = beam.flatMap { x =>
            Seq((
              cf.cost(egraph, enode, t, Map(e -> x._1)),
              ExprWithHashCons(enode.mapChildren(Map(e -> x._2)), t)
            ))
          }
          val identAnnotations = env.get(0).map(Seq(_)).getOrElse(Seq(read, write))
          identAnnotations.foreach { ina =>
            newBeams = newBeams.updatedWith((NotDataTypeAnnotation(FunType(ina, annotation)), newEnv)) {
              case None => Some(newBeam)
              case Some(prevBeam) => Some(prevBeam ++ newBeam)
            }
          }
        }
        newBeams
      case NatApp(f, _) =>
        val fBeams = analysisOf(f)
        fBeams.map { case ((annotation, env), beam) =>
          val newBeam = beam.flatMap { x =>
            Seq((
              cf.cost(egraph, enode, t, Map(f -> x._1)),
              ExprWithHashCons(enode.mapChildren(Map(f -> x._2)), t)
            ))
          }
          annotation match {
            case NotDataTypeAnnotation(NatFunType(at)) => (at, env) -> newBeam
            case _ => throw new Exception("this should not happen")
          }
        }
      case DataApp(f, _) =>
        val fBeams = analysisOf(f)
        fBeams.map { case ((annotation, env), beam) =>
          val newBeam = beam.flatMap { x =>
            Seq((
              cf.cost(egraph, enode, t, Map(f -> x._1)),
              ExprWithHashCons(enode.mapChildren(Map(f -> x._2)), t)
            ))
          }
          annotation match {
            case NotDataTypeAnnotation(DataFunType(at)) => (at, env) -> newBeam
            case _ => throw new Exception("this should not happen")
          }
        }
      case NatLambda(e) =>
        val eBeams = analysisOf(e)
        eBeams.map { case ((annotation, env), beam) =>
          val newBeam = beam.flatMap { x =>
            Seq((
              cf.cost(egraph, enode, t, Map(e -> x._1)),
              ExprWithHashCons(enode.mapChildren(Map(e -> x._2)), t)
            ))
          }
          // note: recording NatFunType() constructor is useless
          (NotDataTypeAnnotation(NatFunType(annotation)), env) -> newBeam
        }
      case DataLambda(e) =>
        val eBeams = analysisOf(e)
        eBeams.map { case ((annotation, env), beam) =>
          val newBeam = beam.flatMap { x =>
            Seq((
              cf.cost(egraph, enode, t, Map(e -> x._1)),
              ExprWithHashCons(enode.mapChildren(Map(e -> x._2)), t)
            ))
          }
          // note: recording DataFunType() constructor is useless
          (NotDataTypeAnnotation(DataFunType(annotation)), env) -> newBeam
        }
      case Literal(_) =>
        val beam = Seq((
          cf.cost(egraph, enode, t, Map.empty),
          ExprWithHashCons(enode.mapChildren(Map.empty), t)
        ))
        Map((read, Map.empty[Int, TypeAnnotation]) -> beam)
      case Primitive(p) =>
        import rise.core.{primitives => rp}
        import rise.openMP.{primitives => rompp}
        import rise.openCL.{primitives => roclp}
        import rise.Cuda.{primitives => rocup}

        val annotations = p match {
          case roclp.mapGlobal(_) | roclp.mapWorkGroup(_) | roclp.mapLocal(_)
               | rocup.mapGlobal(_) | rocup.mapBlock(_) | rocup.mapThreads(_)
               | rocup.mapWarp(_) | rocup.mapLane(_) | rompp.mapPar()
               | rp.mapSeq() | rp.mapSeqUnroll() | rp.iterateStream() => Seq(
            (read ->: write) ->: read ->: write
          )
          case rp.map() | rp.mapFst() | rp.mapSnd() => Seq(
            (read ->: read) ->: read ->: read,
            (write ->: write) ->: write ->: write,
          )
          case rp.mapStream() => Seq(
            (read ->: write) ->: read ->: read
          )
          case rp.toMem() => Seq(
            write ->: read
          )
          case rp.join() | rp.transpose() | rp.asScalar() | rp.unzip() => Seq(
            read ->: read,
            write ->: write
          )
          case rp.vectorFromScalar() | rp.neg() | rp.not() | rp.indexAsNat() |
               rp.fst() | rp.snd()  | rp.cast() => Seq(
            read ->: read
          )
          case rp.let() => Seq(
            read ->: (read ->: read) ->: read,
            read ->: (read ->: write) ->: write,
          )
          case rp.split() | rp.asVector() => Seq(
            nFunT(read ->: read),
            nFunT(write ->: write),
          )
          case rp.asVectorAligned() => Seq(
            nFunT(read ->: read),
            // FIXME: DPIA accepts write -> write but OpenMP codegen fails
          )
          case rp.zip() | rp.makePair() => Seq(
            read ->: read ->: read,
            write ->: write ->: write,
          )
          case rp.idx() | rp.add() | rp.sub() | rp.mul() | rp.div() | rp.gt()
               | rp.lt() | rp.equal() | rp.mod() | rp.gather() => Seq(
            read ->: read ->: read
          )
          case rp.natAsIndex() | rp.take() | rp.drop() => Seq(
            nFunT(read ->: read)
          )
          case rp.reduceSeq() | rp.reduceSeqUnroll() => Seq(
            (read ->: read ->: write) ->: write ->: read ->: read
          )
          case rp.generate() => Seq(
            (read ->: read) ->: read
          )
          case _ => throw new Exception(s"did not expect $p")
        }
        val beam = Seq((
          cf.cost(egraph, enode, t, Map.empty),
          ExprWithHashCons(enode.mapChildren(Map.empty), t)
        ))
        annotations.map { a => (a, Map.empty[Int, TypeAnnotation]) -> beam }.toMap
      case Composition(f, g) => ???
    }
    generatedData.map { case (at, beam) => at -> beam.sortBy(_._1)(cf.ordering).distinct.take(beamSize) }
  }

  override def merge(a: Data[Cost], b: Data[Cost]): Data[Cost] =
    BeamExtractRW.merge(beamSize, cf, a, b)

  override def update(existing: Data[Cost], computed: Data[Cost]): Data[Cost] =
    BeamExtractRW.merge(beamSize, cf, existing, computed)
}