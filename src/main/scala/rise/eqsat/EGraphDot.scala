package rise.eqsat

import scala.language.postfixOps
import scala.sys.process._
import java.io.{File, Writer, BufferedWriter, FileWriter}

/** Defines how to output graphviz files for an [[EGraph]]
  * @param flooding: if not empty,
  *   only print classes which are reachable from the given ones
  */
case class EGraphDot(egraph: EGraph,
                     flooding: Seq[EClassId] = Seq(),
                     printTypes: Boolean = true) {
  def toSVG(path: String): Unit = {
    val dotPath = path.replace(".svg", ".dot")
    toFile(dotPath)
    (s"dot -Tsvg $dotPath -o $path" !!)
  }

  def toFile(path: String): Unit = {
    val file = new File(path)
    val writer = new BufferedWriter(new FileWriter(file))
    try { writeTo(writer) }
    finally { writer.close() }
  }

  def writeTo(writer: Writer): Unit = {
    def writeln(s: String): Unit = {
      writer.write(s); writer.write('\n')
    }

    writeln("digraph egraph {")

    // set compound=true to enable edges to clusters
    writeln("  compound=true")
    writeln("  clusterrank=local")

    val classesToPrint = if (flooding.isEmpty) {
      egraph.classes.values.toSeq
    } else {
      val todo = Vec(flooding: _*)
      val visited = HashSet[EClassId]()
      while (todo.nonEmpty) {
        val next = todo.remove(todo.size - 1)

        if (!visited.contains(next)) {
          visited += next
          egraph.get(next).nodes.foreach { n =>
            n.children().foreach(todo.addOne)
          }
        }
      }
      visited.map(egraph.get).toSeq
    }

    // define all the nodes, clustered by eclass
    for (eclass <- classesToPrint) {
      writeln(s"  subgraph cluster_${eclass.id.i} {")
      writeln("    style=dotted")
      for ((node, i) <- eclass.nodes.zipWithIndex) {
        writeln(s"""    ${eclass.id.i}.$i[label = "${nodeLabel(node)}"]""")
      }
      val t = ExprWithHashCons.`type`(egraph)(eclass.t)
      if (printTypes) writeln(s"""    label = "#${eclass.id.i} : $t"""")
      writeln("  }")
    }

    for (eclass <- classesToPrint) {
      for ((node, i) <- eclass.nodes.zipWithIndex) {
        var argI = 0
        val childrenCount = node.childrenCount()
        node.children().foreach { child =>
          val (anchor, label) = edge(argI, childrenCount)
          val childLeader = egraph.find(child)
          val (targetNode, targetCluster) = if (childLeader == eclass.id) {
            (s"${eclass.id.i}.$i:n", s"cluster_${eclass.id.i}")
          } else {
            // .0 to pick an arbitrary node in the cluster
            (s"${childLeader.i}.0", s"cluster_${childLeader.i}")
          }
          writeln(s"  ${eclass.id.i}.$i$anchor -> $targetNode [lhead = $targetCluster, $label]")
          argI += 1
        }
      }
    }

    writeln("}")
  }

  // returns label and anchor
  private def edge(i: Int, len: Int): (String, String) = {
    assert(i < len)
    (len, i) match {
      case (1, 0) => ("", "")
      case (2, 0) => (":sw", "")
      case (2, 1) => (":se", "")
      case (3, 0) => (":sw", "")
      case (3, 1) => (":s", "")
      case (3, 2) => (":se", "")
      case (_, _) => ("", s"label=$i")
    }
  }

  private def nodeLabel(n: ENode): String = {
    n match {
      case Var(index) => s"%$index"
      case App(_, _) => "app"
      case Lambda(_) => "λ"
      case NatApp(_, n) => s"nApp $n"
      case NatLambda(_) => "Λ : nat"
      case DataApp(_, dt) => s"dtApp $dt"
      case DataLambda(_) => "Λ : data"
      case AddrApp(_, a) => s"aApp $a"
      case AddrLambda(_) => "Λ : addr"
      case Literal(d) => s"$d"
      case NatLiteral(n) => s"$n"
      case IndexLiteral(i, n) => s"idx($i, $n)"
      case Primitive(p) => s"${p.toString.trim}"
      case Composition(_, _) => ">>"
    }
  }
}
