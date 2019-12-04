package elevate

import _root_.lift.core._
import _root_.lift.core.types._
import elevate.core.strategies.debug.peek
import elevate.core.{RewriteResult, Strategy, Success}

package object lift {

  type Lift = Expr

  def printExpr : Strategy[Lift] = peek[Lift](p => println(s"${toEvaluableString(p)}"))
  def printExpr(msg: String) : Strategy[Lift] = peek[Lift](p => println(s"$msg \n${toEvaluableString(p)}"))

  def toEvaluableString(e: Lift): String = {
    e match {
      case Identifier(name) => s"""Identifier("id$name")""" // id prefix prevents name clashes with freshName
      case Lambda(x, e) => s"Lambda(${toEvaluableString(x)}, ${toEvaluableString(e)})"
      case App(f, e) => s"Apply(${toEvaluableString(f)}, ${toEvaluableString(e)})"
      case DepLambda(x, e) => x match {
        case n: NatIdentifier => s"""DepLambda[NatKind](NatIdentifier("id$n"), ${toEvaluableString(e)})"""
        case dt: DataTypeIdentifier => s"""DepLambda[DataKind]("id$dt", ${toEvaluableString(e)})"""
      }
      case DepApp(f, x) => x match {
        case n: Nat => s"DepApply[NatKind](${toEvaluableString(f)}, $n)"
        case dt: DataType => s"DepApply[DataKind](${toEvaluableString(f)}, $dt)"
      }
      case Literal(d) => s"Literal($d)"
      case ff: ForeignFunction => ff.toString
      case p: Primitive => p.toString
    }
  }

  def dotPrinter(expr: Expr,
                 printEdgeLabels: Boolean = true,
                 printTypes: Boolean = false,
                 inlineApply: Boolean = false,
                 inlineLambdaIdentifier: Boolean = false): String = {

    @scala.annotation.tailrec
    def getID(x: Any): String = x match {
      case App(f,_) if inlineApply => getID(f)
      case i:Identifier if !inlineLambdaIdentifier => i.toString
      case _ =>freshName("node")
    }

    def genNodesAndEdges(expr: Expr,
                         parent: String,
                         printEdgeLabels: Boolean,
                         inlineApply: Boolean,
                         inlineLambdaIdentifier: Boolean): String = {

      def attr(str: String): String = s"[$str]"
      def fill(c: String): String = s"fillcolor=$c "
      def fillWhite = fill("white")
      def fillGray = fill("\"#e6e2de\"")
      def fillBlack = fill("black")

      def formatType(t: Type): String = t.toString.replaceAll(">", "\\\\>").replaceAll("<", "\\\\<")

      case class Label(s: String,
                       decorations: String => String = x => s"$x",
                       forEdge: Boolean = false){
        def bold: Label = this.copy(decorations = x => s"<b>${decorations(x)}</b>")
        def italic: Label = this.copy(decorations = x => s"<i>${decorations(x)}</i>")
        def gray: Label = this.copy(decorations = x => s"<font color='gray'>${decorations(x)}</font>")
        def green: Label = this.copy(decorations = x => s"""<font color="#3C8031">${decorations(x)}</font>""")
        def orange: Label = this.copy(decorations = x => s"""<font color="#F26035">${decorations(x)}</font>""")
        def edge: Label = this.copy(forEdge = true)

        override def toString: String = {
          val label = if(!forEdge && printTypes)
            "\"" + s + "\\n"+formatType(expr.t) + "\""
          else "<" + decorations(s) + ">"

          s"label=$label"
        }
      }

      val edgeLabel = (x: String) => attr(fillBlack + Label(x).gray.edge.toString)
      def addEdgeLabel(s: String): String = if (printEdgeLabels) s else ""

      def recurse(e: Expr,
                  parent: String,
                  ty: Option[String]): String =
        genNodesAndEdges(e, parent, printEdgeLabels, inlineApply, inlineLambdaIdentifier)

      def binaryNode(nodeLabel: String, a: (Expr, String), b: (Expr, String)): String = {
        val aID = getID(a._1)
        val bID = getID(b._1)
        s"""$parent ${attr(fillWhite + Label(nodeLabel).bold.green.toString)}
           |$parent -> $aID ${addEdgeLabel(edgeLabel(a._2))};
           |$parent -> $bID ${addEdgeLabel(edgeLabel(b._2))};
           |${recurse(a._1, aID, None)}
           |${recurse(b._1, bID, None)}""".
          stripMargin
        }

      expr match {
        case Lambda(i, e) if !inlineLambdaIdentifier => binaryNode("λ", (i, "id"), (e, "body"))

        case Lambda(i, e) if inlineLambdaIdentifier =>
          val idLabel = getID(i)
          val expr = getID(e)
          s"""$parent ${attr(fillWhite + Label(s"λ.$i").toString)}
             |$parent -> $expr ${addEdgeLabel(edgeLabel("body"))};
             |${recurse(e, expr, None)}""".stripMargin

        case App(f, e) if !inlineApply => binaryNode("apply", (f, "fun"), (e, "arg"))

        case App(f, e) if inlineApply =>
          val expr = getID(e)
          s"""$parent -> $expr ${addEdgeLabel(edgeLabel("apply"))}
             |${recurse(f, parent, None)}
             |${recurse(e, expr, None)}""".stripMargin

        case DepLambda(x, e) if !inlineLambdaIdentifier =>
          val id = getID(x)
          val expr = getID(e)
          s"""$parent ${attr(fillWhite + Label("Λ").bold.green.toString)}
             |$parent -> $id ${addEdgeLabel(edgeLabel("id"))};
             |$parent -> $expr ${addEdgeLabel(edgeLabel("body"))};
             |$id ${attr(fillWhite + Label(x.toString).orange.toString)}
             |${recurse(e, expr, None)}""".stripMargin

        case DepLambda(x, e) if inlineLambdaIdentifier =>
          val expr = getID(e)
          s"""$parent ${attr(fillWhite + Label(s"Λ.$x").toString)}
             |$parent -> $expr ${addEdgeLabel(edgeLabel("body"))};
             |${recurse(e, expr, None)}""".stripMargin

        case DepApp(f, e) =>
          val fun = getID(f)
          val arg = getID(e)
          s"""$parent ${attr(fillWhite + Label("depApply").toString)}
             |$parent -> $fun ${addEdgeLabel(edgeLabel("fun"))};
             |$parent -> $arg ${addEdgeLabel(edgeLabel("arg"))};
             |$arg ${attr(fillWhite + Label(e.toString).toString)}
             |${recurse(f, fun, None)}""".stripMargin

        case l: Literal => s"$parent ${attr(fillWhite + Label(l.toString).orange.italic.toString)}"
        case i: Identifier => s"$parent ${attr(fillWhite + Label(i.toString).orange.italic.toString)}"
        case p: Primitive => s"$parent ${attr(fillGray + Label(p.toString).bold.green.toString)}"
      }
    }

    val content = genNodesAndEdges(expr, getID(expr), printEdgeLabels, inlineApply, inlineLambdaIdentifier)
    s"""
       |digraph graphname
       |{
       |graph [fontname = "FiraCode"];
       |node [fontname = "FiraCode"];
       |edge [fontname = "FiraCode"];
       |node [shape="record",style="rounded, filled"]
       |$content
       |}
     """.stripMargin
  }

  def exprToDot(path: String, name: String, e: Expr, dot: Expr => String): Unit = {
    import java.io._
    import sys.process._

    val w =new PrintWriter(new File(s"$path/$name.dot"))
    w.write(dot(e))
    w.flush()
    w.close()
    val test = s"dot -Tpdf $path/$name.dot -o $path/$name.pdf".!
  }
}
