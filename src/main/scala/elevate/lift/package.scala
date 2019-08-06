package elevate

import _root_.lift.core._

package object lift {

  def dotPrinter(expr: Expr,
                 printEdgeLabels: Boolean = false,
                 inlineApply: Boolean = false,
                 inlineTypedExpr: Boolean = false,
                 inlineLambdaIdentifier: Boolean = false): String = {

    @scala.annotation.tailrec
    def getID(x: Any): String = x match {
      case Apply(f,_) if inlineApply => getID(f)
      case TypedExpr(x,_) if inlineTypedExpr => getID(x)
      case i:Identifier if !inlineLambdaIdentifier => i.toString
      case _ =>freshName("node")
    }

    def genNodesAndEdges(expr: Expr,
                         parent: String,
                         printEdgeLabels: Boolean,
                         inlineApply: Boolean,
                         inlineTypedExpr: Boolean,
                         inlineLambdaIdentifier: Boolean,
                         ty: Option[String]): String = {

      def attr(str: String): String = s"[$str]"
      def fill(c: String): String = s"fillcolor=$c "
      def fillWhite = fill("white")
      def fillGray = fill("\"#e6e2de\"")
      def fillBlack = fill("black")

      case class Label(s: String,
                       decorations: String => String = x => s"$x",
                       forEdge: Boolean = false){
        def bold: Label = this.copy(decorations = x => s"<b>${decorations(x)}</b>")
        def italic: Label = this.copy(decorations = x => s"<i>${decorations(x)}</i>")
        def gray: Label = this.copy(decorations = x => s"<font color='gray'>${decorations(x)}</font>")
        def edge: Label = this.copy(forEdge = true)

        override def toString: String = {
          val label = if(ty.isDefined && !forEdge)
            "\"" + s + "\\n"+ty.get + "\""
          else "<" + decorations(s) + ">"

          s"[label=$label]"
        }
      }

      val edgeLabel = (x: String) => attr(fillBlack + Label(x).gray.edge.toString)
      def addEdgeLabel(s: String): String = if (printEdgeLabels) s else ""

      def recurse(e: Expr,
                  parent: String,
                  ty: Option[String]): String =
        genNodesAndEdges(e, parent, printEdgeLabels, inlineApply, inlineTypedExpr, inlineLambdaIdentifier, ty)

      def binaryNode(nodeLabel: String, a: (Expr, String), b: (Expr, String)): String = {
        val aID = getID(a._1)
        val bID = getID(b._1)
        s"""$parent ${attr(fillWhite + Label(nodeLabel).toString)}
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

        case Apply(f, e) if !inlineApply => binaryNode("apply", (f, "fun"), (e, "arg"))

        case Apply(f, e) if inlineApply =>
          val expr = getID(e)
          s"""$parent -> $expr ${addEdgeLabel(edgeLabel("apply"))}
             |${recurse(f, parent, None)}
             |${recurse(e, expr, None)}""".stripMargin

        case DepLambda(x, e) if !inlineLambdaIdentifier =>
          val id = getID(x)
          val expr = getID(e)
          s"""$parent ${attr(fillWhite + Label("Λ").toString)}
             |$parent -> $id ${addEdgeLabel(edgeLabel("id"))};
             |$parent -> $expr ${addEdgeLabel(edgeLabel("body"))};
             |$id ${attr(fillWhite + Label(x.toString).toString)}
             |${recurse(e, expr, None)}""".stripMargin

        case DepLambda(x, e) if inlineLambdaIdentifier =>
          val expr = getID(e)
          s"""$parent ${attr(fillWhite + Label(s"Λ.$x").toString)}
             |$parent -> $expr ${addEdgeLabel(edgeLabel("body"))};
             |${recurse(e, expr, None)}""".stripMargin

        case DepApply(f, e) =>
          val fun = getID(f)
          val arg = getID(e)
          s"""$parent ${attr(fillWhite + Label("apply").toString)}
             |$parent -> $fun ${addEdgeLabel(edgeLabel("fun"))};
             |$parent -> $arg ${addEdgeLabel(edgeLabel("arg"))};
             |$arg ${attr(fillWhite + Label(e.toString).toString)}
             |${recurse(f, fun, None)}""".stripMargin

        case TypedExpr(e, t) if inlineTypedExpr =>
          val formattedType = t.toString.replaceAll("->", "-\\\\>")
          s"${recurse(e, parent, Some(formattedType))}"

        case TypedExpr(e, t) if !inlineTypedExpr =>
          val expr = getID(e)
          val ty = getID(t)
          val formattedType = t.toString.replaceAll("->", "-\\\\>")
          s"""$parent ${attr(fillWhite + Label("TypedExpr").toString)}
             |$parent -> $expr ${addEdgeLabel(edgeLabel("expr"))}
             |$parent -> $ty ${addEdgeLabel(edgeLabel("type"))}
             |$ty [label="$formattedType"]
             |${recurse(e, expr, None)}""".stripMargin

        case l: Literal => s"$parent ${attr(fillWhite + Label(l.toString).italic.toString)}"
        case i: Identifier => s"$parent ${attr(fillWhite + Label(i.toString).italic.toString)}"
        case p: Primitive => s"$parent ${attr(fillGray + Label(p.toString).bold.toString)}"
      }
    }

    val content = genNodesAndEdges(expr, getID(expr), printEdgeLabels, inlineApply, inlineTypedExpr, inlineLambdaIdentifier, None)
    s"""
       |digraph graphname
       |{
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
