package rise.core

import rise.core.types.Type

case object dotPrinter {

  def apply(expr: Expr): String = generateDotString(expr)

  def generateDotString(
    expr: Expr,
    printTypes: Boolean = false,
    inlineLambdaIdentifier: Boolean = false,
    applyNodes: Boolean = false
  ): String = {

    def getID(x: Any): String = x match {
      case i: Identifier if !inlineLambdaIdentifier => i.toString
      case _                                        => freshName("node")
    }

    def generateNodesAndEdges(
      expr: Expr,
      parent: String,
      printTypes: Boolean,
      inlineLambdaIdentifier: Boolean,
      applyNodes: Boolean
    ): String = {

      def attr(str: String): String = s"[$str]"
      def fill(c: String): String = s"fillcolor=$c "
      def fillWhite: String = fill("white")
      def fillGray: String = fill("\"#e6e2de\"")
      def fillBlack: String = fill("black")

      def formatType(t: Type): String =
        if(printTypes)
          t.toString.replaceAll(">", "\\\\>").replaceAll("<", "\\\\<")
        else ""

      case class Label(
          s: String,
          decorations: String => String = x => s"$x",
          forEdge: Boolean = false
      ) {
        def bold: Label =
          this.copy(decorations = x => s"<b>${decorations(x)}</b>")
        def italic: Label =
          this.copy(decorations = x => s"<i>${decorations(x)}</i>")
        def gray: Label =
          this.copy(decorations =
            x => s"<font color='gray'>${decorations(x)}</font>"
          )
        def green: Label =
          this.copy(decorations =
            x => s"""<font color="#3C8031">${decorations(x)}</font>"""
          )
        def orange: Label =
          this.copy(decorations =
            x => s"""<font color="#F26035">${decorations(x)}</font>"""
          )
        def edge: Label = this.copy(forEdge = true)

        override def toString: String = {
          val label =
            if (!forEdge)
              "\"" + s + "\\n" + formatType(expr.t) + "\""
            else "<" + decorations(s) + ">"

          s"label=$label"
        }
      }

      val edgeLabel = (x: String) =>
        attr(fillBlack + Label(x).gray.edge.toString)

      def recurse(e: Expr, parent: String, ty: Option[String]): String =
        generateNodesAndEdges(e, parent, printTypes, inlineLambdaIdentifier)

      def binaryNode(
          nodeLabel: String,
          a: (Expr, String),
          b: (Expr, String)
      ): String = {
        val aID = getID(a._1)
        val bID = getID(b._1)
        s"""$parent ${attr(fillWhite + Label(nodeLabel).bold.green.toString)}
           |$parent -> $aID ${edgeLabel(a._2)};
           |$parent -> $bID ${edgeLabel(b._2)};
           |${recurse(a._1, aID, None)}
           |${recurse(b._1, bID, None)}""".stripMargin
      }

      expr match {
        case Lambda(i, e) if !inlineLambdaIdentifier =>
          binaryNode("λ", (i, "id"), (e, "body"))

        case Lambda(i, e) if inlineLambdaIdentifier =>
          val expr = getID(e)
          s"""$parent ${attr(fillWhite + Label(s"λ.${i.name}").toString)}
             |$parent -> $expr ${edgeLabel("body")};
             |${recurse(e, expr, None)}""".stripMargin

        case App(f, e) if applyNodes =>
          binaryNode("apply", (f, "fun"), (e, "arg"))

        case App(f, e) if !applyNodes =>
          val eID = getID(e)
          s"""${recurse(f, parent, None)}
             |${recurse(e, eID, None)}
             |$parent -> $eID ${edgeLabel("arg")};""".stripMargin

        case DepLambda(x, e) if !inlineLambdaIdentifier =>
          val id = getID(x)
          val expr = getID(e)
          s"""$parent ${attr(fillWhite + Label("Λ").bold.green.toString)}
             |$parent -> $id ${edgeLabel("id")};
             |$parent -> $expr ${edgeLabel("body")};
             |$id ${attr(fillWhite + Label(x.name).orange.toString)}
             |${recurse(e, expr, None)}""".stripMargin

        case DepLambda(x, e) if inlineLambdaIdentifier =>
          val expr = getID(e)
          s"""$parent ${attr(fillWhite + Label(s"Λ.${x.name}").toString)}
             |$parent -> $expr ${edgeLabel("body")};
             |${recurse(e, expr, None)}""".stripMargin

        case DepApp(f, e) if applyNodes =>
          val fun = getID(f)
          val arg = getID(e)
          s"""$parent ${attr(fillWhite + Label("depApply").toString)}
             |$parent -> $fun ${edgeLabel("fun")};
             |$parent -> $arg ${edgeLabel("arg")};
             |$arg ${attr(fillWhite + Label(e.toString).toString)}
             |${recurse(f, fun, None)}""".stripMargin

        case DepApp(f, e) if !applyNodes =>
          val eID = getID(e)
          s"""${recurse(f, parent, None)}
             |$eID ${attr(fillWhite + Label(e.toString).toString)}
             |$parent -> $eID ${edgeLabel("dep arg")};""".stripMargin

        case Literal(data) =>
          s"$parent ${attr(fillWhite + Label(data.toString).orange.italic.toString)}"
        case i: Identifier =>
          s"$parent ${attr(fillWhite + Label(i.name).orange.italic.toString)}"
        case p: Primitive =>
          s"$parent ${attr(fillGray + Label(p.name).bold.green.toString)}"
      }
    }

    val content =
      generateNodesAndEdges(expr, getID(expr), printTypes, inlineLambdaIdentifier)

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

  // todo remove before pull-request
  def exprToDot(name: String, e: Expr): Unit =
    exprToDot("/home/bastian/development/rewriting/dot", name, e, dotPrinter(_))

  def exprToDot(
      path: String,
      name: String,
      e: Expr,
      dot: Expr => String
  ): Unit = {
    import java.io._
    import sys.process._

    val w = new PrintWriter(new File(s"$path/$name.dot"))
    w.write(dot(e))
    w.flush()
    w.close()
    s"dot -Tsvg $path/$name.dot -o $path/$name.svg".!
  }
}
