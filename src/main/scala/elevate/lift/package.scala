package elevate

import _root_.lift.core._
import _root_.lift.core.types.Kind
import _root_.lift.core.types.Type
import _root_.lift.core.DSL._
import _root_.lift.core.primitives._
import elevate.lift.strategies.normalForm._

package object lift {

  def structEq(a: Expr, b: Expr): Boolean = BENF(a).get == BENF(b).get

  // notation
  val tileSize = 4

  def T: Expr = transpose
  def S: Expr = split(tileSize) //slide(3)(1)
  def J: Expr = join
  def *(x: Expr): Expr = map(x)
  def **(x: Expr): Expr = map(map(x))
  def ***(x: Expr): Expr = map(map(map(x)))
  def ****(x: Expr): Expr = map(map(map(map(x))))
  def *****(x: Expr): Expr = map(map(map(map(map(x)))))
  def ******(x: Expr): Expr = map(map(map(map(map(map(x))))))

  def λ(f: Identifier => Expr): Expr = fun(f)

  // map in LCNF
  def *!(x: Expr): Expr = {
    val i = Identifier(freshName("e"))
    map(Lambda(i, Apply(x, i)))
  }

  def **!(x: Expr): Expr = {
    val i = Identifier(freshName("e"))
    map(Lambda(i, Apply(*!(x), i)))
  }

  def ***!(x: Expr): Expr = {
    val i = Identifier(freshName("e"))
    map(Lambda(i, Apply(**!(x), i)))
  }

  def ****!(x: Expr): Expr = {
    val i = Identifier(freshName("e"))
    map(Lambda(i, Apply(***!(x), i)))
  }

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

      val edgeLabel = (x: String) => Label(x).gray.edge.toString
      def addEdgeLabel(s: String): String = if (printEdgeLabels) s else ""

      def recurse(e: Expr,
                  parent: String,
                  ty: Option[String]): String =
        genNodesAndEdges(e, parent, printEdgeLabels, inlineApply, inlineTypedExpr, inlineLambdaIdentifier, ty)

      def binaryNode(nodeLabel: String, a: (Expr, String), b: (Expr, String)): String = {
        val aID = getID(a._1)
        val bID = getID(b._1)
        s"""$parent ${Label(nodeLabel)}
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
          s"""$parent ${Label(s"λ.$i")}
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
          s"""$parent ${Label("Λ")}
             |$parent -> $id ${addEdgeLabel(edgeLabel("id"))};
             |$parent -> $expr ${addEdgeLabel(edgeLabel("body"))};
             |$id ${Label(x.toString)}
             |${recurse(e, expr, None)}""".stripMargin

        case DepLambda(x, e) if inlineLambdaIdentifier =>
          val expr = getID(e)
          s"""$parent ${Label(s"Λ.$x")}
             |$parent -> $expr ${addEdgeLabel(edgeLabel("body"))};
             |${recurse(e, expr, None)}""".stripMargin

        case DepApply(f, e) =>
          val fun = getID(f)
          val arg = getID(e)
          s"""$parent ${Label("apply")}
             |$parent -> $fun ${addEdgeLabel(edgeLabel("fun"))};
             |$parent -> $arg ${addEdgeLabel(edgeLabel("arg"))};
             |$arg ${Label(e.toString)}
             |${recurse(f, fun, None)}""".stripMargin

        case TypedExpr(e, t) if inlineTypedExpr =>
          val formattedType = t.toString.replaceAll("->", "-\\\\>")
          s"${recurse(e, parent, Some(formattedType))}"

        case TypedExpr(e, t) if !inlineTypedExpr =>
          val expr = getID(e)
          val ty = getID(t)
          val formattedType = t.toString.replaceAll("->", "-\\\\>")
          s"""$parent ${Label("TypedExpr")}
             |$parent -> $expr ${addEdgeLabel(edgeLabel("expr"))}
             |$parent -> $ty ${addEdgeLabel(edgeLabel("type"))}
             |$ty [label="$formattedType"]
             |${recurse(e, expr, None)}""".stripMargin

        case l: Literal => s"$parent ${Label(l.toString).italic}"
        case i: Identifier => s"$parent ${Label(i.toString).italic}"
        case p: Primitive => s"$parent ${Label(p.toString).bold}"
      }
    }

    val content = genNodesAndEdges(expr, getID(expr), printEdgeLabels, inlineApply, inlineTypedExpr, inlineLambdaIdentifier, None)
    s"""
       |digraph graphname
       |{
       |node [shape="record",style="rounded"]
       |$content
       |}
     """.stripMargin
  }

}
