package lift.core

case class RenderException(msg: String) extends Exception {
  override def toString = s"render exception: $msg"
}

object DrawTree {
  case class UnicodeConfig
  (wireHSym: String, wireVSym: String,
   connLSym: String, connLLSym: String, connRSym: String, connRRSym: String,
   spaceSym: String,
   convLSym: String, convRSym: String, convLRSym: String, convUSym: String)

  def genUnicodeConfig(s: String): UnicodeConfig = {
    if (s.length < 11) throw RenderException("not a valid Unicode Config")
    else {
      val wireV = s(0).toString
      val connL = s(1).toString
      val connLL = s(2).toString
      val connR = s(3).toString
      val connRR = s(4).toString
      val convL = s(5).toString
      val convR = s(6).toString
      val convLR = s(7).toString
      val convU = s(8).toString
      val space = s(9).toString
      val wireH = s.substring(10)
      val len = s.length - 10
      val spaces = space * len
      UnicodeConfig(
        wireH,
        spaces + wireV,
        spaces + connL,
        spaces + connLL,
        spaces + connR,
        spaces + connRR,
        space + spaces,
        convL,
        convR,
        convLR,
        convU
      )
    }
  }

  val defaultUnicodeConfig: UnicodeConfig = genUnicodeConfig("│┌├└├╩╦╬═ ─")

  case class UnicodeDraw
  (farLeft: Seq[Seq[String]],
   nearLeft: Seq[Seq[String]],
   nearRight: Seq[Seq[String]],
   farRight: Seq[Seq[String]])

  sealed abstract class Cxt
  final case object L extends Cxt
  final case object LR extends Cxt
  final case object R extends Cxt
  final case object U extends Cxt

  def mergeCxt(a: Cxt, b: Cxt): Cxt = (a, b) match {
    case (_, U) => U
    case (U, c) => c
    case (LR, _) => LR
    case (_, LR) => LR
    case (L, L) => L
    case (L, R) => LR
    case (R, R) => R
    case (R, L) => LR
  }


  abstract class RenderUnicodeDraw {
    def draw(cxt: Cxt)(implicit cfg: UnicodeConfig) : UnicodeDraw
  }

  case class Line(s: String) extends RenderUnicodeDraw {
    def draw(cxt: Cxt)(implicit cfg: UnicodeConfig) =
      UnicodeDraw(Seq.empty, Seq.empty, Seq(cfg.wireHSym +: Seq(s)), Seq.empty)
  }

  def line(s: String) = Line(s)

  case class AddLeft(a: RenderUnicodeDraw, b: RenderUnicodeDraw) extends RenderUnicodeDraw {
    def draw(cxt: Cxt)(implicit cfg: UnicodeConfig): UnicodeDraw = {
      val da = a.draw(mergeCxt(cxt, U))
      val db = b.draw(mergeCxt(cxt, L))
      val wireV = cfg.wireVSym
      val space = cfg.spaceSym
      val conn = if (cxt == L || cxt == LR) cfg.connLLSym else cfg.connLSym
      val wireL = if (cxt == L || cxt == LR) wireV else space
      val fl = da.farLeft.map(wireL +: _) ++ da.nearLeft.map(wireL +: _)
      val nl = (((conn +: da.nearRight.head) +: da.nearRight.tail.map(wireV +: _))
        ++ da.farRight.map(wireV +: _) ++ db.farLeft ++ db.nearLeft)
      val nr = db.nearRight
      val fr = db.farRight
      UnicodeDraw(fl, nl, nr, fr)
    }
  }

  case class AddRight(a: RenderUnicodeDraw, b: RenderUnicodeDraw) extends RenderUnicodeDraw {
    def draw(cxt: Cxt)(implicit cfg: UnicodeConfig): UnicodeDraw = {
      val da = a.draw(mergeCxt(cxt, R))
      val db = b.draw(mergeCxt(cxt, U))
      val wireV = cfg.wireVSym
      val space = cfg.spaceSym
      val conn = if (cxt == R || cxt == LR) cfg.connRRSym else cfg.connRSym
      val wireR = if (cxt == R || cxt == LR) wireV else space
      val fl = da.farLeft
      val nl = da.nearLeft
      val nr = (da.nearRight ++ da.farRight ++ db.farLeft.map(wireV +: _)
          ++ db.nearLeft.map(wireV +: _))
      val fr = (((conn +: db.nearRight.head) +: db.nearRight.tail.map(wireR +: _))
        ++ db.farRight.map(wireR +: _))
      UnicodeDraw(fl, nl, nr, fr)
    }
  }

  implicit class AddLeftOp(a: RenderUnicodeDraw) {
    def <+:(b: RenderUnicodeDraw): RenderUnicodeDraw = AddLeft(b, a)
  }

  implicit class AddRightOp(a: RenderUnicodeDraw) {
    def :+>(b: RenderUnicodeDraw): RenderUnicodeDraw = AddRight(a, b)
  }

  case class Block(a: RenderUnicodeDraw) extends RenderUnicodeDraw {
    def draw(cxt: Cxt)(implicit cfg: UnicodeConfig): UnicodeDraw = {
      val da = a.draw(mergeCxt(cxt, U))
      val wireV = cfg.wireVSym
      val wireH = cfg.wireHSym
      val conv = cxt match {
        case L => cfg.convLSym
        case R => cfg.convRSym
        case LR => cfg.convLRSym
        case _ => cfg.convUSym
      }
      val space = cfg.spaceSym
      val wireL = if (cxt == L || cxt == LR) wireV else space
      val wireR = if (cxt == R || cxt == LR) wireV else space
      val fl = da.farLeft.map(wireL +: _) ++ da.nearLeft.map(wireL +: _)
      val nl = Seq.empty
      val nr = Seq(wireH +: (conv +: da.nearRight.head))
      val fr = da.nearRight.tail.map(wireR +: _) ++ da.farRight.map(wireR +: _)
      UnicodeDraw(fl, nl, nr, fr)
    }
  }

  def block(a: RenderUnicodeDraw) = Block(a)

  case class NamedBlock(n: String, a: RenderUnicodeDraw) extends RenderUnicodeDraw {
    def draw(cxt: Cxt)(implicit cfg: UnicodeConfig): UnicodeDraw = {
      val da = a.draw(mergeCxt(cxt, U))
      val wireV = cfg.wireVSym
      val wireH = cfg.wireHSym
      val space = cfg.spaceSym
      val extraSpace = space.substring(0,1) * (n.length - 1)
      val wireL = if (cxt == L || cxt == LR) wireV else space
      val wireR = if (cxt == R || cxt == LR) wireV else space
      val fl = (da.farLeft.map(x => wireL +: (extraSpace +: x))
        ++ da.nearLeft.map(x => wireL +: (extraSpace +: x)))
      val nl = Seq.empty
      val nr = Seq(wireH +: (n +: da.nearRight.head))
      val fr = (da.nearRight.tail.map(x => wireR +: (extraSpace +: x))
        ++ da.farRight.map(x => wireR +: (extraSpace +: x)))
      UnicodeDraw(fl, nl, nr, fr)
    }
  }

  def block(n: String, a: RenderUnicodeDraw) = NamedBlock(n, a)

  def flattenUnicodeDraw(d: UnicodeDraw): String = {
    (d.farLeft.map(_.mkString) ++ d.nearLeft.map(_.mkString)
      ++ d.nearRight.map(_.mkString) ++ d.farRight.map(_.mkString)).mkString("\n")
  }

  def drawTree(cfg: UnicodeConfig, r: RenderUnicodeDraw): String = {
    flattenUnicodeDraw(r.draw(U)(cfg))
  }

  def drawTree(r: RenderUnicodeDraw): String = drawTree(defaultUnicodeConfig, r)
}

object DrawAST {
  import lift.core.DrawTree._
  def drawAST(e: Expr): String = drawTree(renderAST(e))
  def drawAST(cfg: UnicodeConfig, e: Expr): String = drawTree(cfg, renderAST(e))
  def renderAST(e: Expr): RenderUnicodeDraw = e match {
      case i: Identifier => line(i.name)
      case Lambda(x, e) => block(s"λ${x.name}", renderAST(e))
      case App(f, e) => renderAST(f) :+> renderAST(e)
      case dl @ DepLambda(x, e) => block(s"Λ${x.name}:${dl.kn.get}", renderAST(e))
      case DepApp(f, x) => line(x.toString) <+: renderAST(f)
      case l: Literal => line(l.toString)
      case p: Primitive => line(p.toString)
  }
}

object LessBrackets {
  def lessBrackets(e: Expr): String = lessBrackets(e, wrapped = false)
  def lessBrackets(e: Expr, wrapped: Boolean): String = e match {
    case i: Identifier => i.name
    case Lambda(x, e) => {
      val xs = lessBrackets(x)
      val es = lessBrackets(e)
      if (wrapped) s"<λ$xs. $es>" else s"λ$xs. $es"
    }
    case App(f, e) => {
      val fs = f match {
        case _: Lambda => lessBrackets(f, wrapped = true)
        case _ => lessBrackets(f)
      }
      val es = lessBrackets(e, wrapped = true)
      if (wrapped) s"($fs $es)" else s"$fs $es"
    }
    case dl @ DepLambda(x, e) => {
      val xs = s"${x.name}:${dl.kn.get}"
      val es = lessBrackets(e)
      if (wrapped) s"[Λ$xs. $es]" else s"Λ$xs. $es"
    }
    case DepApp(f, x) => {
      val fs = f match {
        case _: DepLambda[_] => lessBrackets(f, wrapped = true)
        case _ => lessBrackets(f)
      }
      if (wrapped) s"($fs $x)" else s"$fs $x"
    }
    case l: Literal => l.toString
    case p: Primitive => p.toString
  }
}

object ShowLift {
  import lift.core.DrawAST._
  import lift.core.LessBrackets._
  def showLift(e: Expr): String = if (size(e) >= 15) drawAST(e) else lessBrackets(e)
  def size(e: Expr): Int = e match {
    case _: Identifier => 1
    case Lambda(x, e) => size(x) + size(e)
    case App(f, e) => size(f) + size(e)
    case DepLambda(_, e) => 1 + size(e)
    case DepApp(f, _) => size(f) + 1
    case _: Literal => 1
    case _: Primitive => 1
  }
}

object ShowLiftCompact {
  import lift.core.DrawTree._
  import lift.core.semantics._
  def showLiftCompact(e: Expr): String = showLiftCompact(e, 15)
  def showLiftCompact(e: Expr, inlineSize: Int): String = drawTree(renderASTCompact(e)(inlineSize)._3)
  def renderASTCompact(e: Expr, wrapped: Boolean = false)
                      (implicit inlineSize: Int): (Boolean, Int, RenderUnicodeDraw) = e match {
    case i: Identifier => (true, 1, line(i.name))
    case Lambda(x, e) => {
      val xs = x.name
      val (eInline, eSize, er) = renderASTCompact(e)
      val newSize = eSize + 1
      if ((inlineSize > 0) && eInline) {
        crush(er) match {
          case Line(es) =>
            val ls = s"λ$xs. $es"
            (true, newSize, if (wrapped) wrappedLine("<", ls, ">") else line(ls))
        }
      } else (false, newSize, block(s"λ$xs", er))
    }
    case App(f, e) => {
      val (fInline, fSize, fr) = f match {
        case _: Lambda => renderASTCompact(f, wrapped = true)
        case _ => renderASTCompact(f)
      }
      val (_, eSize, er) = renderASTCompact(e, wrapped = true)
      val newSize = fSize + eSize
      if ((inlineSize > 0) && (fInline && (eSize == 1)) || (newSize <= inlineSize)) {
        (crush(fr), crush(er)) match {
          case (Line(fs), Line(es)) =>
            val as = s"$fs $es"
            (true, newSize, if (wrapped) wrappedLine("(", as, ")") else line(as))
        }
      } else (false, newSize, unwrap(fr) :+> unwrap(er))
    }
    case dl @ DepLambda(x, e) => {
      val xs = s"${x.name}:${dl.kn.get}"
      val (eInline, eSize, er) = renderASTCompact(e)
      val newSize = eSize + 1
      if ((inlineSize > 0) && eInline) {
        crush(er) match {
          case Line(es) =>
            val dls = s"Λ$xs. $es"
            (true, newSize, if (wrapped) wrappedLine("[", dls, "]") else line(dls))
        }
      } else (false, newSize, block(s"Λ$xs", er))
    }
    case DepApp(f, x) => {
      val (fInline, fSize, fr) = f match {
        case _: DepLambda[_] => renderASTCompact(f, wrapped = true)
        case _ => renderASTCompact(f)
      }
      val xs = x.toString
      val newSize = fSize + 1
      if ((inlineSize > 0) && fInline) {
        crush(fr) match {
          case Line(fs) =>
            val das = s"$fs $xs"
            (true, newSize, if (wrapped) wrappedLine("(", das, ")") else line(das))
        }
      } else (false, newSize, line(xs) <+: unwrap(fr))
    }
    case l @ Literal(d) => (true, dataSize(d), line(l.toString))
    case p: Primitive => (true, 1, line(p.toString))
  }
  case class WrappedLine(l: String, s: String, r: String) extends RenderUnicodeDraw {
    def draw(cxt: Cxt)(implicit cfg: UnicodeConfig) =
      UnicodeDraw(Seq.empty, Seq.empty, Seq(cfg.wireHSym +: Seq(l + s + r)), Seq.empty)
  }
  def wrappedLine(l: String, s: String, r: String) = WrappedLine(l, s, r)
  def unwrap(d: RenderUnicodeDraw): RenderUnicodeDraw = d match {
    case WrappedLine(_, s, _) => Line(s)
    case d => d
  }
  def crush(d: RenderUnicodeDraw): RenderUnicodeDraw = d match {
    case WrappedLine(l, s, r) => Line(l + s + r)
    case d => d
  }
  def dataSize(d: Data): Int = d match {
    case VectorData(v) => v.length
    case ArrayData(a) => a.foldLeft(0)((acc, now) => acc + dataSize(now))
    case PairData(p1, p2) => dataSize(p1) + dataSize(p2)
    case _ => 1
  }
}
