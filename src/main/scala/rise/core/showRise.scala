package rise.core

import rise.core.DrawTree.UnicodeConfig
import rise.core.primitives.Annotation

case class RenderException(msg: String) extends Exception {
  override def toString = s"render exception: $msg"
}

object DrawTree {
  case class UnicodeConfig
  (wireHSym: String, wireVSym: String,
   connLSym: String, connLLSym: String, connRSym: String, connRRSym: String,
   spaceSym: String,
   convLSym: String, convRSym: String, convLRSym: String, convUSym: String)

  object UnicodeConfig {
    def apply(s: String): UnicodeConfig = {
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
  }

  val defaultUnicodeConfig: UnicodeConfig = UnicodeConfig("│┌├└├╩╦╬═ ─")

  case class RenderUnicodeDraw(farLeft: Seq[Seq[String]],
                               nearLeft: Seq[Seq[String]],
                               nearRight: Seq[Seq[String]],
                               farRight: Seq[Seq[String]]) {
    override def toString: String = {
      (farLeft.map(_.mkString) ++ nearLeft.map(_.mkString)
        ++ nearRight.map(_.mkString) ++ farRight.map(_.mkString)).mkString("\n")
    }
  }

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

  abstract class UnicodeDraw {
    def render(cxt: Cxt)(implicit cfg: UnicodeConfig) : RenderUnicodeDraw
    def show(implicit cfg: UnicodeConfig = defaultUnicodeConfig): String = render(U).toString
  }

  case class Line(s: String) extends UnicodeDraw {
    def render(cxt: Cxt)(implicit cfg: UnicodeConfig): RenderUnicodeDraw =
      RenderUnicodeDraw(Seq.empty, Seq.empty, Seq(cfg.wireHSym +: Seq(s)), Seq.empty)
  }

  def line(s: String): Line = Line(s)

  case class AddLeft(a: UnicodeDraw, b: UnicodeDraw) extends UnicodeDraw {
    def render(cxt: Cxt)(implicit cfg: UnicodeConfig): RenderUnicodeDraw = {
      val ra = a.render(mergeCxt(cxt, U))
      val rb = b.render(mergeCxt(cxt, L))
      val wireV = cfg.wireVSym
      val space = cfg.spaceSym
      val conn = if (cxt == L || cxt == LR) cfg.connLLSym else cfg.connLSym
      val wireL = if (cxt == L || cxt == LR) wireV else space
      val fl = ra.farLeft.map(wireL +: _) ++ ra.nearLeft.map(wireL +: _)
      val nl = (((conn +: ra.nearRight.head) +: ra.nearRight.tail.map(wireV +: _))
        ++ ra.farRight.map(wireV +: _) ++ rb.farLeft ++ rb.nearLeft)
      val nr = rb.nearRight
      val fr = rb.farRight
      RenderUnicodeDraw(fl, nl, nr, fr)
    }
  }

  case class AddRight(a: UnicodeDraw, b: UnicodeDraw) extends UnicodeDraw {
    def render(cxt: Cxt)(implicit cfg: UnicodeConfig): RenderUnicodeDraw = {
      val ra = a.render(mergeCxt(cxt, R))
      val rb = b.render(mergeCxt(cxt, U))
      val wireV = cfg.wireVSym
      val space = cfg.spaceSym
      val conn = if (cxt == R || cxt == LR) cfg.connRRSym else cfg.connRSym
      val wireR = if (cxt == R || cxt == LR) wireV else space
      val fl = ra.farLeft
      val nl = ra.nearLeft
      val nr = (ra.nearRight ++ ra.farRight ++ rb.farLeft.map(wireV +: _)
          ++ rb.nearLeft.map(wireV +: _))
      val fr = (((conn +: rb.nearRight.head) +: rb.nearRight.tail.map(wireR +: _))
        ++ rb.farRight.map(wireR +: _))
      RenderUnicodeDraw(fl, nl, nr, fr)
    }
  }

  implicit class AddLeftOp(a: UnicodeDraw) {
    def <+:(b: UnicodeDraw): UnicodeDraw = AddLeft(b, a)
  }

  implicit class AddRightOp(a: UnicodeDraw) {
    def :+>(b: UnicodeDraw): UnicodeDraw = AddRight(a, b)
  }

  case class Block(a: UnicodeDraw) extends UnicodeDraw {
    def render(cxt: Cxt)(implicit cfg: UnicodeConfig): RenderUnicodeDraw = {
      val ra = a.render(mergeCxt(cxt, U))
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
      val fl = ra.farLeft.map(wireL +: _) ++ ra.nearLeft.map(wireL +: _)
      val nl = Seq.empty
      val nr = Seq(wireH +: (conv +: ra.nearRight.head))
      val fr = ra.nearRight.tail.map(wireR +: _) ++ ra.farRight.map(wireR +: _)
      RenderUnicodeDraw(fl, nl, nr, fr)
    }
  }

  def block(a: UnicodeDraw): Block = Block(a)

  case class NamedBlock(n: String, a: UnicodeDraw) extends UnicodeDraw {
    def render(cxt: Cxt)(implicit cfg: UnicodeConfig): RenderUnicodeDraw = {
      val ra = a.render(mergeCxt(cxt, U))
      val wireV = cfg.wireVSym
      val wireH = cfg.wireHSym
      val space = cfg.spaceSym
      val extraSpace = space.substring(0,1) * (n.length - 1)
      val wireL = if (cxt == L || cxt == LR) wireV else space
      val wireR = if (cxt == R || cxt == LR) wireV else space
      val fl = (ra.farLeft.map(x => wireL +: (extraSpace +: x))
        ++ ra.nearLeft.map(x => wireL +: (extraSpace +: x)))
      val nl = Seq.empty
      val nr = Seq(wireH +: (n +: ra.nearRight.head))
      val fr = (ra.nearRight.tail.map(x => wireR +: (extraSpace +: x))
        ++ ra.farRight.map(x => wireR +: (extraSpace +: x)))
      RenderUnicodeDraw(fl, nl, nr, fr)
    }
  }

  def block(n: String, a: UnicodeDraw): NamedBlock = NamedBlock(n, a)
}

class ShowRiseCompact {
  import rise.core.DrawTree._
  import rise.core.semantics._

  def drawAST(expr: Expr, wrapped: Boolean = false)
             (implicit inlineSize: Int): (Boolean, Int, UnicodeDraw) = expr match {

    case i: Identifier => (true, 1, line(i.name))

    case Lambda(x, e) =>
      val xs = x.name
      val (eInline, eSize, er) = drawAST(e)
      val newSize = eSize + 1
      if ((inlineSize > 0) && eInline) {
        (true, newSize, er >~> (es => {
          val ls = s"λ$xs. $es"
          if (wrapped) wrappedLine("<", ls, ">") else line(ls)
        }))
      } else (false, newSize, er >@> (ed => block(s"λ$xs", ed)))

    case App(f, e) =>
      val (fInline, fSize, fr) = f match {
        case _: Lambda => drawAST(f, wrapped = true)
        case _ => drawAST(f)
      }
      val (_, eSize, er) = drawAST(e, wrapped = true)
      val newSize = fSize + eSize
      if ((inlineSize > 0) && ((fInline && (eSize == 1)) || (newSize <= inlineSize))) {
        (true, newSize, fr >~> (fs => er >~> (es => {
          val as = s"$fs $es"
          if (wrapped) wrappedLine("(", as, ")") else line(as)
        })))
      } else (false, newSize, fr >@> (fd => er >@> (ed => fd :+> ed)))

    case dl @ DepLambda(x, e) =>
      val xs = s"${x.name}:${dl.kindName}"
      val (eInline, eSize, er) = drawAST(e)
      val newSize = eSize + 1
      if ((inlineSize > 0) && eInline) {
        (true, newSize, er >~> (es => {
          val dls = s"Λ$xs. $es"
          if (wrapped) wrappedLine("[", dls, "]") else line(dls)
        }))
      } else (false, newSize, er >@> (ed => block(s"Λ$xs", ed)))

    case DepApp(f, x) =>
      val (fInline, fSize, fr) = f match {
        case _: DepLambda[_] => drawAST(f, wrapped = true)
        case _ => drawAST(f)
      }
      val xs = x.toString
      val newSize = fSize + 1
      if ((inlineSize > 0) && fInline) {
        (true, newSize, fr >~> (fs => {
          val das = s"$fs $xs"
          if (wrapped) wrappedLine("(", das, ")") else line(das)
        }))
      } else (false, newSize, fr >@> (fd => line(xs) <+: fd))

    case Literal(d) => (true, dataSize(d), line(d.toString))

    case Annotation(e, _) => drawAST(e, wrapped)

    case p: Primitive => (true, 1, line(p.name))
  }

  case class WrappedLine(l: String, s: String, r: String) extends UnicodeDraw {
    def render(cxt: Cxt)(implicit cfg: UnicodeConfig): RenderUnicodeDraw =
      RenderUnicodeDraw(Seq.empty, Seq.empty, Seq(cfg.wireHSym +: Seq(l + s + r)), Seq.empty)
  }

  def wrappedLine(l: String, s: String, r: String): WrappedLine = WrappedLine(l, s, r)

  implicit class Extract(d: UnicodeDraw) {
    def >@>(f: UnicodeDraw => UnicodeDraw): UnicodeDraw = extractUnicodeDraw(d, f)
    def >~>(f: String => UnicodeDraw): UnicodeDraw = extractString(d, f)
  }

  def extractUnicodeDraw(d: UnicodeDraw, f: UnicodeDraw => UnicodeDraw): UnicodeDraw = d match {
    case WrappedLine(_, s, _) => f(line(s))
    case d => f(d)
  }

  def extractString(d: UnicodeDraw, f: String => UnicodeDraw): UnicodeDraw = d match {
    case WrappedLine(l, s, r) => f(l + s + r)
    case Line(s) => f(s)
    case _ => f("")
  }

  def modifyString(d: UnicodeDraw, f: String => String): UnicodeDraw = d match {
    case WrappedLine(l, s, r) => wrappedLine(l, f(s), r)
    case Line(s) => line(f(s))
    case _ => d
  }

  private def dataSize(d: Data): Int = d match {
    case VectorData(v) => v.length
    case ArrayData(a) => a.foldLeft(0)((acc, now) => acc + dataSize(now))
    case PairData(p1, p2) => dataSize(p1) + dataSize(p2)
    case _ => 1
  }
}

object ShowRiseCompact {
  private val obj = new ShowRiseCompact
  def apply(expr: Expr, inlineSize: Int, cfg: UnicodeConfig): String = {
    obj.drawAST(expr)(inlineSize)._3.show(cfg)
  }
}

class ShowRiseCompactTrack extends ShowRiseCompact {
  import rise.core.DrawTree._

  var probe: Expr => Boolean = {_ => false}

  case class Track(target: UnicodeDraw) extends UnicodeDraw {
    def render(cxt: Cxt)(implicit cfg: UnicodeConfig): RenderUnicodeDraw = target.render(cxt)
  }

  def track(d: UnicodeDraw): Track = d match {
    case Track(target) => track(target)
    case _ => Track(d)
  }

  override def extractString(d: UnicodeDraw, f: String => UnicodeDraw): UnicodeDraw = d match {
    case Track(target) => extractString(target, s => track(f(s)))
    case _ => super.extractString(d, f)
  }

  override def extractUnicodeDraw(d: UnicodeDraw, f: UnicodeDraw => UnicodeDraw): UnicodeDraw = d match {
    case Track(target) => extractUnicodeDraw(target, d => track(f(track(d))))
    case _ => super.extractUnicodeDraw(d, f)
  }

  def flag(inline: Boolean, r: UnicodeDraw): Track =
    track(if (inline) modifyString(r, "⚑ " + _) else block("⚑", r))

  override def drawAST(expr: Expr, wrapped: Boolean = false)
                      (implicit inlineSize: Int): (Boolean, Int, UnicodeDraw) = {
    val (sInline, sSize, sr) = super.drawAST(expr, wrapped)(inlineSize)
    val tr = if (sr.isInstanceOf[Track]) {
      val srTarget = track(sr).target
      val r = if (sInline) srTarget else
        expr match {
          case _: App => srTarget match {
            case AddRight(a: Track, b: Track) => block("◆", a) :+> b
            case AddRight(a: Track, b) => block("▶", a) :+> b
            case AddRight(a, b: Track) => block("▼", a) :+> b
            case AddRight(_, _) =>
              throw RenderException("there should be a start point of tracking")
            case _ => throw RenderException("the way in which App is drawn is not expected")
          }
          case _ => srTarget
        }
      track(r)
    } else if (probe(expr)) flag(sInline, sr) else sr
    (sInline, sSize, tr)
  }
}

object ShowRiseCompactTrack {
  private val obj = new ShowRiseCompactTrack
  def setProbe(newProbe: Expr => Boolean): Unit = {
    obj.probe = newProbe
  }
  def apply(expr: Expr, inlineSize: Int, cfg: UnicodeConfig): String = {
    obj.drawAST(expr)(inlineSize)._3.show(cfg)
  }
  def apply(probe: Expr => Boolean, expr: Expr, inlineSize: Int, cfg: UnicodeConfig): String = {
    val temp = new ShowRiseCompactTrack
    temp.probe = probe
    temp.drawAST(expr)(inlineSize)._3.show(cfg)
  }
}

class ShowRiseCompactTrackTopDown extends ShowRiseCompactTrack {
  import rise.core.DrawTree._

  override def drawAST(expr: Expr, wrapped: Boolean = false)
                      (implicit inlineSize: Int): (Boolean, Int, UnicodeDraw) = {
    if (probe(expr)) {
      val oldProbe = probe
      probe = _ => false
      val (sInline, sSize, sr) = super.drawAST(expr, wrapped)(inlineSize)
      probe = oldProbe
      (sInline, sSize, flag(sInline, sr))
    } else {
      super.drawAST(expr, wrapped)(inlineSize)
    }
  }
}

object ShowRiseCompactTrackTopDown {
  private val obj = new ShowRiseCompactTrackTopDown
  def setProbe(newProbe: Expr => Boolean): Unit = {
    obj.probe = newProbe
  }
  def apply(expr: Expr, inlineSize: Int, cfg: UnicodeConfig): String = {
    obj.drawAST(expr)(inlineSize)._3.show(cfg)
  }
  def apply(probe: Expr => Boolean, expr: Expr, inlineSize: Int, cfg: UnicodeConfig): String = {
    val temp = new ShowRiseCompactTrackTopDown
    temp.probe = probe
    temp.drawAST(expr)(inlineSize)._3.show(cfg)
  }
}

object ShowRise {
  import rise.core.DrawTree._

  val defaultInlineSize = 10

  val spaceWireH: UnicodeConfig = UnicodeConfig("│┍┝┕┝╩╦╬═  ")

  val roundedCorner: UnicodeConfig = UnicodeConfig("│╭├╰├╩╦╬═ ─")

  def showRise(expr: Expr): String = showRiseCompactTrack(expr, cfg = spaceWireH)

  def showRiseCompact(expr: Expr, inlineSize: Int = defaultInlineSize,
                      cfg: UnicodeConfig = defaultUnicodeConfig): String =
    ShowRiseCompact(expr, inlineSize, cfg)

  def showRiseCompactTrack(expr: Expr, inlineSize: Int = defaultInlineSize,
                           cfg: UnicodeConfig = defaultUnicodeConfig): String =
    ShowRiseCompactTrack(expr, inlineSize, cfg)

  def showRiseCompactTrackTD(expr: Expr, inlineSize: Int = defaultInlineSize,
                             cfg: UnicodeConfig = defaultUnicodeConfig): String =
    ShowRiseCompactTrackTopDown(expr, inlineSize, cfg)

  def showRiseSingleLine(expr: Expr): String =
    ShowRiseCompact(expr, Int.MaxValue, defaultUnicodeConfig).tail

  def showRiseSingleLineTrack(expr: Expr): String =
    ShowRiseCompactTrack(expr, Int.MaxValue, defaultUnicodeConfig).tail

  def showRiseSingleLineTrackTD(expr: Expr): String =
    ShowRiseCompactTrackTopDown(expr, Int.MaxValue, defaultUnicodeConfig).tail

  def trackWith(probe: Expr => Boolean, expr: Expr, inlineSize: Int = defaultInlineSize,
                cfg: UnicodeConfig = defaultUnicodeConfig): String =
    ShowRiseCompactTrack(probe, expr, inlineSize, cfg)
  def setGlobalProbe(probe: Expr => Boolean): Unit = ShowRiseCompactTrack.setProbe(probe)

  def trackTDWith(probe: Expr => Boolean, expr: Expr, inlineSize: Int = defaultInlineSize,
                cfg: UnicodeConfig = defaultUnicodeConfig): String =
    ShowRiseCompactTrackTopDown(probe, expr, inlineSize, cfg)
  def setTDGlobalProbe(probe: Expr => Boolean): Unit = ShowRiseCompactTrackTopDown.setProbe(probe)
}