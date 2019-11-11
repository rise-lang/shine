package lift.core

object DrawTree {
  case class UnicodeConfig
  (wireHSym: String, wireVSym: String,
   connLSym: String, connLLSym: String, connRSym: String, connRRSym: String,
   spaceSym: String,
   convLSym: String, convRSym: String, convLRSym: String, convUSym: String)

  def genUnicodeConfig(s: String): UnicodeConfig = {
    if (s.length < 11) sys.error("not a valid Unicode Config")
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
  def drawAST(e: Expr): String = drawTree(ast(e))
  def drawAST(cfg: UnicodeConfig, e: Expr): String = drawTree(cfg, ast(e))
  def ast(e: Expr): RenderUnicodeDraw = e match {
      case i: Identifier => line(i.name)
      case Lambda(x, e) => block(s"λ${x.name}", ast(e))
      case App(f, e) => ast(f) :+> ast(e)
      case dl @ DepLambda(x, e) => block(s"Λ${x.name}:${dl.kn.get}", ast(e))
      case DepApp(f, x) => line(x.toString) <+: ast(f)
      case l: Literal => line(l.toString)
      case p: Primitive => line(p.toString)
  }
}

object LessBrackets {
  def lessBrackets(e: Expr): String = lessBrackets(e, wrapped = false)
  def lessBrackets(e: Expr, wrapped: Boolean): String = e match {
    case i: Identifier => i.name
    case Lambda(x, e) => {
      val sx = lessBrackets(x)
      val se = lessBrackets(e)
      if (wrapped) s"<λ$sx. $se>" else s"λ$sx. $se"
    }
    case App(f, e) => {
      val sf = f match {
        case _: Lambda => lessBrackets(f, wrapped = true)
        case _ => lessBrackets(f)
      }
      val se = lessBrackets(e, wrapped = true)
      if (wrapped) s"($sf $se)" else s"$sf $se"
    }
    case dl @ DepLambda(x, e) => {
      val sx = s"${x.name}: ${dl.kn.get}"
      val se = lessBrackets(e)
      if (wrapped) s"[Λ$sx. $se]" else s"Λ$sx. $se"
    }
    case DepApp(f, x) => {
      val sf = f match {
        case _: DepLambda[_] => lessBrackets(f, wrapped = true)
        case _ => lessBrackets(f)
      }
      if (wrapped) s"($sf $x)" else s"$sf $x"
    }
    case l: Literal => l.toString
    case p: Primitive => p.toString
  }
}

object ShowLift {
  import lift.core.DrawAST._
  import lift.core.LessBrackets._
  def showLift(e: Expr): String = if (size(e) >= 20) drawAST(e) else lessBrackets(e)
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
