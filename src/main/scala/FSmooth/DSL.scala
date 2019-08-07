package FSmooth

object DSL {
  implicit final class TypeConstructorsForward(private val t: ExpressionType) extends AnyVal {
    @inline def ->(r: Type): PartialFunType = PartialFunType(t, r)
    @inline def ->(r: ExpressionType): FunType = FunType(t, r)
  }

  implicit final class SeqTypeConstructorsForward(private val pt: PartialFunType) extends AnyVal {
    @inline def ->(r: Type): PartialFunType = PartialFunType(pt, r)
    @inline def ->(r: ExpressionType): FunType = FunType(pt, r)
  }
  implicit final class FunTypeConstructorsForward(private val t: FunType) extends AnyVal {
    @inline def ->(r: Type): PartialFunType = PartialFunType(PartialFunType(t.inT, t.outT), r)
    @inline def ->(r: ExpressionType): FunType = FunType(PartialFunType(t.inT, t.outT), r)
  }

  implicit final class TupleTypeConstructors(private val a: ExpressionType) extends AnyVal {
    @inline def x(b: ExpressionType): Pair = Pair(a, b)
  }

  implicit class Apply(f: Expr) {
    def apply(e: Expr): Expr = lifting.liftFunExpr(f).value(Seq(e))
    def apply(e1: Expr, e2: Expr): Expr = lifting.liftFunExpr(f).value(Seq(e1, e2))
    def apply(e1: Expr, e2: Expr, e3: Expr): Expr = lifting.liftFunExpr(f).value(Seq(e1, e2, e3))
    def apply(es: Seq[Expr]): Expr = lifting.liftFunExpr(f).value(es)
  }

  implicit class Get(e0: Expr) {
    def get(e1: Expr): Expr = VectorFunctionConstants.get(None)(e0, e1)
  }

  implicit class Bob(lhs: Expr) {
    def +(rhs: Expr): Expr = ScalarFunctionConstants.`+`(None)(lhs, rhs)
    def -(rhs: Expr): Expr = ScalarFunctionConstants.`-`(None)(lhs, rhs)
    def *(rhs: Expr): Expr = ScalarFunctionConstants.`*`(None)(lhs, rhs)
    def /(rhs: Expr): Expr = ScalarFunctionConstants.`/`(None)(lhs, rhs)
    def **(rhs: Expr): Expr = ScalarFunctionConstants.`**`(None)(lhs, rhs)
    def >(rhs: Expr): Expr = ScalarFunctionConstants.`>`(None)(lhs, rhs)
    def <(rhs: Expr): Expr = ScalarFunctionConstants.`<`(None)(lhs, rhs)
    def =:=(rhs: Expr): Expr = ScalarFunctionConstants.`=:=`(None)(lhs, rhs)
    def <>(rhs: Expr): Expr = ScalarFunctionConstants.`<>`(None)(lhs, rhs)
    def &&(rhs: Expr): Expr = ScalarFunctionConstants.`&&`(None)(lhs, rhs)
    def ||(rhs: Expr): Expr = ScalarFunctionConstants.`||`(None)(lhs, rhs)
  }

  def Vector = Array(Double)
  def Matrix = Array(Array(Double))
  def DoubleD = Pair(Double, Double)
  def VectorD = Array(Pair(Double, Double))
  def MatrixD = Array(Array(Pair(Double, Double)))

  def freshName: String => String = lift.core.freshName.apply

  def implM(f: ExpressionTypeVar => Type): Type = {
    f(ExpressionTypeVar(freshName("M")))
  }

  def implNum(f: ExpressionTypeVar => Type): Type = {
    f(ExpressionTypeVar(freshName("Num")))
  }

  object fun {
    def apply(f: Identifier => Expr): Abstraction = {
      val e = Identifier(freshName("e"))
      Abstraction(Seq(e), f(e))
    }

    def apply(f: (Identifier, Identifier) => Expr): Abstraction = {
      val e0 = Identifier(freshName("e"))
      val e1 = Identifier(freshName("e"))
      Abstraction(Seq(e0, e1), f(e0, e1))
    }

    def apply(f: (Identifier, Identifier, Identifier) => Expr): Abstraction = {
      val e0 = Identifier(freshName("e"))
      val e1 = Identifier(freshName("e"))
      val e2 = Identifier(freshName("e"))
      Abstraction(Seq(e0, e1, e2), f(e0, e1, e2))
    }

    def apply(f: (Identifier, Identifier, Identifier, Identifier) => Expr): Abstraction = {
      val e0 = Identifier(freshName("e"))
      val e1 = Identifier(freshName("e"))
      val e2 = Identifier(freshName("e"))
      val e3 = Identifier(freshName("e"))
      Abstraction(Seq(e0, e1, e2, e3), f(e0, e1, e2, e3))
    }
  }

  object let {
    def apply(init: Expr): Object {
      def beIn(f: Identifier => Expr): Let
    } = new {
      def beIn(f: Identifier => Expr): Let = {
        val e = Identifier(freshName("e"))
        Let(e, init, f(e))
      }
    }
  }

  object card {
    def apply(N: Int): CardinalityValue = CardinalityValue(N)
  }

  object idx {
    def apply(i: Int): IndexValue = IndexValue(i)
  }

  object scalar {
    def apply(d: Double): ScalarValue = ScalarValue(d)
  }

  def build = VectorFunctionConstants.build(None)
  def ifold = VectorFunctionConstants.ifold(None)
  def get = VectorFunctionConstants.get(None)
  def len = VectorFunctionConstants.length(None)

  def pair = PairFunctionConstants.pair(None)
  def fst = PairFunctionConstants.fst(None)
  def snd = PairFunctionConstants.snd(None)

  def sign = ScalarFunctionConstants.sign(None)
  def cos = ScalarFunctionConstants.cos(None)
  def tan = ScalarFunctionConstants.tan(None)
  def log = ScalarFunctionConstants.log(None)
  def exp = ScalarFunctionConstants.exp(None)
}

