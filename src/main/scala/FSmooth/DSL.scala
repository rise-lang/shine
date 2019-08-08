package FSmooth

object DSL {
  implicit final class TypeConstructorsForward(private val t: ExpressionType) extends AnyVal {
    @inline def ->(r: Type): IncompleteFunType = IncompleteFunType(t, r)
    @inline def ->(r: ExpressionType): FunType = FunType(t, r)
  }

  implicit final class SeqTypeConstructorsForward(private val pt: IncompleteFunType) extends AnyVal {
    @inline def ->(r: Type): IncompleteFunType = IncompleteFunType(pt, r)
    @inline def ->(r: ExpressionType): FunType = FunType(pt, r)
  }
  implicit final class FunTypeConstructorsForward(private val t: FunType) extends AnyVal {
    @inline def ->(r: Type): IncompleteFunType = IncompleteFunType(IncompleteFunType(t.inT, t.outT), r)
    @inline def ->(r: ExpressionType): FunType = FunType(IncompleteFunType(t.inT, t.outT), r)
  }

  implicit final class TupleTypeConstructors(private val a: ExpressionType) extends AnyVal {
    @inline def x(b: ExpressionType): Pair = Pair(a, b)
  }

  implicit class Apply(f: Expr) {
    def apply(e: Expr): Expr = lifting.liftFunExpr(f).value(Seq(e))
    def apply(e1: Expr, e2: Expr): Expr = lifting.liftFunExpr(f).value(Seq(e1, e2))
    def apply(e1: Expr, e2: Expr, e3: Expr): Expr = lifting.liftFunExpr(f).value(Seq(e1, e2, e3))
    def apply(e1: Expr, e2: Expr, e3: Expr, e4: Expr): Expr = lifting.liftFunExpr(f).value(Seq(e1, e2, e3, e4))
    def applySeq(es: Seq[Expr]): Expr = lifting.liftFunExpr(f).value(es)
  }

  implicit class Get(e0: Expr) {
    def get(e1: Expr): Expr = VectorFunctionConstants.get(freshTypeVar)(e0, e1)
  }

  implicit class Bob(lhs: Expr) {
    def +(rhs: Expr): Expr = ScalarFunctionConstants.`+`(freshTypeVar)(lhs, rhs)
    def -(rhs: Expr): Expr = ScalarFunctionConstants.`-`(freshTypeVar)(lhs, rhs)
    def *(rhs: Expr): Expr = ScalarFunctionConstants.`*`(freshTypeVar)(lhs, rhs)
    def /(rhs: Expr): Expr = ScalarFunctionConstants.`/`(freshTypeVar)(lhs, rhs)
    def **(rhs: Expr): Expr = ScalarFunctionConstants.`**`(freshTypeVar)(lhs, rhs)
    def >(rhs: Expr): Expr = ScalarFunctionConstants.`>`(freshTypeVar)(lhs, rhs)
    def <(rhs: Expr): Expr = ScalarFunctionConstants.`<`(freshTypeVar)(lhs, rhs)
    def =:=(rhs: Expr): Expr = ScalarFunctionConstants.`=:=`(freshTypeVar)(lhs, rhs)
    def <>(rhs: Expr): Expr = ScalarFunctionConstants.`<>`(freshTypeVar)(lhs, rhs)
    def &&(rhs: Expr): Expr = ScalarFunctionConstants.`&&`(freshTypeVar)(lhs, rhs)
    def ||(rhs: Expr): Expr = ScalarFunctionConstants.`||`(freshTypeVar)(lhs, rhs)
  }

  //noinspection ApparentResultTypeRefinement
  object `if` {
    def apply(cond: Expr): Object {
      def `then`(thenBranch: Expr): Object {
        def `else` (elseBranch: Expr): Conditional
      }
    } = new {
      def `then`(thenBranch: Expr): Object {
        def `else` (elseBranch: Expr): Conditional
      } = new {
        def `else`(elseBranch: Expr) = Conditional(cond, thenBranch, elseBranch)
      }
    }
  }

  def Vector = Array(Double)
  def Matrix = Array(Array(Double))
  def DoubleD = Pair(Double, Double)
  def VectorD = Array(Pair(Double, Double))
  def MatrixD = Array(Array(Pair(Double, Double)))

  def freshName: String => String = lift.core.freshName.apply
  def freshTypeVar: TypeVar = TypeVar(DSL.freshName("T"))
  def freshExprTypeVar: ExpressionTypeVar = ExpressionTypeVar(DSL.freshName("T"))

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

    def apply(p: (Seq[Identifier], Expr)) = Abstraction(p._1, p._2)
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

  def build = VectorFunctionConstants.build(freshTypeVar)
  def ifold = VectorFunctionConstants.ifold(freshTypeVar)
  def get = VectorFunctionConstants.get(freshTypeVar)
  def len = VectorFunctionConstants.length(freshTypeVar)

  def pair = PairFunctionConstants.pair(freshTypeVar)
  def fst = PairFunctionConstants.fst(freshTypeVar)
  def snd = PairFunctionConstants.snd(freshTypeVar)

  def sign = ScalarFunctionConstants.sign(freshTypeVar)
  def cos = ScalarFunctionConstants.cos(freshTypeVar)
  def tan = ScalarFunctionConstants.tan(freshTypeVar)
  def log = ScalarFunctionConstants.log(freshTypeVar)
  def exp = ScalarFunctionConstants.exp(freshTypeVar)
  def sqrt = ScalarFunctionConstants.sqrt(freshTypeVar)
}

