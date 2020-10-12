package rise.core

import rise.core.semantics._
import rise.core.types._

import scala.language.implicitConversions

// scalastyle:off number.of.methods indentation
object DSL {

  def identifier(name: String): Identifier = Identifier(name)()
  def lambda(x: Identifier, e: Expr): Lambda = Lambda(x, e)()
  def app(f: Expr, e: Expr): App = App(f, e)()
  def depLambda[K <: Kind: KindName](x: K#I with Kind.Explicitness,
                                     e: Expr
                                    ): DepLambda[K] = DepLambda[K](x, e)()
  def depApp[K <: Kind](f: Expr, x: K#T): DepApp[K] = DepApp[K](f, x)()
  def literal(d: semantics.Data): Literal = Literal(d)

  def toMem: Primitive = primitives.toMem.primitive
  def toMemFun(f: Expr): Expr = fun(x => toMem(f(x)))
  def makeArray(n: Int): Primitive = primitives.makeArray(n).primitive
  def cast: Primitive = primitives.cast.primitive
  def depJoin: Primitive = primitives.depJoin.primitive
  def depMapSeq: Primitive = primitives.depMapSeq.primitive
  def depZip: Primitive = primitives.depZip.primitive
  def drop: Primitive = primitives.drop.primitive
  def fst: Primitive = primitives.fst.primitive
  def gather: Primitive = primitives.gather.primitive
  def scatter: Primitive = primitives.scatter.primitive
  def generate: Primitive = primitives.generate.primitive
  def idx: Primitive = primitives.idx.primitive
  def id: Primitive = primitives.id.primitive
  def indexAsNat: Primitive = primitives.indexAsNat.primitive
  def iterate: Primitive = primitives.iterate.primitive
  def join: Primitive = primitives.join.primitive
  def map: Primitive = primitives.map.primitive
  def mapFst: Primitive = primitives.mapFst.primitive
  def mapSnd: Primitive = primitives.mapSnd.primitive
  def mapSeq: Primitive = primitives.mapSeq.primitive
  def mapStream: Primitive = primitives.mapStream.primitive
  def iterateStream: Primitive = primitives.iterateStream.primitive
  def mapSeqUnroll: Primitive = primitives.mapSeqUnroll.primitive
  def natAsIndex: Primitive = primitives.natAsIndex.primitive
  def padCst: Primitive = primitives.padCst.primitive
  def padEmpty: Primitive = primitives.padEmpty.primitive
  def padClamp: Primitive = primitives.padClamp.primitive
  def partition: Primitive = primitives.partition.primitive
  def pair: Primitive = primitives.pair.primitive
  def reduce: Primitive = primitives.reduce.primitive
  def reduceSeq: Primitive = primitives.reduceSeq.primitive
  def reduceSeqUnroll: Primitive = primitives.reduceSeqUnroll.primitive
  def reorder: Primitive = primitives.reorder.primitive
  def scanSeq: Primitive = primitives.scanSeq.primitive
  def slide: Primitive = primitives.slide.primitive
  def circularBuffer: Primitive = primitives.circularBuffer.primitive
  def rotateValues: Primitive = primitives.rotateValues.primitive
  def snd: Primitive = primitives.snd.primitive
  def split: Primitive = primitives.split.primitive
  def take: Primitive = primitives.take.primitive
  def transpose: Primitive = primitives.transpose.primitive
  def select: Primitive = primitives.select.primitive
  def unzip: Primitive = primitives.unzip.primitive
  def zip: Primitive = primitives.zip.primitive

  def neg: Primitive = primitives.neg.primitive
  def not: Primitive = primitives.not.primitive
  def add: Primitive = primitives.add.primitive
  def sub: Primitive = primitives.sub.primitive
  def mul: Primitive = primitives.mul.primitive
  def div: Primitive = primitives.div.primitive
  def mod: Primitive = primitives.mod.primitive
  def gt: Primitive = primitives.gt.primitive
  def lt: Primitive = primitives.lt.primitive
  def equal: Primitive = primitives.equal.primitive

  def asVector: Primitive = primitives.asVector.primitive
  def asVectorAligned: Primitive = primitives.asVectorAligned.primitive
  def asScalar: Primitive = primitives.asScalar.primitive
  def vectorFromScalar: Primitive = primitives.vectorFromScalar.primitive

  def printType(msg: String): Primitive = primitives.printType(msg).primitive
  def typeHole(msg: String): Primitive = primitives.typeHole(msg).primitive

  // scalastyle:off method.name
  implicit class Ops(lhs: Expr) {
    // binary
    def +(rhs: Expr): Expr = add(lhs)(rhs)
    def -(rhs: Expr): Expr = sub(lhs)(rhs)
    def *(rhs: Expr): Expr = mul(lhs)(rhs)
    def /(rhs: Expr): Expr = div(lhs)(rhs)
    def %(rhs: Expr): Expr = mod(lhs)(rhs)
    def >(rhs: Expr): Expr = gt(lhs)(rhs)
    def >=(rhs: Expr): Expr = not(lhs < rhs) // TODO: dedicated primitive?
    def <(rhs: Expr): Expr = lt(lhs)(rhs)
    def <=(rhs: Expr): Expr = not(lhs > rhs) // TODO: dedicated primitive?
    def =:=(rhs: Expr): Expr = equal(lhs)(rhs)

    // scalastyle:off disallow.space.before.token
    // unary
    def unary_- : Expr = neg(lhs)
    def unary_! : Expr = not(lhs)
    // scalastyle:on disallow.space.before.token

    // pair accesses
    def _1: Expr = fst(lhs)
    def _2: Expr = snd(lhs)
  }


  implicit class Indexing(e: Expr) {
    def `@`(i: Expr): Expr = idx(i)(e)
  }

  /*
  implicit class TypeAnnotation(t: Type) {
    def ::(e: Expr): Expr =
      if (e.t == TypePlaceholder) e.setType(t)
      else if (e.t == t) e else
        throw TypeException(s"tried to replace ${e.t} with ${t}, but type annotation can only replace a TypePlaceholder")
    def `:`(e: Expr): Expr = e :: t
  }
   */

  implicit class TypeAnnotationHelper(t: Type) {
    def ::(e: Expr): Expr = TypeAnnotation(e, t)
    def `:`(e: Expr): Expr = e :: t
  }
  // scalastyle:on method.name

  implicit class FunCall(f: Expr) {

    def apply(e: Expr): Expr = app(f, e)
    def apply(n: Nat): Expr = depApp[NatKind](f, n)
    def apply(dt: DataType): Expr = depApp[DataKind](f, dt)
    def apply(a: AddressSpace): Expr = depApp[AddressSpaceKind](f, a)

    def apply(n2n: NatToNat): Expr = depApp[NatToNatKind](f, n2n)

    def apply(e1: Expr, e2: Expr): Expr = {
      f(e1)(e2)
    }

    def apply(e1: Expr, e2: Expr, e3: Expr): Expr = {
      f(e1)(e2)(e3)
    }

    def apply(e1: Expr, e2: Expr, e3: Expr, e4: Expr): Expr = {
      f(e1)(e2)(e3)(e4)
    }

    def apply(e1: Expr, e2: Expr, e3: Expr, e4: Expr, e5: Expr): Expr = {
      f(e1)(e2)(e3)(e4)(e5)
    }
  }

  // scalastyle:off method.name
  implicit class FunPipe(e: Expr) {
    def |>(f: Expr): Expr = f.apply(e)
  }

  implicit class FunPipeReverse(f: Expr) {
    def $(e: Expr): Expr = f.apply(e)
  }

  implicit class FunComp(f: Expr) {
    def >>(g: Expr): Expr = fun(x => g(f(x)))
  }

  implicit class FunCompReverse(f: Expr) {
    def o(g: Expr): Expr = fun(x => f(g(x)))
  }
  // scalastyle:on method.name

  // noinspection ScalaUnusedSymbol
  // scalastyle:off structural.type
  object let {
    def apply(e: Expr): Object {
      def be(in: Expr => Expr): Expr
    } = new {
      def be(in: Expr => Expr): Expr =
        primitives.let.primitive.apply(e).apply(fun(in))
    }
  }
  // scalastyle:on structural.type

  object letf {
    def apply(in: Expr => Expr): Expr = {
      fun(e => primitives.let.primitive.apply(e).apply(fun(in)))
    }
    def apply(in: Expr): Expr = {
      fun(e => primitives.let.primitive.apply(e).apply(in))
    }
  }

  // noinspection DuplicatedCode
  // function values
  object fun {
    def apply(t: Type)(f: Expr => Expr): Expr = {
      val x = identifier(freshName("e")).setType(t)
      lambda(x, f(x))
    }

    def apply(f: Identifier => Expr): Expr = untyped(f)
    def apply(f: (Identifier, Identifier) => Expr): Expr = untyped(f)
    def apply(f: (Identifier, Identifier, Identifier) => Expr): Expr =
      untyped(f)
    def apply(
        f: (Identifier, Identifier, Identifier, Identifier) => Expr
    ): Expr = untyped(f)
    def apply(
        f: (Identifier, Identifier, Identifier, Identifier, Identifier) => Expr
    ): Expr = untyped(f)
    def apply(
        f: (
            Identifier,
            Identifier,
            Identifier,
            Identifier,
            Identifier,
            Identifier
        ) => Expr
    ): Expr = untyped(f)

    private def untyped(f: Identifier => Expr): Expr = {
      val e = identifier(freshName("e"))
      lambda(e, f(e))
    }

    private def untyped(f: (Identifier, Identifier) => Expr): Expr = {
      val e = identifier(freshName("e"))
      lambda(e, untyped(e1 => f(e, e1)))
    }

    private def untyped(
        f: (Identifier, Identifier, Identifier) => Expr
    ): Expr = {
      val e = identifier(freshName("e"))
      lambda(e, untyped((e1, e2) => f(e, e1, e2)))
    }

    private def untyped(
        f: (Identifier, Identifier, Identifier, Identifier) => Expr
    ): Expr = {
      val e = identifier(freshName("e"))
      lambda(e, untyped((e1, e2, e3) => f(e, e1, e2, e3)))
    }

    private def untyped(
        f: (Identifier, Identifier, Identifier, Identifier, Identifier) => Expr
    ): Expr = {
      val e = identifier(freshName("e"))
      lambda(e, untyped((e1, e2, e3, e4) => f(e, e1, e2, e3, e4)))
    }

    private def untyped(
        f: (
            Identifier,
            Identifier,
            Identifier,
            Identifier,
            Identifier,
            Identifier
        ) => Expr
    ): Expr = {
      val e = identifier(freshName("e"))
      lambda(e, untyped((e1, e2, e3, e4, e5) => f(e, e1, e2, e3, e4, e5)))
    }

    // noinspection TypeAnnotation
    // scalastyle:off structural.type
    def apply(ft: FunType[Type, Type]): Object {
      def apply(f: (Identifier, Identifier, Identifier,
        Identifier, Identifier, Identifier) => Expr): Expr

      def apply(f: (Identifier, Identifier, Identifier,
        Identifier, Identifier) => Expr): Expr

      def apply(f: (Identifier, Identifier, Identifier,
        Identifier) => Expr): Expr

      def apply(f: (Identifier, Identifier, Identifier) => Expr): Expr

      def apply(f: (Identifier, Identifier) => Expr): Expr

      def apply(f: Identifier => Expr): Expr
    } = new {
      def apply(f: Identifier => Expr): Expr = untyped(f) :: ft
      def apply(f: (Identifier, Identifier) => Expr): Expr = untyped(f) :: ft
      def apply(f: (Identifier, Identifier, Identifier) => Expr): Expr =
        untyped(f) :: ft
      def apply(
          f: (Identifier, Identifier, Identifier, Identifier) => Expr
      ): Expr = untyped(f) :: ft
      def apply(
          f: (
              Identifier,
              Identifier,
              Identifier,
              Identifier,
              Identifier
          ) => Expr
      ): Expr = untyped(f) :: ft
      def apply(
          f: (
              Identifier,
              Identifier,
              Identifier,
              Identifier,
              Identifier,
              Identifier
          ) => Expr
      ): Expr = untyped(f) :: ft
    }
    // scalastyle:on  structural.type
  }

  // noinspection DuplicatedCode
  object nFun {
    def apply(
        r: arithexpr.arithmetic.Range,
        f: NatIdentifier => Expr
    ): DepLambda[NatKind] = {
      val x = NatIdentifier(freshName("n"), r, isExplicit = true)
      depLambda[NatKind](x, f(x))
    }

    def apply(f: NatIdentifier => Expr): DepLambda[NatKind] = {
      nFun(arithexpr.arithmetic.RangeAdd(0, arithexpr.arithmetic.PosInf, 1), f)
    }

    def apply(f: (NatIdentifier, NatIdentifier) => Expr): DepLambda[NatKind] = {
      val r = arithexpr.arithmetic.RangeAdd(0, arithexpr.arithmetic.PosInf, 1)
      val n = NatIdentifier(freshName("n"), r, isExplicit = true)
      depLambda[NatKind](n, nFun(f(n, _)))
    }

    def apply(
        f: (NatIdentifier, NatIdentifier, NatIdentifier) => Expr
    ): DepLambda[NatKind] = {
      val r = arithexpr.arithmetic.RangeAdd(0, arithexpr.arithmetic.PosInf, 1)
      val n = NatIdentifier(freshName("n"), r, isExplicit = true)
      depLambda[NatKind](n, nFun((n1, n2) => f(n, n1, n2)))
    }

    def apply(
        f: (NatIdentifier, NatIdentifier, NatIdentifier, NatIdentifier) => Expr
    ): DepLambda[NatKind] = {
      val r = arithexpr.arithmetic.RangeAdd(0, arithexpr.arithmetic.PosInf, 1)
      val n = NatIdentifier(freshName("n"), r, isExplicit = true)
      depLambda[NatKind](n, nFun((n1, n2, n3) => f(n, n1, n2, n3)))
    }

    def apply(
        f: (
            NatIdentifier,
            NatIdentifier,
            NatIdentifier,
            NatIdentifier,
            NatIdentifier
        ) => Expr
    ): DepLambda[NatKind] = {
      val r = arithexpr.arithmetic.RangeAdd(0, arithexpr.arithmetic.PosInf, 1)
      val n = NatIdentifier(freshName("n"), r, isExplicit = true)
      depLambda[NatKind](n, nFun((n1, n2, n3, n4) => f(n, n1, n2, n3, n4)))
    }
  }

  object dtFun {
    def apply(f: DataTypeIdentifier => Expr): DepLambda[DataKind] = {
      val x = DataTypeIdentifier(freshName("dt"), isExplicit = true)
      depLambda[DataKind](x, f(x))
    }
  }

  implicit def wrapInNatExpr(n: Nat): Literal = literal(NatData(n))

  def l(i: Int): Literal = literal(IntData(i))
  def l(f: Float): Literal = literal(FloatData(f))
  def l(d: Double): Literal = literal(DoubleData(d))
  def lidx(i: Nat, n: Nat): Literal = literal(IndexData(i, n))
  def lvec(v: Seq[ScalarData]): Literal = literal(VectorData(v))
  def larr(a: Seq[Data]): Literal = literal(ArrayData(a))

  object foreignFun {
    def apply(name: String, t: Type): Expr = {
      ForeignFunction(ForeignFunction.Decl(name, None))(t)
    }

    def apply(
        name: String,
        params: Seq[String],
        body: String,
        t: Type
    ): Expr = {
      ForeignFunction(
        ForeignFunction.Decl(name, Some(ForeignFunction.Def(params, body)))
      )(t)
    }
  }
}
// scalastyle:on number.of.methods indentation
