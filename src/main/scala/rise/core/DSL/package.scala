package rise.core

import rise.core.primitives._
import rise.core.semantics._
import rise.core.traversal.{Continue, Result}
import rise.core.types._

import scala.language.implicitConversions

// scalastyle:off number.of.methods
package object DSL {

  // DSL for Exprs

  def identifier(name: String): ToBeTyped[Identifier] =
    toBeTyped(Identifier(name)(TypePlaceholder))
  def lambda(x: ToBeTyped[Identifier], e: ToBeTyped[Expr]): ToBeTyped[Lambda] =
    x >>= (x => e >>= (e => toBeTyped(Lambda(x, e)(TypePlaceholder))))
  def app(f: ToBeTyped[Expr], e: ToBeTyped[Expr]): ToBeTyped[App] =
    f >>= (f => e >>= (e => toBeTyped(App(f, e)(TypePlaceholder))))
  def depLambda[K <: Kind: KindName](
                                      x: K#I with Kind.Explicitness,
                                      e: ToBeTyped[Expr]
                                    ): ToBeTyped[DepLambda[K]] =
    e >>= (e => toBeTyped(DepLambda[K](x, e)(TypePlaceholder)))
  def depApp[K <: Kind](f: ToBeTyped[Expr], x: K#T): ToBeTyped[DepApp[K]] =
    f >>= (f => toBeTyped(DepApp[K](f, x)(TypePlaceholder)))
  def literal(d: semantics.Data): ToBeTyped[Literal] = toBeTyped(Literal(d))

  def store(cont: ToBeTyped[Expr] => ToBeTyped[Expr]): ToBeTyped[Expr] =
    fun(e => let(toMem(e)) be cont)
  def store(how: ToBeTyped[Expr])
           (in: ToBeTyped[Expr] => ToBeTyped[Expr]): ToBeTyped[Expr] =
    fun(e => let(toMem(how(e))) be in)
  def store2(how: ToBeTyped[Expr]): ToBeTyped[Expr] =
    fun(e => let(toMem(how(e))) be (x => x))

  def toMemFun(f: ToBeTyped[Expr]): ToBeTyped[Expr] = fun(x => toMem(f(x)))

  case class `if`(b: ToBeTyped[Expr]) {
    def `then`(tE: ToBeTyped[Expr]): Object {
      def `else` (eE: ToBeTyped[Expr] ): ToBeTyped[Expr]
    } = {
      new {
        def `else`(eE: ToBeTyped[Expr]): ToBeTyped[Expr] = {
          select(b)(tE)(eE)
        }
      }
    }
  }

  implicit class Ops(lhs: ToBeTyped[Expr]) {

    // binary
    def +(rhs: ToBeTyped[Expr]): ToBeTyped[App] = add(lhs)(rhs)
    def -(rhs: ToBeTyped[Expr]): ToBeTyped[App] = sub(lhs)(rhs)
    def *(rhs: ToBeTyped[Expr]): ToBeTyped[App] = mul(lhs)(rhs)
    def /(rhs: ToBeTyped[Expr]): ToBeTyped[App] = div(lhs)(rhs)
    def %(rhs: ToBeTyped[Expr]): ToBeTyped[App] = mod(lhs)(rhs)
    def >(rhs: ToBeTyped[Expr]): ToBeTyped[App] = gt(lhs)(rhs)
    def <(rhs: ToBeTyped[Expr]): ToBeTyped[App] = lt(lhs)(rhs)
    def =:=(rhs: ToBeTyped[Expr]): ToBeTyped[App] = equal(lhs)(rhs)
    def >=(rhs: ToBeTyped[Expr]): ToBeTyped[App] = not(lhs < rhs)
    def <=(rhs: ToBeTyped[Expr]): ToBeTyped[App] = not(lhs > rhs)

    // scalastyle:off disallow.space.before.token
    // unary
    def unary_- : ToBeTyped[App] = neg(lhs)
    def unary_! : ToBeTyped[App] = not(lhs)
    // scalastyle:on disallow.space.before.token

    // pair accesses
    def _1: ToBeTyped[App] = fst(lhs)
    def _2: ToBeTyped[App] = snd(lhs)
  }

  implicit class Indexing(e: ToBeTyped[Expr]) {
    def `@`(i: ToBeTyped[Expr]): ToBeTyped[App] = idx(i)(e)
  }

  implicit class TypeAssertionHelper(t: Type) {
    def !:[T <: Expr](e: ToBeTyped[T]): ToBeTyped[Expr] =
      e >>= (e => toBeTyped(TypeAssertion(e, t)))
  }

  implicit class TypeAnnotationHelper(t: Type) {
    def ::[T <: Expr](e: ToBeTyped[T]): ToBeTyped[Expr] =
      e >>= (e => toBeTyped(TypeAnnotation(e, t)))
  }

  implicit class FunCall(f: ToBeTyped[Expr]) {
    def apply(e: ToBeTyped[Expr]): ToBeTyped[App] =
      app(f, e)
    def apply(e1: ToBeTyped[Expr], e2: ToBeTyped[Expr]): ToBeTyped[App] =
      f(e1)(e2)
    def apply(e1: ToBeTyped[Expr], e2: ToBeTyped[Expr],
              e3: ToBeTyped[Expr]): ToBeTyped[App] =
      f(e1)(e2)(e3)
    def apply(e1: ToBeTyped[Expr],
              e2: ToBeTyped[Expr],
              e3: ToBeTyped[Expr],
              e4: ToBeTyped[Expr]): ToBeTyped[App] =
      f(e1)(e2)(e3)(e4)
    def apply(e1: ToBeTyped[Expr],
              e2: ToBeTyped[Expr],
              e3: ToBeTyped[Expr],
              e4: ToBeTyped[Expr],
              e5: ToBeTyped[Expr]): ToBeTyped[App] =
      f(e1)(e2)(e3)(e4)(e5)

    def apply(n: Nat): ToBeTyped[DepApp[NatKind]] =
      depApp[NatKind](f, n)
    def apply(dt: DataType): ToBeTyped[DepApp[DataKind]] =
      depApp[DataKind](f, dt)
    def apply(a: AddressSpace): ToBeTyped[DepApp[AddressSpaceKind]] =
      depApp[AddressSpaceKind](f, a)
    def apply(n2n: NatToNat): ToBeTyped[DepApp[NatToNatKind]] =
      depApp[NatToNatKind](f, n2n)
    def apply(n2d: NatToData): ToBeTyped[DepApp[NatToDataKind]] =
      depApp[NatToDataKind](f, n2d)
  }

  implicit class FunPipe(e: ToBeTyped[Expr]) {
    def |>(f: ToBeTyped[Expr]): ToBeTyped[App] = f.apply(e)
  }

  implicit class FunPipeReverse(f: ToBeTyped[Expr]) {
    def $(e: ToBeTyped[Expr]): ToBeTyped[App] = f.apply(e)
  }

  implicit class FunPipeReversePrimitiveBuilde(f: Builder) {
    def $(e: ToBeTyped[Expr]): ToBeTyped[App] = f.apply(e)
  }

  implicit class FunComp(f: ToBeTyped[Expr]) {
    def >>(g: ToBeTyped[Expr]): ToBeTyped[Lambda] = fun(x => g(f(x)))
  }

  implicit class FunCompPrimitiveBuilder(f: Builder) {
    def >>(g: ToBeTyped[Expr]): ToBeTyped[Lambda] = fun(x => g(f.apply(x)))
  }

  implicit class FunCompReverse(f: ToBeTyped[Expr]) {
    def o(g: ToBeTyped[Expr]): ToBeTyped[Lambda] = fun(x => f(g(x)))
  }

  implicit class FunCompReversePrimitiveBuilder(f: Builder) {
    def o(g: ToBeTyped[Expr]): ToBeTyped[Lambda] = fun(x => f.apply(g(x)))
  }

  // function values
  object fun {
    def apply(t: Type)
             (f: ToBeTyped[Identifier] => ToBeTyped[Expr]
             ): ToBeTyped[Lambda] = {
      val x = identifier(freshName("e")) >>= (i => toBeTyped(i.setType(t)))
      lambda(x, f(x))
    }

    def apply(f: ToBeTyped[Identifier] => ToBeTyped[Expr]
             ): ToBeTyped[Lambda] = {
      val e = identifier(freshName("e"))
      lambda(e, f(e))
    }

    def apply(
               f: (ToBeTyped[Identifier], ToBeTyped[Identifier]) => ToBeTyped[Expr]
             ): ToBeTyped[Lambda] = {
      val e = identifier(freshName("e"))
      lambda(e, fun(e1 => f(e, e1)))
    }

    def apply(
               f: (ToBeTyped[Identifier], ToBeTyped[Identifier],
                 ToBeTyped[Identifier]) => ToBeTyped[Expr]
             ): ToBeTyped[Lambda] = {
      val e = identifier(freshName("e"))
      lambda(e, fun((e1, e2) => f(e, e1, e2)))
    }

    def apply(
               f: (
                 ToBeTyped[Identifier],
                   ToBeTyped[Identifier],
                   ToBeTyped[Identifier],
                   ToBeTyped[Identifier]
                 ) => ToBeTyped[Expr]
             ): ToBeTyped[Lambda] = {
      val e = identifier(freshName("e"))
      lambda(e, fun((e1, e2, e3) => f(e, e1, e2, e3)))
    }

    def apply(
               f: (
                 ToBeTyped[Identifier],
                   ToBeTyped[Identifier],
                   ToBeTyped[Identifier],
                   ToBeTyped[Identifier],
                   ToBeTyped[Identifier]
                 ) => ToBeTyped[Expr]
             ): ToBeTyped[Lambda] = {
      val e = identifier(freshName("e"))
      lambda(e, fun((e1, e2, e3, e4) => f(e, e1, e2, e3, e4)))
    }

    def apply(
               f: (
                 ToBeTyped[Identifier],
                   ToBeTyped[Identifier],
                   ToBeTyped[Identifier],
                   ToBeTyped[Identifier],
                   ToBeTyped[Identifier],
                   ToBeTyped[Identifier]
                 ) => ToBeTyped[Expr]
             ): ToBeTyped[Lambda] = {
      val e = identifier(freshName("e"))
      lambda(e, fun((e1, e2, e3, e4, e5) => f(e, e1, e2, e3, e4, e5)))
    }

    // noinspection TypeAnnotation
    // scalastyle:off structural.type
    def apply(ft: FunType[Type, Type]): Object {
      def apply(f: (ToBeTyped[Identifier], ToBeTyped[Identifier],
        ToBeTyped[Identifier], ToBeTyped[Identifier],
        ToBeTyped[Identifier], ToBeTyped[Identifier]
        ) => ToBeTyped[Expr]
               ): ToBeTyped[Expr]

      def apply(f: (ToBeTyped[Identifier], ToBeTyped[Identifier],
        ToBeTyped[Identifier], ToBeTyped[Identifier],
        ToBeTyped[Identifier]) => ToBeTyped[Expr]
               ): ToBeTyped[Expr]

      def apply(f: (ToBeTyped[Identifier], ToBeTyped[Identifier],
        ToBeTyped[Identifier], ToBeTyped[Identifier]
        ) => ToBeTyped[Expr]
               ): ToBeTyped[Expr]

      def apply(f: (ToBeTyped[Identifier], ToBeTyped[Identifier],
        ToBeTyped[Identifier]) => ToBeTyped[Expr]
               ): ToBeTyped[Expr]

      def apply(f: (ToBeTyped[Identifier], ToBeTyped[Identifier]
        ) => ToBeTyped[Expr]
               ): ToBeTyped[Expr]

      def apply(f: ToBeTyped[Identifier] => ToBeTyped[Expr]): ToBeTyped[Expr]
    } = new {
      def apply(f: ToBeTyped[Identifier] => ToBeTyped[Expr]): ToBeTyped[Expr] =
        fun(f) :: ft

      def apply(
                 f: (ToBeTyped[Identifier], ToBeTyped[Identifier]) => ToBeTyped[Expr]
               ): ToBeTyped[Expr] = fun(f) :: ft

      def apply(
                 f: (
                   ToBeTyped[Identifier],
                     ToBeTyped[Identifier],
                     ToBeTyped[Identifier]
                   ) => ToBeTyped[Expr]
               ): ToBeTyped[Expr] = fun(f) :: ft

      def apply(
                 f: (
                   ToBeTyped[Identifier],
                     ToBeTyped[Identifier],
                     ToBeTyped[Identifier],
                     ToBeTyped[Identifier]
                   ) => ToBeTyped[Expr]
               ): ToBeTyped[Expr] = fun(f) :: ft

      def apply(
                 f: (
                   ToBeTyped[Identifier],
                     ToBeTyped[Identifier],
                     ToBeTyped[Identifier],
                     ToBeTyped[Identifier],
                     ToBeTyped[Identifier]
                   ) => ToBeTyped[Expr]
               ): ToBeTyped[Expr] = fun(f) :: ft

      def apply(
                 f: (
                   ToBeTyped[Identifier],
                     ToBeTyped[Identifier],
                     ToBeTyped[Identifier],
                     ToBeTyped[Identifier],
                     ToBeTyped[Identifier],
                     ToBeTyped[Identifier]
                   ) => ToBeTyped[Expr]
               ): ToBeTyped[Expr] = fun(f) :: ft
    }
    // scalastyle:on structural.type
  }

  object depFun {
    def apply(r: arithexpr.arithmetic.Range,
              w: NatFunction1Wrapper[ToBeTyped[Expr]]
             ): ToBeTyped[DepLambda[NatKind]] = {
      val n = NatIdentifier(freshName("n"), r, isExplicit = true)
      depLambda[NatKind](n, w.f(n))
    }

    def apply(w: NatFunction1Wrapper[ToBeTyped[Expr]]
             ): ToBeTyped[DepLambda[NatKind]] = {
      val r = arithexpr.arithmetic.RangeAdd(0, arithexpr.arithmetic.PosInf, 1)
      val n = NatIdentifier(freshName("n"), r, isExplicit = true)
      depLambda[NatKind](n, w.f(n))
    }

    def apply(w: NatFunction2Wrapper[ToBeTyped[Expr]]
             ): ToBeTyped[DepLambda[NatKind]] = {
      val r = arithexpr.arithmetic.RangeAdd(0, arithexpr.arithmetic.PosInf, 1)
      val n1 = NatIdentifier(freshName("n"), r, isExplicit = true)
      depLambda[NatKind](n1, depFun((n2: Nat) => w.f(n1, n2)))
    }

    def apply(w: NatFunction3Wrapper[ToBeTyped[Expr]]
             ): ToBeTyped[DepLambda[NatKind]] = {
      val r = arithexpr.arithmetic.RangeAdd(0, arithexpr.arithmetic.PosInf, 1)
      val n1 = NatIdentifier(freshName("n"), r, isExplicit = true)
      depLambda[NatKind](n1, depFun((n2: Nat, n3: Nat) => w.f(n1, n2, n3)))
    }

    def apply(w: NatFunction4Wrapper[ToBeTyped[Expr]]
             ): ToBeTyped[DepLambda[NatKind]] = {
      val r = arithexpr.arithmetic.RangeAdd(0, arithexpr.arithmetic.PosInf, 1)
      val n1 = NatIdentifier(freshName("n"), r, isExplicit = true)
      depLambda[NatKind](n1, depFun((n2: Nat, n3: Nat, n4: Nat) =>
        w.f(n1, n2, n3, n4)))
    }

    def apply(w: NatFunction5Wrapper[ToBeTyped[Expr]]
             ): ToBeTyped[DepLambda[NatKind]] = {
      val r = arithexpr.arithmetic.RangeAdd(0, arithexpr.arithmetic.PosInf, 1)
      val n1 = NatIdentifier(freshName("n"), r, isExplicit = true)
      depLambda[NatKind](n1, depFun((n2: Nat, n3: Nat, n4: Nat, n5: Nat) =>
        w.f(n1, n2, n3, n4, n5)))
    }

    def apply(w: DataTypeFunctionWrapper[ToBeTyped[Expr]]
             ): ToBeTyped[DepLambda[DataKind]] = {
      val x = DataTypeIdentifier(freshName("dt"), isExplicit = true)
      depLambda[DataKind](x, w.f(x))
    }

    def apply(w: NatToDataFunctionWrapper[ToBeTyped[Expr]]
             ): ToBeTyped[DepLambda[NatToDataKind]] = {
      val x = NatToDataIdentifier(freshName("n2d"), isExplicit = true)
      depLambda[NatToDataKind](x, w.f(x))
    }

    def apply(w: NatToNatFunctionWrapper[ToBeTyped[Expr]]
             ): ToBeTyped[DepLambda[NatToNatKind]] = {
      val x = NatToNatIdentifier(freshName("n2n"), isExplicit = true)
      depLambda[NatToNatKind](x, w.f(x))
    }

    def apply(w: AddressSpaceFunctionWrapper[ToBeTyped[Expr]]
             ): ToBeTyped[DepLambda[AddressSpaceKind]] = {
      val x = AddressSpaceIdentifier(freshName("a"), isExplicit = true)
      depLambda[AddressSpaceKind](x, w.f(x))
    }
  }

  // noinspection ScalaUnusedSymbol
  // scalastyle:off structural.type
  object let {
    def apply(e: ToBeTyped[Expr]): Object {
      def be(in: ToBeTyped[Expr] => ToBeTyped[Expr]): ToBeTyped[Expr]
      def be(in: ToBeTyped[Expr]): ToBeTyped[Expr]
    } = new {
      def be(in: ToBeTyped[Expr] => ToBeTyped[Expr]): ToBeTyped[Expr] =
        primitives.let(e)(fun(in))
      def be(in: ToBeTyped[Expr]): ToBeTyped[Expr] =
        primitives.let(e)(in)
    }
  }
  // scalastyle:on structural.type

  object letf {
    def apply(in: ToBeTyped[Expr] => ToBeTyped[Expr]): ToBeTyped[Expr] = {
      fun(e => primitives.let(e)(fun(in)))
    }
    def apply(in: ToBeTyped[Expr]): ToBeTyped[Expr] = {
      fun(e => primitives.let(e)(in))
    }

    implicit def toLetf(l: letf.type): ToBeTyped[Expr] =
      fun(e => fun(in => primitives.let(e)(in)))
  }

  case class NatFunction1Wrapper[A](f: Nat => A)
  implicit def toNatFunction1Wrapper[A](f: Nat => A): NatFunction1Wrapper[A] =
    NatFunction1Wrapper(f)

  case class NatFunction2Wrapper[A](f: (Nat, Nat) => A)
  implicit def toNatFunction2Wrapper[A](f: (Nat, Nat) => A
                                       ): NatFunction2Wrapper[A] =
    NatFunction2Wrapper(f)

  case class NatFunction3Wrapper[A](f: (Nat, Nat, Nat) => A)
  implicit def toNatFunction3Wrapper[A](f: (Nat, Nat, Nat) => A
                                       ): NatFunction3Wrapper[A] =
    NatFunction3Wrapper(f)

  case class NatFunction4Wrapper[A](f: (Nat, Nat, Nat, Nat) => A)
  implicit def toNatFunction4Wrapper[A](f: (Nat, Nat, Nat, Nat) => A
                                       ): NatFunction4Wrapper[A] =
    NatFunction4Wrapper(f)

  case class NatFunction5Wrapper[A](f: (Nat, Nat, Nat, Nat, Nat) => A)
  implicit def toNatFunction5Wrapper[A](f: (Nat, Nat, Nat, Nat, Nat) => A
                                       ): NatFunction5Wrapper[A] =
    NatFunction5Wrapper(f)

  case class DataTypeFunctionWrapper[A](f: DataType => A)
  implicit def toDataTypeFunctionWrapper[A](f: DataType => A
                                           ): DataTypeFunctionWrapper[A] =
    DataTypeFunctionWrapper(f)

  case class NatToDataFunctionWrapper[A](f: NatToData => A)
  implicit def toNatToDataFunctionWrapper[A](f: NatToData => A
                                            ): NatToDataFunctionWrapper[A] =
    NatToDataFunctionWrapper(f)

  case class NatToNatFunctionWrapper[A](f: NatToNat => A)
  implicit def toNatToNatFunctionWrapper[A](f: NatToNat => A
                                           ): NatToNatFunctionWrapper[A] =
    NatToNatFunctionWrapper(f)

  case class AddressSpaceFunctionWrapper[A](f: AddressSpace => A)
  implicit def toAddressSpaceFunctionWrapper[A](f: AddressSpace => A
                                               ): AddressSpaceFunctionWrapper[A] =
    AddressSpaceFunctionWrapper(f)

  case class NatCollectionFunctionWrapper[A](f: NatCollectionIdentifier => A)
  implicit def toNatCollectionFunctionWrapper[A](f: NatCollectionIdentifier => A
                                                ): NatCollectionFunctionWrapper[A] =
    NatCollectionFunctionWrapper(f)

  implicit def wrapInNatExpr(n: Nat): ToBeTyped[Literal] = literal(NatData(n))

  def l(i: Int): ToBeTyped[Literal] = literal(IntData(i))
  def l(f: Float): ToBeTyped[Literal] = literal(FloatData(f))
  def l(d: Double): ToBeTyped[Literal] = literal(DoubleData(d))
  def l(n: Nat): ToBeTyped[Literal] = literal(NatData(n))
  def lidx(i: Nat, n: Nat): ToBeTyped[Literal] = literal(IndexData(i, n))
  def lvec(v: Seq[ScalarData]): ToBeTyped[Literal] = literal(VectorData(v))
  def larr(a: Seq[Data]): ToBeTyped[Literal] = literal(ArrayData(a))

  object foreignFun {
    def apply(name: String, t: Type): ToBeTyped[ForeignFunction] = {
      toBeTyped(ForeignFunction(ForeignFunction.Decl(name, None))(t))
    }

    def apply(
               name: String,
               params: Seq[String],
               body: String,
               t: Type
             ): ToBeTyped[ForeignFunction] = {
      toBeTyped(
        ForeignFunction(
          ForeignFunction.Decl(name, Some(ForeignFunction.Def(params, body)))
        )(t)
      )
    }
  }

  // interface between ToBeTyped and Expr

  implicit def toExpr[T <: Expr](d: ToBeTyped[T]): Expr = d.toExpr

  implicit def preserveType[T <: Expr](e: T): ToBeTyped[Opaque] =
    ToBeTyped(Opaque(e, e.t))

  def toBeTyped[T <: Expr](e: T): ToBeTyped[T] = ToBeTyped(e)

  def topLevel(e: Expr): TopLevel = TopLevel(e)()

  def untypedTopLevel[T <: Expr](d: ToBeTyped[T]): ToBeTyped[TopLevel] =
    toBeTyped(topLevel(toExpr(d)))

  def eraseTypeFromExpr[T <: Expr](e: T): T =
    traversal
      .DepthFirstLocalResult(
        e,
        new traversal.Visitor {
          override def visitExpr(e: Expr): Result[Expr] = e match {
            case l: Literal => Continue(l, this)
            case _          => Continue(e.setType(TypePlaceholder), this)
          }
        }
      )
      .asInstanceOf[T]

  def eraseType[T <: Expr](e: T): ToBeTyped[T] = toBeTyped(eraseTypeFromExpr(e))
}
// scalastyle:on number.of.methods
