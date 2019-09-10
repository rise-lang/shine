package apps

import lift.core._
import lift.core.DSL._
import lift.core.types._
import lift.core.primitives._
import lift.OpenCL.primitives._
import lift.core.HighLevelConstructs._
import idealised.util.gen

class acoustic extends idealised.util.Tests {
  val getNumNeighbours = foreignFun("idxF", Seq("i", "j", "k", "m", "n", "o"),
    """{ int count = 6;
      |  if (i == (m - 1) || i == 0){ count--; }
      |  if (j == (n - 1) || j == 0){ count--; }
      |  if (k == (o - 1) || k == 0){ count--; }
      |  return count;
      |}""".stripMargin,
    int ->: int ->: int ->: int ->: int ->: int ->: int)

  val generateNumNeighbours = nFun(o => nFun(n => nFun(m =>
    generate(fun(k =>
      generate(fun(j =>
        generate(fun(i =>
          getNumNeighbours(cast(i))(cast(j))(cast(k))
            (cast(m: Expr))(cast(n: Expr))(cast(o: Expr))))))))
  )))

  val getCF = foreignFun("getCF", Seq("neigh", "cfB", "cfI"),
    "{ if (neigh < 6) { return cfB; } else { return cfI; } }",
    int ->: float ->: float ->: float)

  val SR = 441.0f
  val alpha = 0.005f
  val c = 344.0f
  val NF = 4410
  val k = 1.0f / SR
  val h = Math.sqrt(3.0f) * c * k
  val lambda = c * k / h

  val loss1 = 1.0f / (1.0f + lambda * alpha)
  val loss2 = 1.0f - lambda * alpha

  val l2 = l(((c * c * k * k) / (h * h)).toFloat)
  val cf1 = Array(loss1.toFloat, 1.0f).map(l)
  val cf21 = Array(loss2.toFloat, 1.0f).map(l)

  val sz: Nat = 3
  val st: Nat = 1

  val acoustic: Expr = fun(
    (3`.`3`.`3`.`TupleType(float, TupleType(float, int))) ->: float
  )(tile => {
    val x = tile `@` lidx(1, 3) `@` lidx(1, 3) `@` lidx(1, 3) |> snd |> snd
    val cf = toPrivate(getCF(x)(cf1(0))(cf1(1)))
    val cf2 = toPrivate(getCF(x)(cf21(0))(cf21(1)))
    val maskedValStencil = l2

    def t(i: Int, j: Int, k: Int) =
      tile `@` lidx(i, 3) `@` lidx(j, 3) `@` lidx(k, 3) |> snd |> fst

    val stencil = toPrivate(
      t(0, 1, 1) + t(1, 0, 1) + t(1, 1, 0) + t(1, 1, 2) + t(1, 2, 1) + t(2, 1, 1))

    val valueMat1 = tile `@` lidx(1, 3) `@` lidx(1, 3) `@` lidx(1, 3) |> fst
    val valueMask = cast(x) :: float

    ((t(1, 1, 1) * (l(2.0f) - (valueMask * l2))) +
      ((stencil * maskedValStencil) - (valueMat1 * cf2))
    ) * cf
  })

  val localDimX = 20
  val localDimY = 15
  val localDimZ = 12

  val zip3D: Expr = zipND(3)

  val stencil: Expr = nFun(o => nFun(n => nFun(m => fun(
    ((o + 2)`.`(n + 2)`.`(m + 2)`.`float) ->:
    ((o + 2)`.`(n + 2)`.`(m + 2)`.`float) ->:
    (o`.`n`.`m`.`float)
  )((mat1, mat2) =>
    transpose o map(transpose) o transpose o
    mapGlobal(0)(mapGlobal(1)(
      // TODO: mapSeqSlide
      mapSeq(acoustic) o slide(sz)(st)
        o transpose o map(transpose)
    )) o transpose o slide2D(sz)(st) o map(transpose) o transpose
      $ zip3D(mat1)(zip3D(mat2)(generateNumNeighbours(o)(n)(m)))
  ))))

  test("acoustic stencil core compiles to syntactically correct OpenCL") {
    gen.OpenCLKernel(acoustic)
  }

  test("acoustic stencil compiles to syntactically correct OpenCL") {
    gen.OpenCLKernel(stencil)
  }
}
