package apps

import rise.core._
import rise.core.DSL._
import rise.core.TypeLevelDSL._
import rise.core.types._
import rise.core.primitives._
import rise.core.HighLevelConstructs._
import rise.OpenCL.DSL._

object acoustic3D {
  private val getNumNeighbours = foreignFun("idxF",
    Seq("i", "j", "k", "m", "n", "o"),
    """{
      |  int count = 6;
      |  if (i == (m - 1) || i == 0){ count--; }
      |  if (j == (n - 1) || j == 0){ count--; }
      |  if (k == (o - 1) || k == 0){ count--; }
      |  return count;
      |}""".stripMargin,
    int ->: int ->: int ->: int ->: int ->: int ->: int
  )

  private val generateNumNeighbours = nFun(o => nFun(n => nFun(m =>
    generate(fun(k =>
      generate(fun(j =>
        generate(fun(i =>
          getNumNeighbours(cast(i))(cast(j))(cast(k))
          (cast(m: Expr))(cast(n: Expr))(cast(o: Expr))
        ))
      ))
    ))
  )))

  private val getCF = foreignFun("getCF", Seq("neigh", "cfB", "cfI"),
    "{ if (neigh < 6) { return cfB; } else { return cfI; } }",
    int ->: f32 ->: f32 ->: f32)

  private val id = fun(x => x)
  private val zip3D: Expr = zipND(3)

  private val SR = 441.0f
  private val alpha = 0.005f
  private val c = 344.0f
  // private val NF = 4410
  private val k = 1.0f / SR
  private val h = Math.sqrt(3.0f) * c * k
  private val lambda = c * k / h

  private val loss1 = 1.0f / (1.0f + lambda * alpha)
  private val loss2 = 1.0f - lambda * alpha

  private val l2 = l(((c * c * k * k) / (h * h)).toFloat)
  private val cf1 = Array(loss1.toFloat, 1.0f).map(l)
  private val cf21 = Array(loss2.toFloat, 1.0f).map(l)

  private val sz: Nat = 3
  private val st: Nat = 1

  val acoustic: Expr = fun(
    (3 `.` 3 `.` 3 `.` PairType(f32, PairType(f32, int))) ->: f32
  )(tile => {
    val x = tile `@` lidx(1, 3) `@` lidx(1, 3) `@` lidx(1, 3) |> snd |> snd
    val cf = toPrivate(getCF(x)(cf1(0))(cf1(1)))
    val cf2 = toPrivate(getCF(x)(cf21(0))(cf21(1)))
    val maskedValStencil = l2

    def t(i: Int, j: Int, k: Int) =
      tile `@` lidx(i, 3) `@` lidx(j, 3) `@` lidx(k, 3) |> snd |> fst

    val stencil = toPrivate(
      t(0, 1, 1) + t(1, 0, 1) + t(1, 1, 0) +
        t(1, 1, 2) + t(1, 2, 1) + t(2, 1, 1))

    val valueMat1 = tile `@` lidx(1, 3) `@` lidx(1, 3) `@` lidx(1, 3) |> fst
    val valueMask = cast(x) :: f32

    ((t(1, 1, 1) * (l(2.0f) - (valueMask * l2))) +
      ((stencil * maskedValStencil) - (valueMat1 * cf2))) * cf
  })

  val stencil: Expr = nFun(o => nFun(n => nFun(m => fun(
    ((o + 2) `.` (n + 2) `.` (m + 2) `.` f32) ->:
      ((o + 2) `.` (n + 2) `.` (m + 2) `.` f32) ->:
      (o `.` n `.` m `.` f32)
  )((mat1, mat2) =>
    mapGlobal(2)(mapGlobal(1)(mapGlobal(0)(acoustic)))
      o slide3D(sz, st)
      $ zip3D(mat1)(zip3D(mat2)(generateNumNeighbours(o + 2)(n + 2)(m + 2)))
  ))))

  val stencilMSS: Expr = nFun(o => nFun(n => nFun(m => fun(
    ((o + 2) `.` (n + 2) `.` (m + 2) `.` f32) ->:
      ((o + 2) `.` (n + 2) `.` (m + 2) `.` f32) ->:
      (o `.` n `.` m `.` f32)
  )((mat1, mat2) =>
    transpose o map(transpose) o transpose o
      mapGlobal(0)(
        mapGlobal(1)(
          oclSlideSeq(SlideSeq.Values)(AddressSpace.Private)(sz)(st)
          (mapSeqUnroll(mapSeqUnroll(id)))(acoustic)
            o transpose o map(transpose)
        )
      ) o transpose o slide2D(sz, st) o map(transpose) o transpose
      $ zip3D(mat1)(zip3D(mat2)(generateNumNeighbours(o + 2)(n + 2)(m + 2)))
  ))))
}
