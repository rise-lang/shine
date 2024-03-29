package apps.stencil

import rise.core._
import rise.core.DSL._
import rise.core.DSL.Type._
import rise.core.primitives._
import rise.core.types._
import rise.core.types.DataType._
import rise.core.DSL.HighLevelConstructs.{slide2D, slide3D, zipND}
import rise.openCL.DSL._
import rise.openCL.primitives.oclRotateValues

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

  private val generateNumNeighbours = depFun((o: Nat, n: Nat, m: Nat) =>
    generate(fun(k =>
      generate(fun(j =>
        generate(fun(i =>
          getNumNeighbours(cast(i))(cast(j))(cast(k))
          (cast(l(m)))(cast(l(n)))(cast(l(o)))
        ))
      ))
    ))
  )

  private val getCF = foreignFun("getCF", Seq("neigh", "cfB", "cfI"),
    "{ if (neigh < 6) { return cfB; } else { return cfI; } }",
    int ->: f32 ->: f32 ->: f32)

  private val id = fun(x => x)
  private val zip3D: ToBeTyped[Expr] = zipND(3)

  private val SR = 441.0f
  private val alpha = 0.005f
  private val c = 344.0f
  // private val NF = 4410
  private val k = 1.0f / SR
  private val h = Math.sqrt(3.0f) * c * k
  private val lambda = c * k / h

  private val loss1 = 1.0f / (1.0f + lambda * alpha)
  private val loss2 = 1.0f - lambda * alpha

  private val l2 = lf32(((c * c * k * k) / (h * h)).toFloat)
  private val cf1 = Array(loss1.toFloat, 1.0f).map(lf32)
  private val cf21 = Array(loss2.toFloat, 1.0f).map(lf32)

  private val sz: Nat = 3

  val acoustic: ToBeTyped[Expr] = fun(
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

    ((t(1, 1, 1) * (lf32(2.0f) - (valueMask * l2))) +
      ((stencil * maskedValStencil) - (valueMat1 * cf2))) * cf
  })

  val stencil: ToBeTyped[Expr] = depFun((o: Nat, n: Nat, m: Nat) => fun(
    ((o + 2) `.` (n + 2) `.` (m + 2) `.` f32) ->:
      ((o + 2) `.` (n + 2) `.` (m + 2) `.` f32) ->:
      (o `.` n `.` m `.` f32)
  )((mat1, mat2) =>
    mapGlobal(2)(mapGlobal(1)(mapGlobal(0)(acoustic)))
      o slide3D(sz, 1)
      $ zip3D(mat1)(zip3D(mat2)(generateNumNeighbours(o + 2)(n + 2)(m + 2)))
  ))

  val stencilMSS: ToBeTyped[Expr] = depFun((o: Nat, n: Nat, m: Nat) => fun(
    ((o + 2) `.` (n + 2) `.` (m + 2) `.` f32) ->:
      ((o + 2) `.` (n + 2) `.` (m + 2) `.` f32) ->:
      (o `.` n `.` m `.` f32)
  )((mat1, mat2) =>
    transpose o map(transpose) o transpose o
      mapGlobal(0)(
        mapGlobal(1)(
          iterateStream(acoustic) o
          oclRotateValues(AddressSpace.Private)(sz)(
            mapSeqUnroll(mapSeqUnroll(id))
          ) o transpose o map(transpose)
        )
      ) o transpose o slide2D(sz, 1) o map(transpose) o transpose
      $ zip3D(mat1)(zip3D(mat2)(generateNumNeighbours(o + 2)(n + 2)(m + 2)))
  ))
}
