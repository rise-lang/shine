package apps

import rise.core.DSL.HighLevelConstructs._
import rise.core.DSL.Type._
import rise.core.DSL._
import rise.core._
import rise.core.primitives._
import rise.core.types._

object resNet50 {
  val highLevel: ToBeTyped[Expr] =
    fun(3`.`224`.`224`.`f32)(input =>
    // parameter values for scaling layers
    fun()
    )

  private val dot = separableConvolution2D.dot

  private val max = depFun((dt: DataType) =>
    foreignFun("max", dt ->: dt ->: dt)
  )

  private val tanh = depFun((dt: DataType) =>
    foreignFun("tanh", dt ->: dt)
  )

  // point-wise max(p, 0)
  def reLuND(dims: Int): ToBeTyped[Expr] = impl { dt: DataType =>
    mapND(dims)(max(dt)(cast(l(0))))
  }

  // point-wise tanh
  def tanhND(dims: Int): ToBeTyped[Expr] = impl { dt: DataType =>
    mapND(dims)(tanh(dt))
  }

  // matrix multiplication, the first matrix is transposed
  val matmul: ToBeTyped[Expr] = depFun((n: Nat, m: Nat, o: Nat) => fun(
    (o`.`n`.`f32) ->: (o`.`m`.`f32) ->: (n`.`m`.`f32)
  )((at, b) =>
    transpose(at) |> map(fun(aRow =>
      transpose(b) |> map(fun(bCol =>
        dot(aRow)(bCol)
      ))
    ))
  ))

  // ReLu(matmul(tanh(A), B))
  val RLMMTH: ToBeTyped[Expr] = depFun((n: Nat, m: Nat, o: Nat) => fun(
    (o`.`n`.`f32) ->: (o`.`m`.`f32) ->: (n`.`m`.`f32)
  )((at, b) =>
    reLuND(2)(matmul(tanhND(2)(at), b))
  ))
}
