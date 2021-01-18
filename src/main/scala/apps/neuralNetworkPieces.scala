package apps

import rise.core._
import rise.core.DSL._
import rise.core.DSL.Type._
import rise.core.DSL.HighLevelConstructs._
import rise.core.types._
import rise.core.primitives._

object neuralNetworkPieces {
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

  // transposed matrix multiplication
  val tmm: ToBeTyped[Expr] = depFun((n: Nat, m: Nat, k: Nat) => fun(
    (m`.`k`.`f32) ->: (n`.`k`.`f32) ->: (m`.`n`.`f32)
  )((a, b) =>
    a |> map(fun(aRow =>
      b |> map(fun(bCol =>
        dot(aRow)(bCol)
      ))
    ))
  ))

  // TODO? transposed batched matrix multiplication
  // TODO? grouped convolutions
  // TODO? Multi-Layer Perception
  // see tensor comprehensions paper

  // ReLu(tmm(tanh(A), B))
  val RLMMTH: ToBeTyped[Expr] = depFun((n: Nat, m: Nat, k: Nat) => fun(
    (m`.`k`.`f32) ->: (n`.`k`.`f32) ->: (m`.`n`.`f32)
  )((a, b) =>
    reLuND(2)(tmm(tanhND(2)(a), b))
  ))
}
