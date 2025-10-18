package shine.GAP8

import rise.core.types.DataType
import shine.DPIA.DSL.λ
import shine.DPIA.Phrases.Phrase
import shine.DPIA.Types.CommType
import shine.DPIA.{VarType, varT}
import shine.GAP8.primitives.imperative.MemoryAlloc

package object DSL {
  object GAP8MemoryAlloc {
    def apply(memoryType: shine.GAP8.MemoryType)
             (dt: DataType, f: Phrase[VarType] => Phrase[CommType]): MemoryAlloc =
      MemoryAlloc(memoryType)(dt, λ(varT(dt))(v => f(v)))
  }
}
