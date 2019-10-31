// for small and quick tests
import lift.core.DSL._
import lift.core.primitives._
import lift.core.types._

let == let
map == map
mapSeq == mapSeq
val ida = identifier("a")
val idb = identifier("a")
ida == idb
ida.t == idb.t