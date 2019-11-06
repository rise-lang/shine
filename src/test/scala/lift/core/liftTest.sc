// for small and quick tests
import lift.core.DSL._
import lift.OpenCL.DSL._
import lift.core._
import lift.OpenCL.primitives._
import lift.core.primitives._
import lift.core.types._

let == let
map == map
mapSeq == mapSeq
val ida = identifier("a")
val idb = identifier("a")
ida == idb
ida.t == idb.t
val m = toPrivate
val x = m match {
  case DepApp(f @ ToMem(), _) => f.typeScheme
  case _ => ???
}

val y = m match {
  case DepApp(f @ ToMem(), _) => f.typeScheme
  case _ => ???
}