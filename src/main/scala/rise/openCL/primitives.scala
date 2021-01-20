package rise.openCL

import rise.core.DSL.Type._
import rise.core.types.{AddressSpace, DataType, Nat}
import rise.core.{Builder, Primitive}
import rise.macros.Primitive.primitive

// noinspection DuplicatedCode
object primitives {
  // TODO? depMapGlobal, depMapLocal, depMapWorkGroup

  @primitive case class mapGlobal(dim: Int) extends Primitive with Builder {
    impl{ n: Nat => impl{ s: DataType => impl{ t: DataType =>
      (s ->: t) ->: (n`.`s) ->: (n`.`t) }}}
  }

  @primitive case class mapLocal(dim: Int) extends Primitive with Builder {
    impl{ n: Nat => impl{ s: DataType => impl{ t: DataType =>
      (s ->: t) ->: (n`.`s) ->: (n`.`t) }}}
  }

  @primitive case class mapWorkGroup(dim: Int) extends Primitive with Builder {
    impl{ n: Nat => impl{ s: DataType => impl{ t: DataType =>
      (s ->: t) ->: (n`.`s) ->: (n`.`t) }}}
  }

  @primitive object oclToMem extends Primitive with Builder {
    impl{ t: DataType => expl((_: AddressSpace) => t ->: t) }
  }

  @primitive object oclReduceSeq extends Primitive with Builder {
    expl((_: AddressSpace) =>
      impl{ n: Nat => impl{ s: DataType => impl{ t: DataType =>
        (t ->: s ->: t) ->: t ->: (n`.`s) ->: t }}})
  }

  @primitive object oclReduceSeqUnroll extends Primitive with Builder {
    expl((_: AddressSpace) =>
      impl{ n: Nat => impl{ s: DataType => impl{ t: DataType =>
        (t ->: s ->: t) ->: t ->: (n`.`s) ->: t }}})
  }

  @primitive object oclIterate extends Primitive with Builder {
    expl((_: AddressSpace) =>
      impl{ n: Nat => impl{ m: Nat =>
        expl((k: Nat) => impl{ t: DataType =>
          expl((l: Nat) => ((l * n)`.`t) ->: (l`.`t)) ->:
            ((m * n.pow(k))`.`t) ->: (m`.`t) })}})
  }

  @primitive object oclCircularBuffer extends Primitive with Builder {
    // TODO: should return a stream / sequential array, not an array
    expl((_: AddressSpace) => impl{ n: Nat =>
      expl((alloc: Nat) => expl((sz: Nat) =>
        impl{ s: DataType => impl{ t: DataType =>
          (s ->: t) ->: // function to load an input
            ((n + sz)`.`s) ->: ((1 + n)`.`sz`.`t) }}))})
  }

  @primitive object oclRotateValues extends Primitive with Builder {
    // TODO: should return a stream / sequential array, not an array
    expl((_: AddressSpace) => impl{ n: Nat =>
      expl((sz: Nat) => impl{ s: DataType =>
        (s ->: s) ->: // function to write a value
          ((n + sz)`.`s) ->: ((1 + n)`.`sz`.`s) })})
  }

  @primitive object oclRun extends Primitive with Builder {
    impl{ t: DataType => t ->: t }
  }
}
