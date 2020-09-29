package rise.openCL

import rise.core.TypeLevelDSL._
import rise.core.{Builder, Primitive}
import rise.macros.Primitive.primitive

// noinspection DuplicatedCode
object primitives {
  // TODO? depMapGlobal, depMapLocal, depMapWorkGroup

  @primitive case class mapGlobal(dim: Int) extends Primitive with Builder {
    implNat(n => implDT(s => implDT(t =>
      (s ->: t) ->: (n`.`s) ->: (n`.`t))))
  }

  @primitive case class mapLocal(dim: Int) extends Primitive with Builder {
    implNat(n => implDT(s => implDT(t =>
      (s ->: t) ->: (n`.`s) ->: (n`.`t))))
  }

  @primitive case class mapWorkGroup(dim: Int) extends Primitive with Builder {
    implNat(n => implDT(s => implDT(t =>
      (s ->: t) ->: (n`.`s) ->: (n`.`t))))
  }

  @primitive object oclToMem extends Primitive with Builder {
    implDT(t => forallAddr(_ => t ->: t))
  }

  @primitive object oclReduceSeq extends Primitive with Builder {
    forallAddr(_ => implNat(n => implDT(s => implDT(t =>
      (t ->: s ->: t) ->: t ->: (n`.`s) ->: t))))
  }

  @primitive object oclReduceSeqUnroll extends Primitive with Builder {
    forallAddr(_ => implNat(n => implDT(s => implDT(t =>
      (t ->: s ->: t) ->: t ->: (n`.`s) ->: t))))
  }

  @primitive object oclIterate extends Primitive with Builder {
    forallAddr(_ => implNat(n => implNat(m =>
      forallNat(k => implDT(t =>
        forallNat(l => ((l * n)`.`t) ->: (l`.`t)) ->:
          ((m * n.pow(k))`.`t) ->: (m`.`t))))))
  }

  @primitive object oclCircularBuffer extends Primitive with Builder {
    // TODO: should return a stream / sequential array, not an array
    forallAddr(_ => implNat(n =>
      forallNat(alloc => forallNat(sz => implDT(s => implDT(t =>
        (s ->: t) ->: // function to load an input
          ((n + sz)`.`s) ->: ((1 + n)`.`sz`.`t)))))))
  }

  @primitive object oclRotateValues extends Primitive with Builder {
    // TODO: should return a stream / sequential array, not an array
    forallAddr(_ => implNat(n => forallNat(sz => implDT(s =>
      (s ->: s) ->: // function to write a value
        ((n + sz)`.`s) ->: ((1 + n)`.`sz`.`s)))))
  }
}
