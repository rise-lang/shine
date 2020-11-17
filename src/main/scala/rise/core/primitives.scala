package rise.core

import arithexpr.arithmetic.BigSum
import rise.core.TypeLevelDSL._
import rise.core.types._
import rise.macros.Primitive.primitive

// noinspection DuplicatedCode
// scalastyle:off number.of.methods
object primitives {

  @primitive case class makeArray(n: Int) extends Primitive with Builder {
    impl{ t: DataType => {
      def tRec(m: Int, dt: DataType): Type =
        if (m <= 0) {
          ArrayType(n, dt)
        } else {
          dt ->: tRec(m - 1, dt)
        }
      tRec(n, t)
    }}
  }

  @primitive object cast extends Primitive with Builder {
    impl{ s: DataType => impl{ t: DataType => s ->: t }}
  }

  @primitive object depJoin extends Primitive with Builder {
    impl{ n: Nat => impl{ lenF: NatToNat => impl{ dt: DataType =>
      (n `*.` (i => lenF(i) `.` dt)) ->:
        (BigSum(from = 0, upTo = n - 1, n => lenF(n)) `.` dt) }}}
  }

  @primitive object depMapSeq extends Primitive with Builder {
    impl{ n: Nat =>
      impl{ ft1: NatToData => impl{ ft2: NatToData =>
        expl((k: Nat) => ft1(k) ->: ft2(k)) ->: (n`*.`ft1) ->: (n`*.`ft2)}}}
  }

  @primitive object depZip extends Primitive with Builder {
    impl{ n: Nat => impl{ ft1: NatToData => impl{ ft2: NatToData =>
      (n`*.`ft1) ->: (n`*.`ft2) ->: (n`*.`(i => ft1(i) x ft2(i))) }}}
  }

  @primitive object drop extends Primitive with Builder {
    expl((n: Nat) => impl{ m: Nat => impl{ t: DataType =>
      ((n + m)`.`t) ->: (m`.`t) }})
  }

  @primitive object fst extends Primitive with Builder {
    impl{ s: DataType => impl{ t: DataType =>
      (s x t) ->: s }}
  }

  @primitive object gather extends Primitive with Builder {
    impl{ n: Nat => impl{ m: Nat => impl{ t: DataType =>
      (m`.`IndexType(n)) ->: (n`.`t) ->: (m`.`t)  }}}
  }

  @primitive object generate extends Primitive with Builder {
    impl{ n: Nat => impl{ t: DataType =>
      (IndexType(n) ->: t) ->: ArrayType(n, t) }}
  }

  @primitive object idx extends Primitive with Builder {
    impl{ n: Nat => impl{ t: DataType =>
      IndexType(n) ->: (n`.`t) ->: t }}
  }

  @primitive object id extends Primitive with Builder {
    impl{ t: DataType => t ->: t }
  }

  @primitive object indexAsNat extends Primitive with Builder {
    impl{ n: Nat => IndexType(n) ->: NatType }
  }

  @primitive object iterate extends Primitive with Builder {
    impl{ n: Nat => impl{ m: Nat =>
      expl((k: Nat) => impl{ t: DataType => expl((l: Nat) =>
        ((l * n)`.`t) ->: (l`.`t)) ->: ((m * n.pow(k))`.`t) ->: (m`.`t) })}}
  }

  @primitive object join extends Primitive with Builder {
    impl{ n: Nat => impl{ m: Nat => impl{ t: DataType =>
      (n`.`m`.`t) ->: ((n * m)`.`t) }}}
  }

  @primitive object let extends Primitive with Builder {
    impl{ s: DataType => impl{ t: DataType =>
      s ->: (s ->: t) ->: t }}
  }

  @primitive object map extends Primitive with Builder {
    impl{ n: Nat => impl{ s: DataType => impl{ t: DataType =>
      (s ->: t) ->: (n `.` s) ->: (n `.` t) }}}
  }

  @primitive object mapFst extends Primitive with Builder {
    impl{ s: DataType => impl{ t: DataType => impl{ s2: DataType =>
      (s ->: s2) ->: (s x t) ->: (s2 x t) }}}
  }

  @primitive object mapSnd extends Primitive with Builder {
    impl{ s: DataType => impl{ t: DataType => impl{ t2: DataType =>
      (t ->: t2) ->: (s x t) ->: (s x t2) }}}
  }

  @primitive object mapSeq extends Primitive with Builder {
    impl{ n: Nat => impl{ s: DataType => impl{ t: DataType =>
      (s ->: t) ->: (n`.`s) ->: (n`.`t) }}}
  }

  @primitive object mapStream extends Primitive with Builder {
    impl{ n: Nat => impl{ s: DataType => impl{ t: DataType =>
      // stream to stream
      (s ->: t) ->: (n`.`s) ->: (n`.`t) }}}
  }

  @primitive object iterateStream extends Primitive with Builder {
    impl{ n: Nat => impl{ s: DataType => impl{ t: DataType =>
      // stream to array (or should it be stream to value?)
      (s ->: t) ->: (n`.`s) ->: (n`.`t) }}}
  }

  @primitive object mapSeqUnroll extends Primitive with Builder {
    impl{ n: Nat => impl{ s: DataType => impl{ t: DataType =>
      (s ->: t) ->: (n `.` s) ->: (n `.` t) }}}
  }

  @primitive object toMem extends Primitive with Builder {
    impl{ t: DataType => t ->: t }
  }

  @primitive object natAsIndex extends Primitive with Builder {
    expl((n: Nat) => NatType ->: IndexType(n))
  }

  // TODO? could be expressed in terms of a pad idx -> val
  @primitive object padCst extends Primitive with Builder {
    impl{ n: Nat =>
      expl((l: Nat) =>
        expl((q: Nat) => impl{ t: DataType =>
          t ->: (n`.`t) ->: ((l + n + q)`.`t) }))}
  }

  @primitive object padEmpty extends Primitive with Builder {
    impl{ n: Nat =>
      expl((r: Nat) => impl{ t: DataType => (n`.`t) ->: ((n + r)`.`t) })}
  }

  // TODO? could be expressed in terms of a pad idx -> idx or idx -> val
  @primitive object padClamp extends Primitive with Builder {
    impl{ n: Nat =>
      expl((l: Nat) => expl((q: Nat) => impl{ t: DataType =>
        (n`.`t) ->: ((l + n + q)`.`t) }))}
  }

  @primitive object partition extends Primitive with Builder {
    impl{ n: Nat => impl{ dt: DataType =>
      expl((m: Nat) =>
        expl((lenF: NatToNat) =>
          (n`.`dt) ->: (m`*.`(i => lenF(i)`.`dt))))}}
  }

  @primitive object pair extends Primitive with Builder {
    impl{ s: DataType => impl{ t: DataType =>
      s ->: t ->: (s x t) }}
  }

  @primitive object dpair extends Primitive with Builder {
    impl{ fdt: NatToData => expl((n: Nat) =>
      fdt(n) ->: (Nat `**` (fdt(_))))}
  }

  @primitive object dpairNats extends Primitive with Builder {
    impl{ fdt: NatCollectionToData => expl((ns: NatCollection) =>
      fdt(ns) ->: (NatCollection `**` (fdt(_))))}
  }

  @primitive object dmatch extends Primitive with Builder {
    impl{ ft: NatToData => impl{ tOut: DataType =>
      (Nat `**` (ft(_))) ->: expl((m: Nat) => ft(m) ->: tOut) ->: tOut }}
  }

  @primitive object filter extends Primitive with Builder {
    impl{ n: Nat => impl { dt: DataType => (n `.` dt) ->: (dt ->: bool) ->: (Nat `**` (m => m `.` IndexType(n))) } }
  }

  @primitive object filterW extends Primitive with Builder {
    impl{ n: Nat => impl { dt: DataType => (n `.` dt) ->: (dt ->: bool) ->: (Nat `**` (m => m `.` IndexType(n))) } }
  }

  @primitive object count extends Primitive with Builder {
    impl { n: Nat => impl { dt : DataType => (n `.` dt) ->: (dt ->: bool) ->: IndexType(n) }}
  }

  @primitive object which extends Primitive with Builder {
    impl { n: Nat => impl { dt : DataType => (n `.` dt) ->: expl((count: Nat) => (dt ->: bool) ->: (count `.` IndexType(n)))}}
  }

  @primitive object liftN extends Primitive with Builder {
    impl { dt: DataType => NatType ->: expl {(_: Nat) => dt } ->: dt }
  }

  @primitive object liftNats extends Primitive with Builder {
    impl { n: Nat => impl { dt: DataType => (n `.` NatType) ->: expl {(_: NatCollection) => dt} ->: dt } }
  }

  @primitive object toDepArray extends Primitive with Builder {
    impl { n: Nat => impl { dt: DataType => (n `.` dt) ->: (n `*.` n2dtFun(_ => dt)) } }
  }

  @primitive object reduce extends Primitive with Builder {
    impl{ n: Nat => impl{ t: DataType =>
      (t ->: t ->: t) ->: t ->: (n`.`t) ->: t }}
  }

  @primitive object reduceSeq extends Primitive with Builder {
    impl{ n: Nat => impl{ s: DataType => impl{ t: DataType =>
      (t ->: s ->: t) ->: t ->: (n`.`s) ->: t }}}
  }

  @primitive object reduceSeqUnroll extends Primitive with Builder {
    impl{ n: Nat => impl{ s: DataType => impl{ t: DataType =>
      (t ->: s ->: t) ->: t ->: (n`.`s) ->: t }}}
  }

  @primitive object reorder extends Primitive with Builder {
    impl{ n: Nat => impl{ t: DataType =>
      (IndexType(n) ->: IndexType(n)) ->: // idxF
        (IndexType(n) ->: IndexType(n)) ->: // idxFinv
        (n`.`t) ->: (n`.`t) }}
  }

  @primitive object scanSeq extends Primitive with Builder {
    impl{ n: Nat => impl{ s: DataType => impl{ t: DataType =>
      (s ->: t ->: t) ->: t ->: (n `.` s) ->: (n `.` t) }}}
  }

  @primitive object slide extends Primitive with Builder {
    impl{ n: Nat => expl((sz: Nat) => expl((sp: Nat) => impl{ t: DataType =>
      ((sp * n + sz) `.` t) ->: ((1 + n) `.` sz `.` t) }))}
  }

  @primitive object circularBuffer extends Primitive with Builder {
    // TODO: should return a stream / sequential array, not an array
    impl{ n: Nat => expl((alloc: Nat) => expl((sz: Nat) =>
      impl{ s: DataType => impl{ t: DataType =>
        (s ->: t) ->: // function to load an input
          ((n + sz) `.` s) ->: ((1 + n) `.` sz `.` t) }}))}
  }

  // mainly to achieve register rotation
  @primitive object rotateValues extends Primitive with Builder {
    // TODO: should return a stream / sequential array, not an array
    impl{ n: Nat => expl((sz: Nat) => impl{ s: DataType =>
      (s ->: s) ->: // function to write a value
        ((n + sz) `.` s) ->: ((1 + n) `.` sz `.` s) })}
  }

  @primitive object snd extends Primitive with Builder {
    impl{ s: DataType => impl{ t: DataType => (s x t) ->: t }}
  }

  @primitive object split extends Primitive with Builder {
    expl( (n: Nat) => impl{ m: Nat => impl{ t: DataType =>
      ((m * n)`.`t) ->: (m`.`n`.`t) }})
  }

  @primitive object take extends Primitive with Builder {
    expl( (n: Nat) => impl{ m: Nat => impl{ t: DataType =>
      ((n + m)`.`t) ->: (n`.`t) }})
  }

  @primitive object transpose extends Primitive with Builder {
    impl{ n: Nat => impl{ m: Nat => impl{ dt: DataType =>
      (n`.`m`.`dt) ->: (m`.`n`.`dt) }}}
  }

  // if-then-else
  @primitive object select extends Primitive with Builder {
    impl{ t: DataType => bool ->: t ->: t ->: t }
  }

  @primitive object unzip extends Primitive with Builder {
    impl{ n: Nat => impl{ dt1: DataType => impl{ dt2: DataType =>
      (n`.`(dt1 x dt2)) ->: ((n`.`dt1) x (n`.`dt2)) }}}
  }

  @primitive object zip extends Primitive with Builder {
    impl{ n: Nat => impl{ a: DataType => impl{ b: DataType =>
      (n`.`a) ->: (n`.`b) ->: (n`.`(a x b)) }}}
  }

  @primitive object neg extends Primitive with Builder {
    impl{ t: DataType => t ->: t }
  }

  @primitive object not extends Primitive with Builder {
    bool ->: bool
  }

  // TODO: address https://github.com/rise-lang/rise/issues/11
  @primitive object add extends Primitive with Builder { impl{ t: DataType => t ->: t ->: t } }
  @primitive object sub extends Primitive with Builder { impl{ t: DataType => t ->: t ->: t } }
  @primitive object mul extends Primitive with Builder { impl{ t: DataType => t ->: t ->: t } }
  @primitive object div extends Primitive with Builder { impl{ t: DataType => t ->: t ->: t } }
  @primitive object mod extends Primitive with Builder { impl{ t: DataType => t ->: t ->: t } }

  @primitive object gt extends Primitive with Builder { impl{ t: DataType => t ->: t ->: bool } }
  @primitive object lt extends Primitive with Builder { impl{ t: DataType => t ->: t ->: bool } }
  @primitive object equal extends Primitive with Builder { impl{ t: DataType => t ->: t ->: bool } }
  @primitive object notEqual extends Primitive with Builder { impl{ t: DataType => t ->: t ->: bool } }

  // TODO: should vectorisation be in the core or not?

  // TODO: track alignment in type system?
  // TODO: address https://github.com/rise-lang/rise/issues/11
  @primitive object asVectorAligned extends Primitive with Builder {
    expl((n: Nat) => impl{ m: Nat => impl{ a: DataType =>
      ((m * n)`.`a) ->: (m`.`vec(n, a)) }})
  }

  // TODO: address https://github.com/rise-lang/rise/issues/11
  @primitive object asVector extends Primitive with Builder {
    expl((n: Nat) => impl{ m: Nat => impl{ t: DataType =>
      ((m * n)`.`t) ->: (m`.`vec(n, t)) }})
  }

  // TODO: address https://github.com/rise-lang/rise/issues/11
  @primitive object asScalar extends Primitive with Builder {
    impl{ n: Nat => impl{ m: Nat => impl{ t: DataType =>
      (m`.`vec(n, t)) ->: ((m * n)`.`t) }}}
  }

  // TODO: address https://github.com/rise-lang/rise/issues/11
  @primitive object vectorFromScalar extends Primitive with Builder {
    impl{ n: Nat => impl{ t: DataType =>
      t ->: vec(n, t) }}
  }

  @primitive case class printType(msg: String = "") extends Primitive with Builder {
    impl{ t: DataType => t ->: t }
  }

  @primitive case class typeHole(msg: String = "") extends Primitive with Builder {
    impl{ t: DataType => t }
  }
}
// scalastyle:on number.of.methods
