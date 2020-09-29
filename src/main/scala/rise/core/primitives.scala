package rise.core

import arithexpr.arithmetic.BigSum
import rise.core.TypeLevelDSL._
import rise.core.types._
import rise.macros.Primitive.primitive

// noinspection DuplicatedCode
// scalastyle:off number.of.methods
object primitives {

  @primitive case class makeArray(n: Int) extends Primitive with Builder {
    implDT(t => {
      def tRec(m: Int, dt: DataType): Type =
        if (m <= 0) {
          ArrayType(n, dt)
        } else {
          dt ->: tRec(m - 1, dt)
        }
      tRec(n, t)
    })
  }

  @primitive object cast extends Primitive with Builder {
    implBT(s => implBT(t => s ->: t))
  }

  @primitive object depJoin extends Primitive with Builder {
    implNat(n => implN2N(lenF => implDT(dt =>
      (n `..` (i => lenF(i) `.` dt)) ->:
        (BigSum(from = 0, upTo = n - 1, n => lenF(n)) `.` dt))))
  }

  @primitive object depMapSeq extends Primitive with Builder {
    implNat(n =>
      implN2DT(ft1 => implN2DT(ft2 =>
        forallNat(k => ft1(k) ->: ft2(k)) ->: (n`..`ft1) ->: (n`..`ft2))))
  }

  @primitive object depZip extends Primitive with Builder {
    implNat(n => implN2DT(ft1 => implN2DT(ft2 =>
      (n`..`ft1) ->: (n`..`ft2) ->: (n`..`(i => ft1(i) x ft2(i))))))
  }

  @primitive object drop extends Primitive with Builder {
    forallNat(n => implNat(m => implDT(t =>
      ((n + m)`.`t) ->: (m`.`t))))
  }

  @primitive object fst extends Primitive with Builder {
    implDT(s => implDT(t =>
      (s x t) ->: s))
  }

  @primitive object gather extends Primitive with Builder {
    implNat(n => implNat(m => implDT(t =>
      (m`.`IndexType(n)) ->: (n`.`t) ->: (m`.`t))))
  }

  @primitive object generate extends Primitive with Builder {
    implNat(n => implDT(t =>
      (IndexType(n) ->: t) ->: ArrayType(n, t)))
  }

  @primitive object idx extends Primitive with Builder {
    implNat(n => implDT(t =>
      IndexType(n) ->: (n`.`t) ->: t))
  }

  @primitive object id extends Primitive with Builder { implDT(t => t ->: t) }

  @primitive object indexAsNat extends Primitive with Builder {
    implNat(n => IndexType(n) ->: NatType)
  }

  @primitive object iterate extends Primitive with Builder {
    implNat(n => implNat(m =>
      forallNat(k => implDT(t => forallNat(l =>
        ((l * n)`.`t) ->: (l`.`t)) ->: ((m * n.pow(k))`.`t) ->: (m`.`t)))))
  }

  @primitive object join extends Primitive with Builder {
    implNat(n => implNat(m => implDT(t =>
      (n`.`m`.`t) ->: ((n * m)`.`t))))
  }

  @primitive object let extends Primitive with Builder {
    implDT(s => implDT(t =>
      s ->: (s ->: t) ->: t))
  }

  @primitive object map extends Primitive with Builder {
    implNat(n => implDT(s => implDT(t =>
      (s ->: t) ->: (n `.` s) ->: (n `.` t))))
  }

  @primitive object mapFst extends Primitive with Builder {
    implDT(s => implDT(t => implDT(s2 =>
      (s ->: s2) ->: (s x t) ->: (s2 x t))))
  }

  @primitive object mapSnd extends Primitive with Builder {
    implDT(s => implDT(t => implDT(t2 =>
      (t ->: t2) ->: (s x t) ->: (s x t2))))
  }

  @primitive object mapSeq extends Primitive with Builder {
    implNat(n => implDT(s => implDT(t =>
      (s ->: t) ->: (n`.`s) ->: (n`.`t))))
  }

  @primitive object mapStream extends Primitive with Builder {
    implNat(n => implDT(s => implDT(t =>
      // stream to stream
      (s ->: t) ->: (n`.`s) ->: (n`.`t))))
  }

  @primitive object iterateStream extends Primitive with Builder {
    implNat(n => implDT(s => implDT(t =>
      // stream to array (or should it be stream to value?)
      (s ->: t) ->: (n`.`s) ->: (n`.`t))))
  }

  @primitive object mapSeqUnroll extends Primitive with Builder {
    implNat(n => implDT(s => implDT(t =>
      (s ->: t) ->: (n `.` s) ->: (n `.` t))))
  }

  @primitive object toMem extends Primitive with Builder { implDT(t => t ->: t) }

  @primitive object natAsIndex extends Primitive with Builder {
    forallNat(n => NatType ->: IndexType(n))
  }

  // TODO? could be expressed in terms of a pad idx -> val
  @primitive object padCst extends Primitive with Builder {
    implNat(n =>
      forallNat(l =>
        forallNat(q => implDT(t => t ->: (n`.`t) ->: ((l + n + q)`.`t)))))
  }

  @primitive object padEmpty extends Primitive with Builder {
    implNat(n =>
      forallNat(r => implDT(t => (n`.`t) ->: ((n + r)`.`t))))
  }

  // TODO? could be expressed in terms of a pad idx -> idx or idx -> val
  @primitive object padClamp extends Primitive with Builder {
    implNat(n =>
      forallNat(l => forallNat(q => implDT(t =>
        (n`.`t) ->: ((l + n + q)`.`t)))))
  }

  @primitive object partition extends Primitive with Builder {
    implNat(n => implDT(dt =>
      forallNat(m =>
        forallN2N(lenF =>
          (n`.`dt) ->: (m`..`(i => lenF(i)`.`dt))))))
  }

  @primitive object pair extends Primitive with Builder {
    implDT(s => implDT(t =>
      s ->: t ->: (s x t)))
  }

  @primitive object dpair extends Primitive with Builder {
    implN2DT(fdt => forallNat(n => fdt(n) ->: n2dPairT(fdt(_))))
  }

  @primitive object dmatch extends Primitive with Builder {
    implN2DT(ft => implDT(tOut =>
      n2dPairT(ft(_)) ->: forallNat(m => ft(m) ->: tOut) ->: tOut))
  }

  @primitive object reduce extends Primitive with Builder {
    implNat(n => implDT(t =>
      (t ->: t ->: t) ->: t ->: (n`.`t) ->: t))
  }

  @primitive object reduceSeq extends Primitive with Builder {
    implNat(n => implDT(s => implDT(t =>
      (t ->: s ->: t) ->: t ->: (n`.`s) ->: t)))
  }

  @primitive object reduceSeqUnroll extends Primitive with Builder {
    implNat(n => implDT(s => implDT(t =>
      (t ->: s ->: t) ->: t ->: (n`.`s) ->: t)))
  }

  @primitive object reorder extends Primitive with Builder {
    implNat(n => implDT(t =>
      (IndexType(n) ->: IndexType(n)) ->: // idxF
        (IndexType(n) ->: IndexType(n)) ->: // idxFinv
        (n`.`t) ->: (n`.`t)))
  }

  @primitive object scanSeq extends Primitive with Builder {
    implNat(n => implDT(s => implDT(t =>
      (s ->: t ->: t) ->: t ->: (n `.` s) ->: (n `.` t))))
  }

  @primitive object slide extends Primitive with Builder {
    implNat(n => forallNat(sz => forallNat(sp => implDT(t =>
      ((sp * n + sz) `.` t) ->: ((1 + n) `.` sz `.` t)))))
  }

  @primitive object circularBuffer extends Primitive with Builder {
    // TODO: should return a stream / sequential array, not an array
    implNat(n => forallNat(alloc => forallNat(sz => implDT(s => implDT(t =>
      (s ->: t) ->: // function to load an input
        ((n + sz) `.` s) ->: ((1 + n) `.` sz `.` t))))))
  }

  // mainly to achieve register rotation
  @primitive object rotateValues extends Primitive with Builder {
    // TODO: should return a stream / sequential array, not an array
    implNat(n => forallNat(sz => implDT(s =>
      (s ->: s) ->: // function to write a value
        ((n + sz) `.` s) ->: ((1 + n) `.` sz `.` s))))
  }

  @primitive object snd extends Primitive with Builder {
    implDT(s => implDT(t => (s x t) ->: t))
  }

  @primitive object split extends Primitive with Builder {
    forallNat(n => implNat(m => implDT(t =>
      ((m * n)`.`t) ->: (m`.`n`.`t))))
  }

  @primitive object take extends Primitive with Builder {
    forallNat(n => implNat(m => implDT(t =>
      ((n + m)`.`t) ->: (n`.`t))))
  }

  @primitive object transpose extends Primitive with Builder {
    implNat(n => implNat(m => implDT(dt =>
      (n`.`m`.`dt) ->: (m`.`n`.`dt))))
  }

  // if-then-else
  @primitive object select extends Primitive with Builder {
    implDT(t => bool ->: t ->: t ->: t)
  }

  @primitive object unzip extends Primitive with Builder {
    implNat(n => implDT(dt1 => implDT(dt2 =>
      (n`.`(dt1 x dt2)) ->: ((n`.`dt1) x (n`.`dt2)))))
  }

  @primitive object zip extends Primitive with Builder {
    implNat(n => implDT(a => implDT(b =>
      (n`.`a) ->: (n`.`b) ->: (n`.`(a x b)))))
  }

  @primitive object neg extends Primitive with Builder {
    implBT(t => t ->: t)
  }

  @primitive object not extends Primitive with Builder {
    bool ->: bool
  }

  @primitive object add extends Primitive with Builder { implBT(t => t ->: t ->: t) }
  @primitive object sub extends Primitive with Builder { implBT(t => t ->: t ->: t) }
  @primitive object mul extends Primitive with Builder { implBT(t => t ->: t ->: t) }
  @primitive object div extends Primitive with Builder { implBT(t => t ->: t ->: t) }
  @primitive object mod extends Primitive with Builder { implBT(t => t ->: t ->: t) }

  @primitive object gt extends Primitive with Builder { implBT(t => t ->: t ->: bool) }
  @primitive object lt extends Primitive with Builder { implBT(t => t ->: t ->: bool) }
  @primitive object equal extends Primitive with Builder { implBT(t => t ->: t ->: bool) }

  // TODO: should vectorisation be in the core or not?

  // TODO: track alignment in type system?
  @primitive object asVectorAligned extends Primitive with Builder {
    forallNat(n => implNat(m => implDT(a =>
      ((m * n)`.`a) ->: (m`.`vec(n, a)))))
  }

  @primitive object asVector extends Primitive with Builder {
    forallNat(n => implNat(m => implST(t =>
      ((m * n)`.`t) ->: (m`.`vec(n, t)))))
  }

  @primitive object asScalar extends Primitive with Builder {
    implNat(n => implNat(m => implST(t =>
      (m`.`vec(n, t)) ->: ((m * n)`.`t))))
  }

  @primitive object vectorFromScalar extends Primitive with Builder {
    implNat(n => implST(t =>
      t ->: vec(n, t)))
  }

  @primitive case class printType(msg: String = "") extends Primitive with Builder {
    implType(t => t ->: t)
  }

  @primitive case class typeHole(msg: String = "") extends Primitive with Builder {
    implType(t => t)
  }
}
// scalastyle:on number.of.methods
