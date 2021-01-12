package shine.OpenMP.primitives.functional

import shine.DPIA.Compilation.TranslationContext
import shine.DPIA.Phrases.Phrase
import shine.DPIA.Types.{AccType, CommType, DataType, ExpType}
import shine.DPIA.primitives.functional.AbstractMapLoop
import shine.DPIA.{->:, Nat}
import shine.OpenMP.primitives.intermediate

//noinspection TypeAnnotation
final case class MapPar(override val n: Nat,
                        override val dt1: DataType,
                        override val dt2: DataType,
                        override val f: Phrase[ExpType ->: ExpType],
                        override val array: Phrase[ExpType])
  extends AbstractMapLoop(n, dt1, dt2, f, array) {
  override def makeMap = MapPar

  override def makeMapI(n: Nat, dt1: DataType, dt2: DataType,
                        f: Phrase[->:[ExpType, ->:[AccType, CommType]]],
                        array: Phrase[ExpType],
                        out: Phrase[AccType])
                       (implicit context: TranslationContext): Phrase[CommType] =
    intermediate.MapParI(n, dt1, dt2, f, array, out)
}
