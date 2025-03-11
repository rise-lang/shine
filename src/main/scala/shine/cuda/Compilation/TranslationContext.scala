package shine.cuda.Compilation

import rise.core.types.DataType.FragmentType
import rise.core.types.{DataType, read}
import shine.DPIA.DSL._
import shine.DPIA.Phrases.Phrase
import shine.DPIA.Types.{AccType, CommType, ExpType}
import shine.DPIA.primitives.imperative.Assign
import shine.DPIA.{accT, expT}
import shine.cuda.primitives.imperative.ForFragment

class TranslationContext() extends shine.OpenCL.Compilation.TranslationContext {
  override def assign(dt: DataType,
                      lhs: Phrase[AccType],
                      rhs: Phrase[ExpType]): Phrase[CommType] = {
    dt match {
      case FragmentType(rows, columns, layers, dt, frag, layout) =>
        ForFragment(rows, columns, layers, dt, frag, layout, rhs, lhs,
          fun(expT(dt, read))(x =>
            fun(accT(dt))(o =>
              Assign(dt, o, x))))

      case _ =>
        super.assign(dt, lhs, rhs)
    }
  }
}
