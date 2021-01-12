package shine.cuda

import shine.DPIA.DSL._
import shine.DPIA.ImperativePrimitives.Assign
import shine.DPIA.Phrases.Phrase
import shine.DPIA.Types.{AccType, CommType, DataType, ExpType, Fragment, read}
import shine.DPIA.{accT, expT}
import shine.cuda.primitives.imperative.ForFragmentElements

class TranslationContext() extends shine.OpenCL.TranslationContext {
  override def assign(dt: DataType,
                      lhs: Phrase[AccType],
                      rhs: Phrase[ExpType]): Phrase[CommType] = {
    dt match {
      case f: Fragment =>
        ForFragmentElements(f, rhs, lhs,
          λ(expT(dt, read))(x =>
            λ(accT(dt))(o =>
              Assign(dt, o, x))))

      case _ =>
        super.assign(dt, lhs, rhs)
    }
  }
}
