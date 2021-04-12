package shine.cuda.primitives.functional

import shine.DPIA.DSL.{`new` => _}
import shine.DPIA.Phrases._
import shine.DPIA.Types._
import shine.DPIA._
import shine.macros.Primitive.expPrimitive

/**
  * Returns a copy in shared memory of data in global memory ({@link GlobalToSharedAcc}).
  * @param dt          datatype of data which should be copied
  * @param inputGlobal data in global memory which should be copied to shared memory
  */
@expPrimitive
final case class GlobalToShared(dt: DataType,
                                inputGlobal: Phrase[ExpType]) extends ExpPrimitive {

  inputGlobal :: expT(dt, write)
  override val t: ExpType = expT(dt, read)
}

