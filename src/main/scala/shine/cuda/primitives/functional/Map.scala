package shine.cuda.primitives.functional

import shine.DPIA.Compilation.TranslationContext
import shine.DPIA.Compilation.TranslationToImperative._
import shine.DPIA.DSL._
import shine.DPIA.Phrases._
import shine.DPIA.Semantics.OperationalSemantics
import shine.DPIA.Semantics.OperationalSemantics._
import shine.DPIA.Types.DataType._
import shine.DPIA.Types._
import shine.DPIA._
import shine.OpenCL.ParallelismLevel
import shine.cuda.primitives.intermediate.MapI
import shine.macros.Primitive.expPrimitive

@expPrimitive
final case class Map(level: ParallelismLevel,
                     dim: Char)
                    (val n: Nat,
                     val dt1: DataType,
                     val dt2: DataType,
                     val f: Phrase[ExpType ->: ExpType],
                     val array: Phrase[ExpType]
                    ) extends ExpPrimitive with ConT with AccT {
  f :: expT(dt1, read) ->: expT(dt2, write)
  array :: expT(n `.` dt1, read)
  override val t: ExpType = expT(n `.` dt2, write)

  def acceptorTranslation(A: Phrase[AccType])
                         (implicit context: TranslationContext): Phrase[CommType] = {
    con(array)(λ(expT(n `.` dt1, read))(x =>
      MapI(level, dim)(n, dt1, dt2,
        λ(expT(dt1, read))(x => λ(accT(dt2))(o => acc(f(x))(o))),
        x, A)
    ))
  }

  def continuationTranslation(C: Phrase[ExpType ->: CommType])
                             (implicit context: TranslationContext): Phrase[CommType] = {
    println("WARNING: map loop continuation translation allocates memory")
    // TODO should be removed
    `new`(n `.` dt2, λ(varT(n `.` dt2))(tmp =>
      acc(this)(tmp.wr) `;` C(tmp.rd)))
  }

  override def eval(s: Store): Data = {
    val fE = OperationalSemantics.eval(s, f)
    OperationalSemantics.eval(s, array) match {
      case ArrayData(xs) =>
        ArrayData(xs.map { x =>
          OperationalSemantics.eval(s, fE(Literal(x)))
        })

      case _ => throw new Exception("This should not happen")
    }
  }
}

