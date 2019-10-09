package idealised.C

import idealised.DPIA
import idealised.DPIA.DSL._
import idealised.DPIA.ImperativePrimitives.Assign
import idealised.DPIA.IntermediatePrimitives.{DepMapSeqI, MapSeqI}
import idealised.DPIA.Phrases.{DepLambda, Phrase}
import idealised.DPIA.Types._
import idealised.DPIA.freshName

class TranslationContext() extends idealised.DPIA.Compilation.TranslationContext {
  override def assign(dt: DataType,
                      lhs: Phrase[AccType],
                      rhs: Phrase[ExpType]): Phrase[CommType] = {
    dt match {
      case _: ScalarType => Assign(dt, lhs, rhs)

      case _: IndexType => Assign(dt, lhs, rhs)

      //TODO think about this more thoroughly
      case RecordType(dt1, dt2) => Assign(dt, lhs, rhs)
        // assign(dt1, recordAcc1(dt1, dt2, lhs), fst(rhs)) `;` assign(dt2, recordAcc2(dt1, dt2, lhs), snd(rhs))

      //TODO makes a decision. Not allowed!
      case ArrayType(n, et) =>
        MapSeqI(n, et, et, λ(ExpType(et, read))(x => λ(AccType(et))(a => assign(et, a, x) )), rhs, lhs)(this)

      //TODO makes a decision. Not allowed!
      case DepArrayType(n, ft) =>
        DepMapSeqI(n, ft, ft,
          depFun[NatKind](k =>
            λ(ExpType(ft(k), read))(x => λ(AccType( ft(k) ))(a => assign(ft(k), a, x) ))),
          rhs, lhs)(this)

      case x => throw new Exception(s"Don't know how to assign value of type $x")
    }
  }
}
