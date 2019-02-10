package idealised.C
import idealised.DPIA.DSL._
import idealised.DPIA.ImperativePrimitives.Assign
import idealised.DPIA.IntermediatePrimitives.{DepMapSeqI, MapSeqI}
import idealised.DPIA.Phrases.{NatDependentLambda, Phrase}
import idealised.DPIA.Types._
import idealised.DPIA.freshName
import lift.arithmetic.NamedVar

class TranslationContext() extends idealised.DPIA.Compilation.TranslationContext {
  override def assign(dt: DataType,
                      lhs: Phrase[AccType],
                      rhs: Phrase[ExpType]): Phrase[CommandType] = {
    dt match {
      case _: ScalarType => Assign(dt, lhs, rhs)

      case RecordType(dt1, dt2) =>
        assign(dt1, recordAcc1(dt1, dt2, lhs), fst(rhs)) `;` assign(dt2, recordAcc2(dt1, dt2, lhs), snd(rhs))


      case ArrayType(n, et) =>
        MapSeqI(n, et, et, λ(ExpType(et))(x => λ(AccType(et))(a => assign(et, a, x) )), rhs, lhs)(this)

      case DepArrayType(n, i, et) =>
        val i_ = NamedVar(freshName())
        val et_ = DataType.substitute(i_, `for`=i, in=et)
        val k = NamedVar(freshName())
        val etk = DataType.substitute(k, `for`=i, in=et)
        DepMapSeqI(n, i_, et_, i_, et_,
          NatDependentLambda(k,
            λ(ExpType( etk ))(x => λ(AccType( etk ))(a => assign(etk, a, x) ))),
          rhs, lhs)(this)

      case x => throw new Exception(s"Don't know how to assign value of type $x")
    }
  }
}
