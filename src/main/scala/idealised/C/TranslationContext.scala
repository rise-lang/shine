package idealised.C
import idealised.DPIA.DSL.λ
import idealised.DPIA.ImperativePrimitives.Assign
import idealised.DPIA.IntermediatePrimitives.{DepMapSeqI, MapSeqI}
import idealised.DPIA.Phrases.{NatDependentLambda, Phrase}
import idealised.DPIA.Types.{AccType, ArrayType, CommandType, DataType, DepArrayType, ExpType, ScalarType}
import idealised.DPIA.freshName
import lift.arithmetic.NamedVar

class TranslationContext() extends idealised.DPIA.Compilation.TranslationContext {
  override def assign(dt: DataType,
                      lhs: Phrase[AccType],
                      rhs: Phrase[ExpType]): Phrase[CommandType] = {
    dt match {
      case _: ScalarType => Assign(dt, lhs, rhs)

      case ArrayType(n, et) =>
        MapSeqI(n, et, et, λ(ExpType(et))(x => λ(AccType(et))(a => assign(et, a, x) )), rhs, lhs)

      case DepArrayType(n, i, et) =>
        val i_ = NamedVar(freshName())
        val et_ = DataType.substitute(i_, `for`=i, in=et)
        val k = NamedVar(freshName())
        val etk = DataType.substitute(k, `for`=i, in=et)
        DepMapSeqI(n, i_, et_, i_, et_,
          NatDependentLambda(k,
            λ(ExpType( etk ))(x => λ(AccType( etk ))(a => assign(etk, a, x) ))),
          rhs, lhs)

      case _ => throw new Exception("This should not happen")
    }
  }
}
