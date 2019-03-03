package idealised.C
import idealised.DPIA.DSL._
import idealised.DPIA.ImperativePrimitives.Assign
import idealised.DPIA.IntermediatePrimitives.{DepMapSeqI, MapSeqI}
import idealised.DPIA.Phrases.{NatDependentLambda, Phrase}
import idealised.DPIA.Types.{AccType, ArrayType, CommandType, DataType, DepArrayType, ExpType, RecordType, ScalarType}
import idealised.DPIA.{NatDataTypeFunction, NatIdentifier, freshName}
import idealised.SurfaceLanguage.Types.NatDependentFunctionType
import lift.arithmetic.{NamedVar, RangeAdd}

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
        //val i_ = NamedVar(freshName())
        //val et_ = DataType.substitute(i_, `for`=i, in=et)
        val k = NamedVar(freshName())
        //val etk = DataType.substitute(k, `for`=i, in=et)

        val ft1 = NatDataTypeFunction(n, (x:NatIdentifier) => DataType.substitute(x, `for`=i, in=et))
        val ft2 = NatDataTypeFunction(n, (x:NatIdentifier) => DataType.substitute(x, `for`=i, in=et))

        DepMapSeqI(n, ft1, ft2,
          NatDependentLambda(k,
            λ(ExpType( ft2(k) ))(x => λ(AccType( ft2(k) ))(a => assign(ft2(k), a, x) ))),
          rhs, lhs)(this)

      case _ => throw new Exception("This should not happen")
    }
  }
}
