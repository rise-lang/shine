package shine.C

import shine.C
import shine.DPIA.Phrases.Identifier
import shine.DPIA.Types._
import shine.DPIA

package object AST {
  def makeParam(gen: CodeGenerator)(i: Identifier[_]): C.AST.ParamDecl = {
    // Turn array types into pointer types
    val paramType = getDataType(i) match {
      case DPIA.Types.ArrayType(_, dt) =>
        val baseDt = DataType.getBaseDataType(dt)
        C.AST.PointerType(gen.typ(baseDt))
      case DepArrayType(_, NatToDataLambda(_, dt)) =>
        val baseDt = DataType.getBaseDataType(dt)
        C.AST.PointerType(gen.typ(baseDt))
      case r: PairType => gen.typ(r)
      case dr: DepPairType => gen.typ(dr)
      case t: DPIA.Types.BasicType => gen.typ(t)
      case _: DataTypeIdentifier | _: NatToDataApply | DepArrayType(_, NatToDataIdentifier(_)) =>
        throw new Exception("This should not happen")
    }
    C.AST.ParamDecl(i.name, paramType)
  }

  def getDataType(i: Identifier[_]): DataType = i.t match {
    case ExpType(dataType, _) => dataType
    case AccType(dataType) => dataType
    case PhrasePairType(ExpType(dt1, _), AccType(dt2)) if dt1 == dt2 => dt1
    case _ => throw new Exception("This should not happen")
  }
}
