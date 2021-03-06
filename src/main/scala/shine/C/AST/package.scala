package shine.C

import shine.C
import shine.C.Compilation.CodeGenerator
import shine.DPIA.Phrases.Identifier
import shine.DPIA.Types._
import shine.DPIA

package object AST {
  sealed trait IncludeDirective
  case class IncludeHeader(name: String) extends IncludeDirective {
    override def toString: String = s"#include <$name>"
  }
  case class IncludeSource(path: String) extends IncludeDirective {
    override def toString: String = s"#include ${'"'}$path${'"'}"
  }

  // Turn array types into pointer types
  def makeParamTy(gen: CodeGenerator): DataType => Type = {
    case DPIA.Types.ArrayType(_, dt) =>
      val baseDt = DataType.getBaseDataType(dt)
      C.AST.PointerType(gen.typ(baseDt))
    case DepArrayType(_, NatToDataLambda(_, dt)) =>
      val baseDt = DataType.getBaseDataType(dt)
      C.AST.PointerType(gen.typ(baseDt))
    case r: PairType => gen.typ(r)
    case dr: DepPairType => gen.typ(dr)
    case t: DPIA.Types.BasicType => gen.typ(t)
    case dt => throw new Exception(s"did not expect $dt")
  }

  def makeParam(makeTy: DataType => Type)(i: Identifier[_]): C.AST.ParamDecl = {
    C.AST.ParamDecl(i.name, makeTy(getDataType(i)))
  }

  def getDataType(i: Identifier[_]): DataType = i.t match {
    case ExpType(dataType, _) => dataType
    case AccType(dataType) => dataType
    case PhrasePairType(ExpType(dt1, _), AccType(dt2)) if dt1 == dt2 => dt1
    case _ => throw new Exception("This should not happen")
  }
}
