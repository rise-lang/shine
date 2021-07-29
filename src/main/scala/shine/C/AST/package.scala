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
  def makeParamTy(gen: CodeGenerator): rise.core.types.DataType => Type = {
    case rise.core.types.DataType.ArrayType(_, dt) =>
      val baseDt = DataType.getBaseDataType(dt)
      C.AST.PointerType(gen.typ(baseDt))
    case rise.core.types.DataType.DepArrayType(_, rise.core.types.NatToDataLambda(_, dt)) =>
      val baseDt = DataType.getBaseDataType(dt)
      C.AST.PointerType(gen.typ(baseDt))
    case r: rise.core.types.DataType.PairType => gen.typ(r)
    case dr: rise.core.types.DataType.DepPairType[_, _] => gen.typ(dr)
    case t: rise.core.types.DataType.ScalarType => gen.typ(t)
    case rise.core.types.DataType.NatType => gen.typ(rise.core.types.DataType.NatType)
    case t: rise.core.types.DataType.FragmentType => gen.typ(t)
    case t: rise.core.types.DataType.IndexType =>gen.typ(t)
    case dt => throw new Exception(s"did not expect $dt")
  }

  def makeParam(makeTy: rise.core.types.DataType => Type)(i: Identifier[_]): C.AST.ParamDecl = {
    C.AST.ParamDecl(i.name, makeTy(getDataType(i)))
  }

  def getDataType(i: Identifier[_]): rise.core.types.DataType = i.t match {
    case ExpType(dataType, _) => dataType
    case AccType(dataType) => dataType
    case PhrasePairType(ExpType(dt1, _), AccType(dt2)) if dt1 == dt2 => dt1
    case _ => throw new Exception("This should not happen")
  }
}
