package shine.C

import shine.DPIA.Types.DataType

object ParamMetaData {
  object Kind extends Enumeration {
    type ParamKind = Value
    val input, output, temporary = Value
  }
}

case class ParamMetaData(typ: DataType, kind: ParamMetaData.Kind.Value)
