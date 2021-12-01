package shine.C.AST

import shine.DPIA.Phrases.Identifier
import shine.DPIA.Types.BasePhraseType
import rise.core.types.DataType

object ParamKind {
  object Kind extends Enumeration {
    type ParamKind = Value
    val input, output, temporary = Value
  }

  def input(p: Identifier[_ <: BasePhraseType]): ParamKind =
   input(p.`type`.dataType)

  def input(t: DataType): ParamKind =
    ParamKind(t, Kind.input)

  def output(p: Identifier[_ <: BasePhraseType]): ParamKind =
    output(p.`type`.dataType)

  def output(t: DataType): ParamKind =
    ParamKind(t, Kind.output)

  def temporary(p: Identifier[_ <: BasePhraseType]): ParamKind =
    temporary(p.`type`.dataType)

  def temporary(t: DataType): ParamKind =
    ParamKind(t, Kind.temporary)
}

case class ParamKind(typ: DataType, kind: ParamKind.Kind.Value)
