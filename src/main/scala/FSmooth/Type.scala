package FSmooth

sealed trait Type

final case class TypeVar(name: String) extends Type

sealed trait ExpressionType extends Type

final case class FunType(inTs: Seq[Type], outT: Type) extends Type {
  override def toString: String = s"(${inTs.mkString(" -> ")} -> $outT)"
}

final case class ExpressionTypeVar(name: String) extends ExpressionType

sealed trait Num extends ExpressionType

case object Bool extends ExpressionType

final case class Array(elemType: ExpressionType) extends ExpressionType {
  override def toString: String = s"Array<$elemType>"
}

final case class Pair(fst: ExpressionType, snd: ExpressionType) extends ExpressionType {
  override def toString: String = s"$fst x $snd"
}

case object Double extends Num

case object Index extends Num

case object Card extends Num
