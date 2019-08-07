package FSmooth

sealed trait Type

sealed trait ExpressionType extends Type

final case class FunType(inTs: Seq[Type], outT: ExpressionType) extends Type {
  override def toString: String = s"(${inTs.mkString("->")} -> $outT)"
}

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
