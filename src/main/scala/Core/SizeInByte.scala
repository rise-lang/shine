package Core

case class SizeInByte(size: Nat) {
  def *(rhs: Nat) = SizeInByte(size * rhs)
  def +(rhs: SizeInByte) = SizeInByte(size + rhs.size)

  override def toString = s"$size bytes"
}
