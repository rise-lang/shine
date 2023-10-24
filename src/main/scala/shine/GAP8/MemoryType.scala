package shine.GAP8

sealed trait MemoryType {
  def toAllocString: String
}

case object L1 extends MemoryType {
  override def toAllocString: String = "RT_ALLOC_CL_DATA"
}
case object L2 extends MemoryType {
  override def toAllocString: String = "RT_ALLOC_L2_CL_DATA"
}
