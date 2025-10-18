package shine.GAP8

sealed trait DMATransferType {
  def toGAP8string: String
}

case object L1toL2 extends DMATransferType {
  override def toGAP8string: String = "RT_DMA_DIR_LOC2EXT"
}

case object L2toL1 extends DMATransferType {
  override def toGAP8string: String = "RT_DMA_DIR_EXT2LOC"
}