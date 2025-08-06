package shine.GAP8

sealed trait ConvolutionFilterSize {
  def toBackendConst: String
  def functionName: String
}
case object _3x3 extends ConvolutionFilterSize {
  override def toBackendConst: String = "HWCE_CONV3x3"
  override def functionName: String = "HWCE_ProcessOneTile3x3_MultiOut"
}
case object _5x5 extends ConvolutionFilterSize {
  override def toBackendConst: String = "HWCE_CONV5x5"
  override def functionName: String = "HWCE_ProcessOneTile5x5"
}
case object _7x7 extends ConvolutionFilterSize {
  override def toBackendConst: String = "HWCE_CONV7x7"
  override def functionName: String = "HWCE_ProcessOneTile7x7"
}
case object _7x4 extends ConvolutionFilterSize {
  override def toBackendConst: String = "HWCE_CONV7x7"
  override def functionName: String = "HWCE_ProcessOneTile7x4"
}
