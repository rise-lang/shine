package rise.core.types

case class InferenceException(msg: String, trace: Seq[Constraint])
  extends Exception {
  override def toString: String =
    s"inference exception: $msg\n${trace.mkString("---- trace ----\n",
      "\n", "\n---------------")}"
}

object InferenceException {
  def error(msg: String)(implicit trace: Seq[Constraint]): Nothing =
    throw InferenceException(msg, trace)
}
