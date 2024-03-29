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

case class NonIdentifierInBinderException[T](lambda: T,
                                             subbed: Nat) extends Exception {
  override def toString: String =
    s"substitution exception: the expression ($subbed):" +
      s"nat (scala type ${subbed.getClass.getName}) cannot " +
      s"be substituted for binder in the expression $lambda"
}

case class TypeException(msg: String) extends Exception {
  override def toString: String = s"type exception: $msg"
}
