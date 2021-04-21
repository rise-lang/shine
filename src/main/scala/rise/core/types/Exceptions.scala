package rise.core.types

import scala.collection.immutable.{AbstractSeq, LinearSeq}
import scala.xml.NodeSeq

case class InferenceException(msg: String, trace: Seq[Constraint], span:List[Option[parser.Span]])
  extends Exception {
  override def toString: String =
    s"inference exception in $span: $msg\n${trace.mkString("---- trace ----\n",
      "\n", "\n---------------")}"
}

object InferenceException {
  def error(msg: String, span:List[Option[parser.Span]])(implicit trace: Seq[Constraint]): Nothing =
    throw InferenceException(msg, trace, span)
  def error(msg: String, span:Option[parser.Span])(implicit trace: Seq[Constraint]): Nothing =
    throw InferenceException(msg, trace, span::Nil)
}

case class NonIdentifierInBinderException[T](lambda: T,
                                          subbed: Nat) extends Exception {
  override def toString: String =
    s"substitution exception: the expression ($subbed):" +
      s"nat (scala type ${subbed.getClass.getName}) cannot " +
      s"be substituted for binder in the expression $lambda"
}