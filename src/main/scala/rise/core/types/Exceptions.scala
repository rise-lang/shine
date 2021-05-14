package rise.core.types

import scala.collection.immutable.{AbstractSeq, LinearSeq}
import scala.xml.NodeSeq

case class InferenceException(msg: String, trace: Seq[Constraint], span:Option[parser.Span])
  extends Exception {
  override def toString: String = span match {
    case Some(sp) =>     s"inference exception in $sp: $msg\n${trace.mkString("---- trace ----\n",
      "\n", "\n---------------")}"// + trace.isEmpty //trace is almost every time empty
    case None =>    s"inference exception: $msg\n${trace.mkString("---- trace ----\n",
      "\n", "\n---------------")}"// + trace.isEmpty
  }
}

object InferenceException {
  def error(msg: String)(implicit trace: Seq[Constraint]): Nothing =
    throw InferenceException(msg, trace, None)
  def error(msg: String, span:Option[parser.Span])(implicit trace: Seq[Constraint]): Nothing =
    throw InferenceException(msg, trace, span)
}

case class NonIdentifierInBinderException[T](lambda: T,
                                          subbed: Nat) extends Exception {
  override def toString: String =
    s"substitution exception: the expression ($subbed):" +
      s"nat (scala type ${subbed.getClass.getName}) cannot " +
      s"be substituted for binder in the expression $lambda"
}