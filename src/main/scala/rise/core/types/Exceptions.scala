package rise.core.types

import parser.ErrorMessage.{BLUE, ErrorMessage, No_Type_For_Variable, YELLOW}

import scala.collection.immutable.{AbstractSeq, LinearSeq}
import scala.xml.NodeSeq

case class InferenceException(msg: String, rs:Seq[Constraint],trace: Seq[Constraint], span:Option[parser.Span])
  extends Exception {
  override def toString: String = span match {
    case Some(sp) => {
      var error = "inference exception: "+msg+"\n"
      for(i <- 0 until rs.length){
        error += ErrorMessage.give_char_n_times(7, '_', Some(YELLOW()))+i+".te:\n"
        error += rs(i).constraintTypeError.toString
      }
      error +"<-"+sp.toUri
    }
//    case Some(sp) =>     s"inference exception in $sp: $msg\n${trace.mkString("---- trace ----\n",
//      "\n", "\n---------------")}"// + trace.isEmpty //trace is almost every time empty
    case None =>    if(rs.isEmpty) s"inference exception: $msg\n${trace.mkString("---- trace ----\n",
      "\n", "\n---------------")}"else s"inference exception: $msg: $rs\n${trace.mkString("---- trace ----\n",
      "\n", "\n---------------")}"
  }
}

object InferenceException {
//  def error(msg: String,rs:Seq[Constraint])(implicit trace: Seq[Constraint]): Nothing =
//    throw InferenceException(msg, trace, None)
  def error(msg: String,span:Option[parser.Span],rs:Seq[Constraint])(implicit trace: Seq[Constraint]): Nothing =
    throw InferenceException(msg, rs,trace,span)
  def error(msg: String,span:Option[parser.Span])(implicit trace: Seq[Constraint]): Nothing =
    throw InferenceException(msg, Nil,trace,span)
  def error_no_type(expr:rise.core.Expr)(implicit trace: Seq[Constraint]): Nothing = expr.span match {
    case Some(sp) => throw No_Type_For_Variable(sp,expr)
    case None =>throw InferenceException(s"$expr has no type", Nil,trace,None)
  }

}

case class NonIdentifierInBinderException[T](lambda: T,
                                          subbed: Nat) extends Exception {
  override def toString: String =
    s"substitution exception: the expression ($subbed):" +
      s"nat (scala type ${subbed.getClass.getName}) cannot " +
      s"be substituted for binder in the expression $lambda"
}