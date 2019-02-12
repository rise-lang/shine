package object idealised {
  // reverse function application in the style of F#
  implicit class Pipe[A](val a: A) extends AnyVal {
    def |>[B](f: A => B): B = f(a)
  }
}
