package elevate.core.strategies

import elevate.core.{RewriteResult, Strategy, Success}

object debug {

  case class peek[P](f: P => Unit) extends Strategy[P] {
    def apply(p: P): RewriteResult[P] = { f(p); Success(p) }
    override def toString: String = s"peek(f)"
  }

  case class debug[P](msg: String) extends Strategy[P] {
    def apply(p: P): RewriteResult[P] = peek[P](x => println(msg + "\n" + x))(p)
    override def toString: String = "debug"
  }

  case class println[P](msg: String) extends Strategy[P] {
    def apply(e: P): RewriteResult[P] = peek[P](_ => println(msg))(e)
    override def toString: String = "println"
  }
}
