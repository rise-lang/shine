object newName {
  private var counter = 0

  def apply(prefix: String): String = {
    counter += 1
    prefix + counter
  }
}

trait T

case class TI(n: String) extends T

case class TB() extends T

def newTName: T = TI(newName("t"))

abstract class E {
  val t: T
  def setA(t: T): E
}

final case class L(x: String)(override val t: T = newTName) extends E {
  override def toString = s"${this.t}__$x"
  override def setA(t: T) = this.copy(x)(t)
}

final case class M(x: String)(override val t: T = newTName) extends E {
  override def toString = s"${this.t}__$x"
  override def setA(t: T) = this.copy(x)(t)
}

abstract class P extends E {
  val pt: String
  override def setA(t: T) = this
}

final case class D()(override val t: T = newTName) extends P {
  override def toString = s"${this.t}__${this.pt}"
  override val pt = "xxx"
  override def setA(t: T) = D()(t)
}

final case class U()(override val t: T = newTName) extends P {
  override def toString = s"${this.t}__${this.pt}"
  override val pt = "yyy"
  override def setA(t: T) = U()(t)
}

L("a")()
L("b")(TB())
val x = M("a")()
val y = x.setA(TB())
val n = D()()
val m = n.setA(TB())
val p = U()()
val q = p.setA(TB())
U()()
D()()
U()()
