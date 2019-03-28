package idealised

package object traversal {
  // TODO? could replace Any with something more precise

  trait ToChildren {
    def children: Seq[Any]
  }

  trait FromChildren[T] {
    def rebuild: Seq[Any] => T
  }
}
