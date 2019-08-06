package idealised

import com.github.ghik.silencer.silent

package object traversal {
  // TODO? could replace Any with something more precise

  @silent("define classes/objects inside of package objects")
  trait ToChildren {
    def children: Seq[Any]
  }

  @silent("define classes/objects inside of package objects")
  trait FromChildren[T] {
    def rebuild: Seq[Any] => T
  }
}
