package rise.core.types

sealed trait Fragment

object Fragment {
  object AMatrix extends Fragment { override def toString = "AMatrix"}
  object BMatrix extends Fragment { override def toString = "BMatrix"}
  object Accumulator extends Fragment { override def toString = "Accumulator"}
}

final case class FragmentIdentifier(name: String) extends Fragment {
  override def toString: String = name
}
