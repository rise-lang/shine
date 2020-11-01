package rise.core.util

object freshName {
  private var counter = 0

  def apply(prefix: String): String = {
    counter += 1
    prefix + counter
  }
}
