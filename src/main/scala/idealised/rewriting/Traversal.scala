package idealised.rewriting

sealed trait Traversal

case object BFS extends Traversal

case object DFS extends Traversal
