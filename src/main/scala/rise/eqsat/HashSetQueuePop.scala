package rise.eqsat

import scala.collection.mutable.Queue

object HashSetQueuePop {
  def empty[V] = new HashSetQueuePop[V](HashSet.empty, Queue.empty)
}

/** This is a hash set with fast queue-ordered pops.
  *
  * Removing first/last from a HashSet is slow and not ordered.
  * Removing last from a LinkedHashSet is slow (O(n) list traversal).
  * Removing last from a TreeSet is better (log(N)) but order is not queue order.
  * This implements queue-ordered pops in O(1) by combining a HashSet and a Queue.
  *
  * Alternatively IndexSet could be ported from Rust to Scala (it is used by egg)
  * https://docs.rs/indexmap/1.0.2/indexmap/set/struct.IndexSet.html#method.pop
  */
class HashSetQueuePop[V](var map: HashSet[V], var queue: Queue[V]) {
  def +=(v: V): Unit = {
    if (map.add(v)) {
      queue += v
    }
  }

  def ++=(vs: Iterable[V]): Unit = vs.foreach { this += _ }

  def pop(): V = {
    val v = queue.dequeue()
    map.remove(v)
    v
  }

  def isEmpty: Boolean = queue.isEmpty
  def nonEmpty: Boolean = queue.nonEmpty
}
