//package shine.DPIA
//
//import scala.collection.generic.{CanBuildFrom, MapFactory}
//import scala.collection.mutable
//
//object MutableIdentityHashMap extends MapFactory[MutableIdentityHashMap] {
//  override def empty[K, V]: MutableIdentityHashMap[K, V] =
//    new MutableIdentityHashMap[K, V]()
//
//  implicit def
//  canBuildFrom[K, V]: CanBuildFrom[Coll, (K, V), MutableIdentityHashMap[K, V]] =
//    new MapCanBuildFrom[K, V]
//}
//
//class MutableIdentityHashMap[K, V]
//  extends mutable.AbstractMap[K, V]
//    with mutable.Map[K, V]
//    with mutable.MapLike[K, V, MutableIdentityHashMap[K, V]] {
//
//  private val innerMap = new java.util.IdentityHashMap[K, V]()
//
//  override def empty: MutableIdentityHashMap[K, V] =
//    MutableIdentityHashMap.empty[K, V]
//
//  override def +=(kv: (K, V)): MutableIdentityHashMap.this.type = {
//    innerMap put (kv._1, kv._2)
//    this
//  }
//
//  override def -=(key: K): MutableIdentityHashMap.this.type = {
//    innerMap remove key
//    this
//  }
//
//  override def get(key: K): Option[V] = {
//    if (innerMap containsKey key) Some(innerMap get key)
//    else None
//  }
//
//  override def iterator: Iterator[(K, V)] = new Iterator[(K, V)] {
//    private val mapIter = innerMap.entrySet.iterator
//
//    override def hasNext: Boolean = mapIter.hasNext
//
//    override def next(): (K, V) = {
//      val nextEntry = mapIter.next()
//      (nextEntry.getKey, nextEntry.getValue)
//    }
//  }
//}
