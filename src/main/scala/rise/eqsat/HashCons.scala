package rise.eqsat

object HashCons {
  def empty[Node, Id, Data]: HashCons[Node, Id, Data] =
    new HashCons(HashMap(), HashMap())
}

class HashCons[Node, Id, Data](
  var memo: HashMap[Node, Id],
  var nodes: HashMap[Id, (Node, Data)],
) {
  def add(node: Node, data: Node => Data, makeId: Int => Id): Id = {
    memo.getOrElseUpdate(node, {
      val id = makeId(nodes.size)
      nodes += id -> (node, data(node))
      memo += node -> id
      id
    })
  }

  def addWithSimplification(node: Node, makeId: Int => Id,
                            simplify: Node => Node, data: Node => Data): Id = {
    memo.get(node) match {
      case Some(id) => id
      case None =>
        val simplified = simplify(node) // this may add more hash-consed values
        add(simplified, data, makeId)
    }
  }

  def get(id: Id): (Node, Data) = nodes(id)
}
