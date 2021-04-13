package rise.eqsat

class UnionFindCheck extends test_util.Tests {
  test("union find") {
    val n = 10

    val uf = UnionFind.empty
    for (_ <- 0 until n) {
      uf.makeSet()
    }

    // test the initial condition of everyone in their own set
    for (i <- 0 until n) {
      assert(uf.parents(i) == EClassId(i))
    }

    // build up one set
    uf.union(EClassId(0), EClassId(1))
    uf.union(EClassId(0), EClassId(2))
    uf.union(EClassId(0), EClassId(3))

    // build up another set
    uf.union(EClassId(6), EClassId(7))
    uf.union(EClassId(6), EClassId(8))
    uf.union(EClassId(6), EClassId(9))

    // compress all paths
    for (i <- 0 until n) {
      uf.findMut(EClassId(i))
    }

    // indices:        0, 1, 2, 3, 4, 5, 6, 7, 8, 9
    val expected = Seq(0, 0, 0, 0, 4, 5, 6, 6, 6, 6)
    for (i <- 0 until n) {
      assert(uf.parents(i) == EClassId(expected(i)))
    }
  }
}
