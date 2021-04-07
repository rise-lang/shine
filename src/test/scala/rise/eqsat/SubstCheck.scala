package rise.eqsat

class SubstCheck extends test_util.Tests {
  test("simple substitution") {
    val s = Subst.empty
    val x = PatternVar("x")
    val y = PatternVar("y")
    assert(s.get(x).isEmpty)
    assert(s.get(y).isEmpty)
    s.insert(x, EClassId(0))
    assert(s.get(x).contains(EClassId(0)))
    s.insert(x, EClassId(1))
    assert(s.get(x).contains(EClassId(1)))
    s.insert(y, EClassId(0))
    s.insert(x, EClassId(2))
    assert(s.get(y).contains(EClassId(0)))
    assert(s.get(x).contains(EClassId(2)))
  }
}
