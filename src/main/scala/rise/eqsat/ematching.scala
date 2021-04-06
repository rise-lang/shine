package rise.eqsat

import scala.collection.mutable

// NOTE: we currently only support Nat pattern variables if they are directly the argument of a DepApp node
object ematching {
  object AbstractMachine {
    def init(eclass: EClassId): AbstractMachine =
      AbstractMachine(Vec(eclass), Vec())
  }

  case class AbstractMachine(regs: Vec[EClassId],
                             natRegs: Vec[Nat]) {
    def reg(r: Reg): EClassId = regs(r.n)
    def natReg(r: NatReg): Nat = natRegs(r.n)

    def run[D](egraph: EGraph[D],
               instructions: Seq[Instruction],
               yieldFn: () => Unit): Unit = {
      var instrs = instructions
      while (instrs.nonEmpty) {
        instrs.head match {
          case Bind(node, i, out, natOut) =>
            forEachMatchingNode(egraph.get(reg(i)), node, { matched =>
              regs.remove(out.n, regs.size - out.n)
              natRegs.remove(natOut.n, natRegs.size - natOut.n)
              matched.children().foreach(id => regs += id)
              matched.nats().foreach(n => natRegs += n)
              run(egraph, instrs.tail, yieldFn)
            })
            return
          case Compare(i, j) =>
            if (egraph.find(reg(i)) != egraph.find(reg(j))) {
              return
            }
          case NatCompare(i, j) =>
            if (natReg(i) != natReg(j)) {
              return
            }
          case NatCheck(i, n) =>
            if (natReg(i) != n) {
              return
            }
        }
        instrs = instrs.tail
      }

      yieldFn()
    }
  }

  sealed trait Instruction
  // try all matches of `node` in `i`
  case class Bind(node: MNode, i: Reg, out: Reg, natOut: NatReg) extends Instruction
  // handle repeated variable occurrences
  case class Compare(i: Reg, j: Reg) extends Instruction
  case class NatCompare(i: NatReg, j: NatReg) extends Instruction
  case class NatCheck(i: NatReg, n: Nat) extends Instruction

  case class Reg(var n: Int)
  case class NatReg(var n: Int)
  case class TypeReg(var n: Int)
  case class DataTypeReg(var n: Int)

  class Program(val instructions: Vec[Instruction],
                var v2r: HashMap[PatternVar, Reg], // TODO? HashMap[_, _] -> compact Vec[_]
                var ni2r: HashMap[NatPatternVar, NatReg],
                var t2r: HashMap[TypePatternVar, TypeReg],
                var dt2r: HashMap[DataTypePatternVar, DataTypeReg]) {
    def run[D](egraph: EGraph[D], eclass: EClassId): Vec[Subst] = {
      val machine = AbstractMachine.init(eclass)

      val substs = Vec.empty[Subst]
      machine.run(egraph, instructions.toSeq, { () =>
        val substExprs = VecMap(v2r.iterator.map { case (v, reg) => (v, machine.reg(reg)) }.to(Vec))
        val substNats = VecMap(ni2r.iterator.map { case (v, reg) => (v, machine.natReg(reg)) }.to(Vec))
        val substTypes = ??? // VecMap(t2r.iterator.map { case (v, reg) => (v, machine.typeReg(reg)) }.to(Vec))
        val substDataTypes = ??? // VecMap(dt2r.iterator.map { case (v, reg) => (v, machine.dataTypeReg(reg)) }.to(Vec))
        substs += Subst(substExprs, substNats, substTypes, substDataTypes)
      })

      substs
    }
  }

  object Program {
    def compileFromPattern(pat: Pattern): Program = {
      Compiler.compile(pat)
    }
  }

  // A node without children for matching purposes
  type MNode = Node[(), (), ()]

  def forEachMatchingNode[D](eclass: EClass[D], node: MNode, f: ENode => Unit): Unit = {
    import scala.math.Ordering.Implicits._
    import Node.{ordering, eclassIdOrdering}

    if (eclass.nodes.size < 50) {
      eclass.nodes.filter(n => node.matches(n)).foreach(f)
    } else {
      assert(eclass.nodes.sliding(2).forall(w => w(0) < w(1)))
      // binary search
      eclass.nodes.view.map(_.map(_ => (), _ => (), _ => ())).search(node) match {
        case scala.collection.Searching.Found(found) =>
          def findStart(pos: Int): Int =
            if ((pos > 0) && eclass.nodes(pos - 1).matches(node)) {
              findStart(pos - 1)
            } else {
              pos
            }
          def findEnd(pos: Int): Int =
            if ((pos + 1 < eclass.nodes.size) && eclass.nodes(pos + 1).matches(node)) {
              findEnd(pos + 1)
            } else {
              pos
            }
          val start = findStart(found)
          val end = findEnd(found)
          val matching = eclass.nodes.iterator.slice(start, end + 1)
          assert(matching.size == eclass.nodes.count(n => node.matches(n)))
          matching.foreach(f)
        case scala.collection.Searching.InsertionPoint(_) => ()
      }
    }
  }

  case class Todo(reg: Reg, pat: Pattern)

  object Todo {
    implicit val todoOrd: math.Ordering[Todo] = new Ordering[Todo] {
      override def compare(x: Todo, y: Todo): Int = {
        (x.pat.node, y.pat.node) match {
          // fewer children has higher priority
          case (Left(e1), Left(e2)) =>
            e2.childrenCount() compare e1.childrenCount()
          // var has higher priority than node
          case (Left(_), Right(_)) => -1
          case (Right(_), Left(_)) => 1
          case (Right(_), Right(_)) => 0
        }
      }
    }
  }

  object Compiler {
    def compile(pattern: Pattern): Program = {
      val compiler = new Compiler(
        pattern, HashMap.empty, HashMap.empty, mutable.PriorityQueue.empty, Reg(1), NatReg(0))
      compiler.todo.addOne(Todo(Reg(0), pattern))
      compiler.go()
    }
  }

  class Compiler(var pattern: Pattern,
                 var v2r: HashMap[PatternVar, Reg],
                 var ni2r: HashMap[NatPatternVar, NatReg],
                 var todo: mutable.PriorityQueue[Todo],
                 var out: Reg,
                 var natOut: NatReg) {
    def go(): Program = {
      val instructions = Vec.empty[Instruction]
      while (todo.nonEmpty) {
        val Todo(i, pat) = todo.dequeue()
        pat.node match {
          case Right(v) => v2r.get(v) match {
            case Some(j) => instructions += Compare(i, j)
            case None => v2r += v -> i
          }
          case Left(node) =>
            val currentOut = Reg(out.n)
            val currentNatOut = NatReg(natOut.n)
            out.n += node.childrenCount()
            natOut.n += node.natsCount()

            instructions += Bind(node.map(_ => (), _ => (), _ => ()), i, currentOut, currentNatOut)

            var id = 0
            node.children().foreach { child =>
              val r = Reg(currentOut.n + id)
              todo.addOne(Todo(r, child))
              id += 1
            }

            var nid = 0
            node.nats().foreach { child =>
              val r = NatReg(currentNatOut.n + nid)
              // TODO: could add to the `todo` priority queue first
              child match {
                /*case ni: NatIdentifier if ni.name.startsWith("?") => {
                  ni2r.get(ni) match {
                    case Some(j) => instructions += NatCompare(r, j)
                    case None => ni2r += ni -> r
                  }
                }*/
                case n: Nat => NatCheck(r, n)
                case _ => ???
              }
              nid += 1
            }
        }
      }

      new Program(instructions, v2r, ni2r, ???, ???)
    }
  }
}
