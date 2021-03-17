package rise.eqsat

import scala.collection.mutable.PriorityQueue

object ematching {
  object AbstractMachine {
    def init(eclass: EClassId): AbstractMachine =
      AbstractMachine(Vec(eclass))
  }

  case class AbstractMachine(regs: Vec[EClassId]) {
    def reg(r: Reg): EClassId = regs(r.n)

    def run[D](egraph: EGraph[D],
               instructions: Seq[Instruction],
               subst: Subst,
               yieldFn: (AbstractMachine, Subst) => Unit): Unit = {
      var instrs = instructions
      while (instrs.nonEmpty) {
        instrs.head match {
          case Bind(node, i, out) =>
            return forEachMatchingNode(egraph(reg(i)), node, { matched =>
              regs.remove(out.n, regs.size - out.n)
              matched.children().foreach(id => regs += id)
              run(egraph, instrs.tail, subst, yieldFn)
            })
          case Compare(i, j) =>
            if (egraph.find(reg(i)) != egraph.find(reg(j))) {
              return
            }
        }
        instrs = instrs.tail
      }

      yieldFn(this, subst)
    }
  }

  sealed trait Instruction
  case class Bind(node: MNode, i: Reg, out: Reg) extends Instruction
  case class Compare(i: Reg, j: Reg) extends Instruction

  case class Reg(var n: Int)

  class Program(val instructions: Vec[Instruction],
                val subst: Subst) {
    def run[D](egraph: EGraph[D], eclass: EClassId): Vec[Subst] = {
      val machine = AbstractMachine.init(eclass)

      val substs = Vec.empty[Subst]
      machine.run(egraph, instructions.toSeq, subst, { case (machine, subst) =>
        // (egg) HACK: we are reusing Ids here, this is bad
        val substVec = subst.vec.map { case (v, regId) => (v, machine.reg(Reg(regId.i))) }
        substs += Subst(substVec)
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
  type MNode = Node[()]
  /* TODO
  object MNodeOrdering extends math.Ordering[MNode] {
    override def compare(x: MNode, y: MNode): Int = {
      (x, y) match {
        case (Var(i), Var(j)) => i compare j
        case (Var(_), _) => -1
        case (_, Var(_)) => 1
        case (App(_, _), App(_, _)) => 0
        case (App(_, _), _) => -1
        case (_, App(_, _)) => 1
        case (Lambda(_), Lambda(_)) => 0
        case (Lambda(_), _) => -1
        case (_, Lambda(_)) => 1
        case (DepApp(_, x1), DepApp(_, x2)) => x1 compare x2
        case (DepApp(_, _), _) => -1
        case (_, DepApp(_, _)) => 1
        case (Literal(d1), Literal(d2)) => d1 compare d2
        case (Literal(_), _) => -1
        case (_, Literal(_)) => 1
        case (Primitive(p1), Primitive(p2)) => p1 compare p2
      }
    }
  } */

  def forEachMatchingNode[D](eclass: EClass[D], node: MNode, f: ENode => Unit): Unit = {
    // if (eclass.nodes.size < 50) {
      eclass.nodes.filter(n => node.matches(n)).foreach(f)
    /* } else { TODO
      assert(eclass.nodes.sliding(2).forall(w => (w(0) < w(1)): Boolean))
      val start = eclass.nodes.search(node)()
    } */
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
        pattern, HashMap.empty, PriorityQueue.empty, Reg(1))
      compiler.todo.addOne(Todo(Reg(0), pattern))
      compiler.go()
    }
  }

  class Compiler(var pattern: Pattern,
                 var v2r: HashMap[PatternVar, Reg],
                 var todo: PriorityQueue[Todo],
                 var out: Reg) {
    def go(): Program = {
      val instructions = Vec.empty[Instruction]
      while (todo.nonEmpty) {
        val Todo(i, pat) = todo.dequeue()
        pat.node match {
          case Right(v) => v2r.get(v) match {
            case Some(j) => instructions += Compare(i, j)
            case None => v2r(v) = i
          }
          case Left(node) =>
            val currentOut = Reg(out.n)
            out.n += node.childrenCount()

            var id = 0
            node.children().foreach { child =>
              val r = Reg(currentOut.n + id)
              todo.addOne(Todo(r, child))
              id += 1
            }

            instructions += Bind(node.mapChildren(_ => ()), i, currentOut)
        }
      }

      val subst = Subst.empty
      for ((v, r) <- v2r) {
        subst.insert(v, EClassId(r.n))
      }
      new Program(instructions, subst)
    }
  }
}
