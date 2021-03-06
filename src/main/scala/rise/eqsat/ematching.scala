package rise.eqsat

import scala.collection.mutable

/** e-matching tries to find a [[Pattern]] in an [[EGraph]],
  * returning a list of [[Subst]]s representing successful matches.
  * @see [[http://leodemoura.github.io/files/ematching.pdf Efficient e-matching for SMT Solvers]]
  */
object ematching {
  object AbstractMachine {
    def init(eclass: EClassId): AbstractMachine =
      AbstractMachine(Vec(eclass), Vec(), Vec())
  }

  /** An abstract machine on which compiled [[Pattern]]s ([[Program]]s) are executed
    * @todo we could use heterogeneous or generic registers to avoid code duplication
    */
  case class AbstractMachine(regs: Vec[EClassId],
                             nRegs: Vec[Nat],
                             tRegs: Vec[Type]) {
    def reg(r: Reg): EClassId = regs(r.n)
    def nReg(r: NatReg): Nat = nRegs(r.n)
    def tReg(r: TypeReg): Type = tRegs(r.n)

    def run[D](egraph: EGraph[D],
               instructions: Seq[Instruction],
               yieldFn: () => Unit): Unit = {
      var instrs = instructions
      while (instrs.nonEmpty) {
        instrs.head match {
          case PushType(i) =>
            tRegs += egraph.get(reg(i)).t
          case Bind(node, i, out, nOut, tOut) =>
            forEachMatchingNode(egraph.get(reg(i)), node, { matched =>
              regs.remove(out.n, regs.size - out.n)
              nRegs.remove(nOut.n, nRegs.size - nOut.n)
              tRegs.remove(tOut.n, tRegs.size - tOut.n)
              matched.map(
                id => regs += id,
                n => nRegs += n,
                dt => tRegs += Type(dt.node)
              )
              run(egraph, instrs.tail, yieldFn)
            })
            return
          case Compare(i, j) =>
            if (egraph.find(reg(i)) != egraph.find(reg(j))) {
              return
            }
          case NatBind(node, i) =>
            val matched = nReg(i)
            if (matched.node.map(_ => ()) == node) {
              matched.node.map(n => nRegs += n)
            } else {
              return
            }
          case NatCompare(i, j) =>
            if (nReg(i) != nReg(j)) {
              return
            }
          case TypeBind(node, i) =>
            val matched = tReg(i)
            if (matched.node.map(_ => (), _ => (), _ => ()) == node) {
              matched.node.map(
                t => tRegs += t,
                n => nRegs += n,
                dt => tRegs += Type(dt.node)
              )
            } else {
              return
            }
          case TypeCompare(i, j) =>
            if (tReg(i) != tReg(j)) {
              return
            }
          case DataTypeCheck(i) =>
            if (!tReg(i).node.isInstanceOf[DataTypeNode[_, _]]) {
              return
            }
        }
        instrs = instrs.tail
      }

      yieldFn()
    }
  }

  sealed trait Instruction
  case class PushType(i: Reg) extends Instruction
  // try all matches of `node` in `i`
  case class Bind(node: MNode, i: Reg,
                  out: Reg, nOut: NatReg, tOut: TypeReg) extends Instruction
  // handle repeated variable occurrences
  case class Compare(i: Reg, j: Reg) extends Instruction
  case class NatBind(n: MNatNode, i: NatReg) extends Instruction
  case class NatCompare(i: NatReg, j: NatReg) extends Instruction
  case class TypeBind(t: MTypeNode, i: TypeReg) extends Instruction
  case class TypeCompare(i: TypeReg, j: TypeReg) extends Instruction
  case class DataTypeCheck(i: TypeReg) extends Instruction

  case class Reg(n: Int)
  case class NatReg(n: Int)
  case class TypeReg(n: Int)

  /** A program compiled from a [[Pattern]] */
  class Program(val instructions: Vec[Instruction],
                var v2r: HashMap[PatternVar, Reg], // TODO? HashMap[_, _] -> compact Vec[_]
                var n2r: HashMap[NatPatternVar, NatReg],
                var t2r: HashMap[TypePatternVar, TypeReg],
                var dt2r: HashMap[DataTypePatternVar, TypeReg]) {
    def run[D](egraph: EGraph[D], eclass: EClassId): Vec[Subst] = {
      val machine = AbstractMachine.init(eclass)

      val substs = Vec.empty[Subst]
      machine.run(egraph, instructions.toSeq, { () =>
        val substExprs = VecMap(v2r.iterator.map { case (v, reg) => (v, machine.reg(reg)) }.to(Vec))
        val substNats = VecMap(n2r.iterator.map { case (v, reg) => (v, machine.nReg(reg)) }.to(Vec))
        val substTypes = VecMap(t2r.iterator.map { case (v, reg) => (v, machine.tReg(reg)) }.to(Vec))
        val substDataTypes = VecMap(dt2r.iterator.map { case (v, reg) =>
          (v, DataType(machine.tReg(reg).node.asInstanceOf[DataTypeNode[Nat, DataType]]))
        }.to(Vec))
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

  /** A node without children for matching purposes */
  type MNode = Node[(), (), ()]
  type MNatNode = NatNode[()]
  type MTypeNode = TypeNode[(), (), ()]

  def forEachMatchingNode[D](eclass: EClass[D], node: MNode, f: ENode => Unit): Unit = {
    import scala.math.Ordering.Implicits._
    import Node.{ordering, eclassIdOrdering, natOrdering, dataTypeOrdering}

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

  sealed trait Todo
  case class TodoExpr(reg: Reg, pat: Pattern) extends Todo
  case class TodoNat(reg: NatReg, pat: NatPattern) extends Todo
  case class TodoType(reg: TypeReg, pat: TypePattern) extends Todo

  // TODO: this ordering has not been thought-through for the addition of types yet
  object Todo {
    implicit val todoExprOrd: math.Ordering[TodoExpr] = new Ordering[TodoExpr] {
      override def compare(x: TodoExpr, y: TodoExpr): Int = {
        (x.pat.p, y.pat.p) match {
          // fewer children has higher priority
          case (PatternNode(e1), PatternNode(e2)) =>
            e2.childrenCount() compare e1.childrenCount()
          // var has higher priority than node
          case (PatternNode(_), PatternVar(_)) => -1
          case (PatternVar(_), PatternNode(_)) => 1
          case (PatternVar(_), PatternVar(_)) => 0
        }
      }
    }

    implicit val todoNatOrd: math.Ordering[TodoNat] = new Ordering[TodoNat] {
      override def compare(x: TodoNat, y: TodoNat): Int = {
        (x.pat, y.pat) match {
          // fewer children has higher priority
          case (NatPatternNode(n1), NatPatternNode(n2)) =>
            n1.natCount() compare n2.natCount()
          // var has higher priority than node
          case (NatPatternNode(_), NatPatternVar(_)) => -1
          case (NatPatternVar(_), NatPatternNode(_)) => 1
          case (NatPatternVar(_), NatPatternVar(_)) => 0
          // any has lower priority
          case (NatPatternAny, _) => -1
          case (_, NatPatternAny) => 1
        }
      }
    }

    implicit val todoTypeOrd: math.Ordering[TodoType] = new Ordering[TodoType] {
      override def compare(x: TodoType, y: TodoType): Int = {
        (x.pat, y.pat) match {
          // fewer children has higher priority
          case (TypePatternNode(n1), TypePatternNode(n2)) =>
            n1.childrenCount() compare n2.childrenCount()
          case (TypePatternNode(_), _) => -1
          case (_, TypePatternNode(_)) => 1
          // var has higher priority than node
          case (TypePatternVar(_), TypePatternVar(_)) => 0
          case (TypePatternVar(_), _) => -1
          case (_, TypePatternVar(_)) => 1
          // any has lower priority
          case (TypePatternAny, TypePatternAny) => 0
          case (TypePatternAny, _) => -1
          case (_, TypePatternAny) => 1

          // fewer children has higher priority
          case (DataTypePatternNode(n1), DataTypePatternNode(n2)) =>
            n1.childrenCount() compare n2.childrenCount()
          // var has higher priority than node
          case (DataTypePatternNode(_), DataTypePatternVar(_)) => -1
          case (DataTypePatternVar(_), DataTypePatternNode(_)) => 1
          case (DataTypePatternVar(_), DataTypePatternVar(_)) => 0
          // any has lower priority
          case (DataTypePatternAny, _) => -1
          case (_, DataTypePatternAny) => 1
        }
      }
    }

    implicit val todoOrd: math.Ordering[Todo] = new Ordering[Todo] {
      override def compare(x: Todo, y: Todo): Int = {
        (x, y) match {
          case (t1: TodoExpr, t2: TodoExpr) => todoExprOrd.compare(t1, t2)
          case (_: TodoExpr, _) => 1
          case (_, _: TodoExpr) => -1
          case (t1: TodoNat, t2: TodoNat) => todoNatOrd.compare(t1, t2)
          case (_: TodoNat, _) => 1
          case (_, _: TodoNat) => -1
          case (t1: TodoType, t2: TodoType) => todoTypeOrd.compare(t1, t2)
        }
      }
    }
  }

  object Compiler {
    def compile(pattern: Pattern): Program = {
      val compiler = new Compiler(pattern,
        HashMap.empty, HashMap.empty, HashMap.empty, HashMap.empty,
        mutable.PriorityQueue(TodoExpr(Reg(0), pattern)),
        Reg(1), NatReg(0), TypeReg(0))
      compiler.go()
    }
  }

  /** A compiler for [[Pattern]]s */
  class Compiler(var pattern: Pattern,
                 var v2r: HashMap[PatternVar, Reg],
                 var n2r: HashMap[NatPatternVar, NatReg],
                 var t2r: HashMap[TypePatternVar, TypeReg],
                 var dt2r: HashMap[DataTypePatternVar, TypeReg],
                 var todo: mutable.PriorityQueue[Todo],
                 var out: Reg,
                 var nOut: NatReg,
                 var tOut: TypeReg) {
    def pushTodo(p: Pattern): Unit = {
      todo.addOne(TodoExpr(out, p))
      out = Reg(out.n + 1)
    }
    def pushTodo(p: NatPattern): Unit = {
      todo.addOne(TodoNat(nOut, p))
      nOut = NatReg(nOut.n + 1)
    }
    def pushTodo(p: TypePattern): Unit = {
      todo.addOne(TodoType(tOut, p))
      tOut = TypeReg(tOut.n + 1)
    }

    def go(): Program = {
      val instructions = Vec.empty[Instruction]
      while (todo.nonEmpty) {
        todo.dequeue() match {
          case TodoExpr(i, pat) =>
            pushTodo(pat.t)
            instructions += PushType(i)

            pat.p match {
              case v: PatternVar => v2r.get(v) match {
                case Some(j) => instructions += Compare(i, j)
                case None => v2r += v -> i
              }
              case PatternNode(node) =>
                val currentOut = Reg(out.n)
                val currentNOut = NatReg(nOut.n)
                val currentTOut = TypeReg(tOut.n)

                val mNode = node.map(pushTodo, pushTodo, pushTodo)
                instructions += Bind(mNode, i, currentOut, currentNOut, currentTOut)
            }
          case TodoNat(i, pat) =>
            pat match {
              case v: NatPatternVar => n2r.get(v) match {
                case Some(j) => instructions += NatCompare(i, j)
                case None => n2r += v -> i
              }
              case NatPatternAny =>
              case NatPatternNode(node) =>
                val mNode = node.map(pushTodo)
                instructions += NatBind(mNode, i)
          }
          case TodoType(i, pat) =>
            pat match {
              case v: TypePatternVar => t2r.get(v) match {
                case Some(j) => instructions += TypeCompare(i, j)
                case None => t2r += v -> i
              }
              case v: DataTypePatternVar => dt2r.get(v) match {
                case Some(j) => instructions += TypeCompare(i, j)
                case None =>
                  dt2r += v -> i
                  // TODO: can be avoided in some cases
                  instructions += DataTypeCheck(i)
              }
              case TypePatternAny =>
              case DataTypePatternAny =>
                // TODO: can be avoided in some cases
                instructions += DataTypeCheck(i)
              case TypePatternNode(node) => handleTypePattern(node)
              case DataTypePatternNode(node) => handleTypePattern(node)
            }

            def handleTypePattern(node: TypeNode[TypePattern, NatPattern, DataTypePattern]): Unit = {
              val mNode = node.map(pushTodo, pushTodo, pushTodo)
              instructions += TypeBind(mNode, i)
            }
        }
      }

      new Program(instructions, v2r, n2r, t2r, dt2r)
    }
  }
}
