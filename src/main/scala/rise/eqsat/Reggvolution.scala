package rise.eqsat

/* 
 This package contains features to translate Rise expressions and rewrites
 to an egg-compatible language living in Rust.

 see https://github.com/Bastacyclop/reggvolution

 */
object Reggvolution {
  def sym(s: String): String = s
    // s"""sym("$s")"""

  // cascade of apps bearing no types, used to encode many language constructs
  // as simple symbol applications
  def noTyApp(f: String, args: Iterable[String]): String = {
    args.foldLeft(f) { case (acc, arg) =>
      s"(app $acc $arg)"
      // s"App([$acc, $arg])"
    }
  }

  type Shift = rise.eqsat.Expr.Shift
  
  def reggvolve(expr: Expr): String =
    Expr.reggvolve(expr, (0, 0, 0, 0, 0))

  object Expr {
    def reggvolve(expr: Expr, s: Shift): String = {
      val e = expr.node match {
        case Var(index) => s"%${index + s._1}"
          // s"Var(${index + s._1})"
        case App(f, e) => s"(app ${reggvolve(f, s)} ${reggvolve(e, s)})"
          // s"App([${reggvolve(f, s)}, ${reggvolve(e, s)}])"
        case NatApp(f, x) => s"(app ${reggvolve(f, s)} ${Nat.reggvolve(x, s)})"
        case DataApp(f, x) => s"(app ${reggvolve(f, s)} ${DataType.reggvolve(x, s)})"
        case AddrApp(f, x) => s"(app ${reggvolve(f, s)} ${Addr.reggvolve(x, s)})"
        case AppNatToNat(f, x) => s"(app ${reggvolve(f, s)} ${NatToNat.reggvolve(x, s)})"
        case Lambda(e) =>
          val s2 = (s._1, s._2 + 1, s._3 + 1, s._4 + 1, s._5 + 1)
          s"(lam ${reggvolve(e, s2)})"
        case NatLambda(e) =>
          val s2 = (s._1 + 1, s._2, s._3 + 1, s._4 + 1, s._5 + 1)
          s"(lam ${reggvolve(e, s2)})"
        case DataLambda(e) =>
          val s2 = (s._1 + 1, s._2 + 1, s._3, s._4 + 1, s._5 + 1)
          s"(lam ${reggvolve(e, s2)})"
        case AddrLambda(e) =>
          val s2 = (s._1 + 1, s._2 + 1, s._3 + 1, s._4, s._5 + 1)
          s"(lam ${reggvolve(e, s2)})"
        case LambdaNatToNat(e) =>
          val s2 = (s._1 + 1, s._2 + 1, s._3 + 1, s._4 + 1, s._5)
          s"(lam ${reggvolve(e, s)})"
        case Literal(d) =>
          import rise.core.semantics._

          d match {
            case BoolData(true) => sym("true")
            case BoolData(false) => sym("false")
            case IntData(i) => i.toString() // s"Integer($i)"
            case FloatData(f) => f.toString() // s"Float($f)"
            case DoubleData(d) => d.toString() // s"Double($d)"
            case _=> throw new Exception(s"not supporting literal $d yet")
          }
        case NatLiteral(n) => Nat.reggvolve(n, s)
        case IndexLiteral(i, n) => noTyApp(sym("idxL"), List(i, n).map(Nat.reggvolve(_, s)))
        case Primitive(p) => sym(p.name)
        case Composition(f, g) => ???
      }
      val t = Type.reggvolve(expr.t, s)
      // s"TypeOf([$e, $t])"
      s"(typeOf $e $t)"
    }
  }

  object Type {
    def reggvolve(ty: Type, s: Shift): String = {
      ty.node match {
        case dt: DataTypeNode[Nat, DataType] =>
          DataType.reggvolve(rise.eqsat.DataType(dt), s)
        case FunType(a, b) => noTyApp(sym("fun"), List(a, b).map(reggvolve(_, s)))
        // TODO: do we need to remember the arg kind as a type ?
        case NatFunType(t) =>
          val s2 = (s._1 + 1, s._2, s._3 + 1, s._4 + 1, s._5 + 1)
          s"(lam ${reggvolve(t, s2)})"
        case DataFunType(t) =>
          val s2 = (s._1 + 1, s._2 + 1, s._3, s._4 + 1, s._5 + 1)
          s"(lam ${reggvolve(t, s2)})"
        case AddrFunType(t) =>
          val s2 = (s._1 + 1, s._2 + 1, s._3 + 1, s._4, s._5 + 1)
          s"(lam ${reggvolve(t, s2)})"
        case NatToNatFunType(t) =>
          val s2 = (s._1 + 1, s._2 + 1, s._3 + 1, s._4 + 1, s._5)
          s"(lam ${reggvolve(t, s2)})"
      }
    }
  }

  object Nat {
    def reggvolve(n: Nat, s: Shift): String = {
      n.node match {
        case NatVar(index) => s"%${index + s._2}"
        case NatCst(value) => value.toString()
        case NatNegInf => ???
        case NatPosInf => ???
        case NatAdd(a, b) => noTyApp(sym("add"), List(a, b).map(reggvolve(_, s)))
        case NatMul(a, b) => noTyApp(sym("mul"), List(a, b).map(reggvolve(_, s)))
        case NatPow(a, b) => noTyApp(sym("pow"), List(a, b).map(reggvolve(_, s)))
        case NatMod(a, b) => noTyApp(sym("mod"), List(a, b).map(reggvolve(_, s)))
        case NatIntDiv(a, b) => noTyApp(sym("floorDiv"), List(a, b).map(reggvolve(_, s)))
        case NatToNatApp(f, n) => ???
      }
    }
  }

  object DataType {
    def reggvolve(dty: DataType, s: Shift): String = {
      dty.node match {
        case DataTypeVar(index) => s"%${index + s._3}"
        case ScalarType(s) => sym(s.toString())
        case NatType => sym("natT")
        case IndexType(n) => noTyApp(sym("idxT"), List(Nat.reggvolve(n, s)))
        case PairType(dt1, dt2) => noTyApp(sym("pairT"), List(dt1, dt2).map(reggvolve(_, s)))
        case ArrayType(n, et) => noTyApp(sym("arrT"), List(Nat.reggvolve(n, s), reggvolve(et, s)))
        case VectorType(n, et) => noTyApp(sym("vecT"), List(Nat.reggvolve(n, s), reggvolve(et, s)))
      }
    }
  }

  object Addr {
    def reggvolve(a: Address, s: Shift): String = {
      a match {
        case AddressVar(index) => s"%${index + s._4}"
        case Global => sym("global")
        case Local => sym("local")
        case Private => sym("private")
        case Constant => sym("constant")
      }
    }
  }

  object NatToNat {
    def reggvolve(n: NatToNat, s: Shift): String = {
      ???
    }
  }
}
