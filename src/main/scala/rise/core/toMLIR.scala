package rise.core

import arithexpr.arithmetic._
import rise.core.types._
import rise.core.DSL.Type._
import rise.core.primitives._

import scala.annotation.tailrec

object toMLIR {

  def toCppBuilderAPI(exp :Expr): String = {
    @tailrec
    def extractBody(e: Expr, env: Map[Identifier, String], i: Int): (Expr, Map[Identifier, String]) = e match {
      case Lambda(x, body) => extractBody(body, env.updated(x, createInOp(x, i+1)), i+1)
      case _ => (e, env)
    }

    val (body, env) = extractBody(exp, Map(), 0)
    s"""[](OpBuilder& b, Location loc, FuncOp funcOp) {
       |  return ${toCppBuilderAPI(body, env)};
       |}""".stripMargin
  }

  private def createInOp(i: Identifier, n: Int): String = {
    s"""[&] {  // InOp
       |  auto type = ${fromType(i.t)};
       |  return b.create<InOp>(loc, TypeRange{type}, funcOp.getArgument($n));
       |}()""".stripMargin
  }

  private def toCppBuilderAPI(exp :Expr, env: Map[Identifier, String]): String = exp match {
    case id: Identifier => env(id)
    case l@Lambda(x, e) =>
      val params = collectParams(x, e)
      val body = findBody(e)
      val updatedEnv = params.zipWithIndex.foldLeft(env) {
        case (env, (p, i)) => env.updated(p, s"args[$i]")
      }
      s"""[&] {  // LambdaOp
         |  auto type = ${fromType(l.t)};
         |  auto fun = [&](OpBuilder& b, Location loc, MutableArrayRef<BlockArgument> args) {
         |    OpBuilder::InsertionGuard guard(b);
         |    return ${toCppBuilderAPI(body, updatedEnv)};
         |  };
         |  return b.create<LambdaOp>(loc, type, fun);
         |}()""".stripMargin
    case a@App(fun, arg) =>
      val args = collectArgs(fun, arg)
      val argsDecls = args.zipWithIndex.map {
        case (arg, i) => s"auto arg$i = ${toCppBuilderAPI(arg, env)}"
      }.mkString(";\n  ")
      val argsRefs = args.indices.map(i => s"arg$i").mkString(", ")

      val f = findFunction(fun)
      f match {
        case add() | mul() =>
          val op = f match {
            case add() => "AddFOp"
            case mul() => "MulFOp"
          }
          s"""[&] {  // EmbedOp
             |  auto type = ${fromType(a.t)};
             |  $argsDecls;
             |  auto fun = [&](OpBuilder& b, Location loc, MutableArrayRef<BlockArgument> args) {
             |    OpBuilder::InsertionGuard guard(b);
             |    return b.create<$op>(loc, args[0], args[1]);
             |  };
             |  return b.create<EmbedOp>(loc, type, ValueRange{ $argsRefs }, fun);
             |}()""".stripMargin
        case f =>
          s"""[&] {  // ApplyOp
             |  auto type = ${fromType(a.t)};
             |  $argsDecls;
             |  auto fun = ${toCppBuilderAPI(f, env)};
             |  return b.create<ApplyOp>(loc, type, fun, ValueRange{ $argsRefs });
             |}()""".stripMargin
      }
    case l@Literal(d) =>
      s"""[&] {  // LiteralOp
         |  auto type = ${fromType(l.t)};
         |  return b.create<LiteralOp>(loc, type, LiteralAttr::get(b.getContext(), type, "${d.toString}"));
         |}()""".stripMargin
    case primitive: Primitive => primitiveToCppBuilderAPI(primitive)
    case DepLambda(kind, x, e) => ???
    case DepApp(kind, f, x) => ???
    case Opaque(e, t) => ???
    case TypeAnnotation(e, annotation) => ???
    case TypeAssertion(e, assertion) => ???
  }

  private def collectArgs(expr: Expr, arg: Expr): Seq[Expr] = collectArgs(expr, Seq(arg))

  @tailrec
  private def collectArgs(expr: Expr, args: Seq[Expr]): Seq[Expr] = expr match {
    case App(f, arg) => collectArgs(f, arg +: args)
    case _ => args
  }

  private def collectParams(param: Identifier, expr: Expr): Seq[Identifier] = collectParams(expr, Seq(param))

  @tailrec
  private def collectParams(expr: Expr, params: Seq[Identifier]): Seq[Identifier] = expr match {
    case Lambda(p, e) => collectParams(e, params :+ p)
    case _ => params
  }

  @tailrec
  private def findFunction(expr: Expr): Expr = expr match {
    case App(f, _) => findFunction(f)
    case _ => expr
  }

  @tailrec
  private def findBody(expr: Expr): Expr = expr match {
    case Lambda(_, e) => findBody(e)
    case _ => expr
  }

  private def fromType(ty: Type): String = ty match {
    case dt: DataType => fromDataType(dt)
    case FunType(inT, outT) =>
      s"rise::FunType::get(b.getContext(), ${fromType(inT)}, ${fromType(outT)})"
    case TypePlaceholder => ???
    case TypeIdentifier(name) => ???
    case DepFunType(kind, x, t) => ???
  }

  private def fromDataType(dt: DataType): String = dt match {
    case scalarType: ScalarType => scalarType match {
      case `bool` =>  ???
      case `int` => s"rise::ScalarType::get(b.getContext(), IntegerType::get(b.getContext()))"
      case `i8` => ???
      case `i16` => ???
      case `i32` => ???
      case `i64` => ???
      case `u8` => ???
      case `u16` => ???
      case `u32` => ???
      case `u64` => ???
      case `f16` => s"rise::ScalarType::get(b.getContext(), Float16Type::get(b.getContext()))"
      case `f32` => s"rise::ScalarType::get(b.getContext(), Float32Type::get(b.getContext()))"
      case `f64` => s"rise::ScalarType::get(b.getContext(), Float64Type::get(b.getContext()))"
    }
    case NatType => s"rise::ScalarType::get(b.getContext(), IntegerType::get(b.getContext()))"
    case PairType(dt1, dt2) =>
      s"rise::Tuple::get(b.getContext(), ${fromDataType(dt1)}, ${fromDataType(dt2)})"
    case ArrayType(size, elemType) =>
      s"rise::ArrayType::get(b.getContext(), ${fromNat(size)}, ${fromDataType(elemType)})"
    case DataTypeIdentifier(name) => ???
    case OpaqueType(name) => ???
    case VectorType(size, elemType) => ???
    case IndexType(size) => ???
    case FragmentType(rows, columns, d3, dataType, fragmentKind, layout) => ???
    case ManagedBufferType(dt) => ???
    case DepPairType(kind, x, t) => ???
    case apply: NatToDataApply => ???
    case DepArrayType(size, fdt) => ???
  }

  private def fromNat(nat: Nat): String = nat match {
    case Cst(c) =>
      s"rise::Nat::get(b.getContext(), ${c.toString})"
    case _ => ???
  }

  private def primitiveToCppBuilderAPI(p: Primitive): String = (p, p.t) match {
    case (zip(), (n`.`dt1) ->: (_`.`dt2) ->: _) =>
      s"""[&] {  // ZipOp
         |  auto type = ${fromType(p.t)};
         |  auto n = rise::NatAttr::get(b.getContext(), ${fromNat(n)});
         |  auto dt1 = rise::DataTypeAttr::get(b.getContext(), ${fromType(dt1)});
         |  auto dt2 = rise::DataTypeAttr::get(b.getContext(), ${fromType(dt2)});
         |  return b.create<ZipOp>(loc, type, n, dt1, dt2);
         |}()""".stripMargin
    case (map(), (dt1 ->: dt2) ->: (n`.`_) ->: _) =>
      s"""[&] {  // MapOp
         |  auto type = ${fromType(p.t)};
         |  auto n = rise::NatAttr::get(b.getContext(), ${fromNat(n)});
         |  auto dt1 = rise::DataTypeAttr::get(b.getContext(), ${fromType(dt1)});
         |  auto dt2 = rise::DataTypeAttr::get(b.getContext(), ${fromType(dt2)});
         |  return b.create<MapOp>(loc, type, n, dt1, dt2);
         |}()""".stripMargin
    case (reduceSeq(), (dt1 ->: dt2 ->: _) ->: _ ->: (n`.`_) ->: _) =>
      s"""[&] {  // ReduceSeqOp
         |  auto type = ${fromType(p.t)};
         |  auto n = rise::NatAttr::get(b.getContext(), ${fromNat(n)});
         |  auto dt1 = rise::DataTypeAttr::get(b.getContext(), ${fromType(dt1)});
         |  auto dt2 = rise::DataTypeAttr::get(b.getContext(), ${fromType(dt2)});
         |  auto lowerTo = StringAttr::get(b.getContext(), "scf");
         |  return b.create<ReduceSeqOp>(loc, type, n, dt1, dt2, lowerTo);
         |}()""".stripMargin
    case (fst(), (dt1 x dt2) ->: _) =>
      s"""[&] {  // FstOp
         |  auto type = ${fromType(p.t)};
         |  auto dt1 = rise::DataTypeAttr::get(b.getContext(), ${fromType(dt1)});
         |  auto dt2 = rise::DataTypeAttr::get(b.getContext(), ${fromType(dt2)});
         |  return b.create<FstOp>(loc, type, dt1, dt2);
         |}()""".stripMargin
    case (snd(), (dt1 x dt2) ->: _) =>
      s"""[&] {  // SndOp
         |  auto type = ${fromType(p.t)};
         |  auto dt1 = rise::DataTypeAttr::get(b.getContext(), ${fromType(dt1)});
         |  auto dt2 = rise::DataTypeAttr::get(b.getContext(), ${fromType(dt2)});
         |  return b.create<SndOp>(loc, type, dt1, dt2);
         |}()""".stripMargin
    case (transpose(), (n`.`(m`.`dt)) ->: _) =>
      s"""[&] {  // TransposeOp
         |  auto type = ${fromType(p.t)};
         |  auto n = rise::NatAttr::get(b.getContext(), ${fromNat(n)});
         |  auto m = rise::NatAttr::get(b.getContext(), ${fromNat(m)});
         |  auto dt = rise::DataTypeAttr::get(b.getContext(), ${fromType(dt)});
         |  return b.create<TransposeOp>(loc, type, n, m, dt);
         |}()""".stripMargin
  }

}
