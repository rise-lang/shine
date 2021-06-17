package rise.elevate.strategies

import elevate.core.strategies.Traversable
import elevate.core.{Failure, Strategy, Success}
import elevate.core.strategies.traversal.tryAll
import rise.elevate.Rise
import rise.core.DSL._
import rise.core._
import rise.core.primitives.{let, toMem}

object lowering {

  def storeInMemory(what: Strategy[Rise], how: Strategy[Rise])
                   (implicit ev: Traversable[Rise]): Strategy[Rise] = { p =>
    extract(what)(p) >>= (extracted => {
      how(extracted) >>= (storedSubExpr => {
        val idx = Identifier(freshName("x"))(extracted.t)

        replaceAll(what, idx)(ev)(p) match {
          case Success(replaced) =>
            Success(let(toMem(storedSubExpr))(lambda(ToBeTyped(idx), replaced)) !: p.t)
          case Failure(_) => Failure(storeInMemory(what, how))
        }
      })
    })
  }


  def replaceAll(exprPredicate: Strategy[Rise], withExpr: Rise)
                (implicit ev: Traversable[Rise]): Strategy[Rise] =
    p => tryAll(exprPredicate `;` insert(withExpr)).apply(p)

  def insert(expr: Rise): Strategy[Rise] = _ => Success(expr)
  def extract(what: Strategy[Rise]): Strategy[Rise] = (expr: Rise) => {
    what(expr).flatMapFailure(_ => expr match {
      case App(f,e)           => extract(what)(f).flatMapFailure(_ => extract(what)(e))
      case Lambda(x, e)       => extract(what)(x).flatMapFailure(_ => extract(what)(e))
      case DepLambda(_, _, e) => extract(what)(e)
      case DepApp(_, _, _)    => Failure(extract(what))
      case _: Identifier      => Failure(extract(what))
      case _: Literal         => Failure(extract(what))
      case _: TypeAnnotation  => throw new Exception("Type annotations should be gone.")
      case _: TypeAssertion   => throw new Exception("Type assertions should be gone.")
      case _: Opaque          => throw new Exception("Opaque expressions should be gone.")
      case _: Primitive       => Failure(extract(what))
    })
  }
}
