package shine.DPIA

import rise.core.DSL._
import rise.core.types.infer
import shine.DPIA.inferAccessAnnotation._
import shine.DPIA.RiseExprAnnotated._
import shine.DPIA.Types.{read, write}
import shine.DPIA.access.{AccessTypeAnnotation, DataType, FunType}

class InferAccessTypes extends test_util.Tests {
  private val x = identifier(rise.core.freshName("x"))
  private val id = infer(lambda(x, x))
  private val annotId =
    Lambda[AccessTypeAnnotation](
      Identifier(x.name, DataType(read)),
      Identifier(x.name, DataType(read)),
      FunType(DataType(read), DataType(read)))

  test("(read -> read) is inferred for id.") {
    val inferredId = inferAccessAnnotation(id)
    assertResult(annotId)(inferredId)
  }

  test("(read -> write) is inferred for id over array with mapSeq") {
    val arrIdent = identifier(rise.core.freshName("arrIdent"))
    val arrId = infer(lambda(arrIdent, arrIdent |> mapSeq(id)))
    val inferredArrId = inferAccessAnnotation(arrId)
    val annotArrId =
      Lambda[AccessTypeAnnotation](
        Identifier(arrIdent.name, DataType(read)),
        App(
          App(
            Primitive(mapSeqAccessType),
            annotId,
            FunType(DataType(read), DataType(write))),
          Identifier(arrIdent.name, DataType(read)),
          DataType(write)),
        FunType(DataType(read), DataType(write)))

    println(annotArrId)
    println(inferredArrId)
    assertResult(annotArrId)(inferredArrId)
  }
}
