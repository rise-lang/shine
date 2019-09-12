package apps

import lift.core.DSL._
import lift.core.types._
import lift.core.primitives._
import idealised.util.gen

class nbody extends idealised.util.Tests {
  val update = foreignFun("update",
    Seq("pos", "vel", "deltaT", "acceleration"),
    """|{
       |  float4 newPos;
       |  newPos.xyz = pos.xyz + vel.xyz * deltaT + 0.5f * acceleration.xyz * deltaT * deltaT;
       |  newPos.w = pos.w;
       |  float4 newVel;
       |  newVel.xyz = vel.xyz + acceleration.xyz * deltaT;
       |  newVel.w = vel.w;
       |  Tuple t = {newPos, newVel};
       |  return t;
       |}
    """.stripMargin,
    float4 ->: float4 ->: float4 ->: float4 ->: TupleType(float4, float4)
  )

  val e = nFun(n => fun(ArrayType(n, float4))(input =>
    input |> mapSeq(update
      (vectorFromScalar(l(1.0f)))
      (vectorFromScalar(l(2.0f)))
      (vectorFromScalar(l(3.0f))))
  ))

  // TODO: fix struct declarations
  ignore("nbody expression generates valid OpenCL") {
    gen.OpenCLKernel(e)
  }
}
