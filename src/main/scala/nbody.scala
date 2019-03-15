import idealised.C
import idealised.SurfaceLanguage.DSL._
import idealised.SurfaceLanguage.Types._
import lift.arithmetic.SizeVar

object nbody extends App {

  val update = fun(pos => fun(vel => fun(deltaT => fun(acceleration =>
    foreignFun(
      TupleType(float, float), "update", Seq((float, "pos"), (float, "vel"), (float, "deltaT"), (float, "acceleration")),
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
      Seq(pos, vel, deltaT, acceleration))))))

  {
    val f = fun(ArrayType(SizeVar("N"), float))(input =>
      mapSeq( update(1.0f)(2.0f)(3.0f) ) $ input
    )
    val phrase = TypeInference(f, Map()).convertToPhrase
    val program = C.ProgramGenerator.makeCode(phrase)
    println(program.code)
  }



}
