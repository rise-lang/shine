#ifndef Tuple_float4_float4_DEFINED
#define Tuple_float4_float4_DEFINED
typedef struct {
  float4 _0;
  float4 _1;
} Tuple_float4_float4;
#endif

float4 calcAcc(float4 p1, float4 p2, float deltaT, float espSqr, float4 acc){
  {
  float4 r;
  r.xyz = p2.xyz - p1.xyz ;
  float distSqr = r.x*r.x + r.y*r.y + r.z*r.z;
  float invDist = 1.0f / sqrt(distSqr + espSqr);
  float invDistCube = invDist * invDist * invDist;
  float s = invDistCube * p2.w;
  float4 res;
  res.xyz = acc.xyz + s * r.xyz;
  return res;
}

}
Tuple_float4_float4 update(float4 pos, float4 vel, float deltaT, float4 acceleration){
  typedef Tuple_float4_float4 Tuple;

  {
  float4 newPos;
  newPos.xyz = pos.xyz + vel.xyz * deltaT + 0.5f * acceleration.xyz * deltaT * deltaT;
  newPos.w = pos.w;
  float4 newVel;
  newVel.xyz = vel.xyz + acceleration.xyz * deltaT;
  newVel.w = vel.w;
  Tuple t = {newPos, newVel};
  return t;
}

}
kernel void KERNEL(const global float4* restrict v__28, const global float4* restrict v__29, float v__30, float v__31, global Tuple_float4_float4* v__39, int v_N_0){
  /* Static local memory */
  /* Typed Value memory */
  float4 v__34;
  /* Private Memory */
  /* iteration count is exactly 1, no loop emitted */
  {
    int v_gl_id_25 = get_global_id(0);
    float4 v_tmp_55 = (float4) 0.0f;
    v__34 = v_tmp_55;
    /* reduce_seq */
    for (int v_i_26 = 0;v_i_26<v_N_0;v_i_26 = (1 + v_i_26)){
      v__34 = calcAcc(v__28[v_gl_id_25], v__28[v_i_26], v__31, v__30, v__34);
    }
    /* end reduce_seq */
    /* map_seq */
    /* iteration count is exactly 1, no loop emitted */
    {
      int v_i_27 = 0;
      v__39[v_gl_id_25] = update(v__28[v_gl_id_25], v__29[v_gl_id_25], v__31, v__34);
    }
    /* end map_seq */
  }
}