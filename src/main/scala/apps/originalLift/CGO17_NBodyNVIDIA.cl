#ifndef Tuple_float4_float4_DEFINED
#define Tuple_float4_float4_DEFINED
typedef struct {
  float4 _0;
  float4 _1;
} Tuple_float4_float4;
#endif
#ifndef Tuple_Tuple_float4_float4_float4_DEFINED
#define Tuple_Tuple_float4_float4_float4_DEFINED
typedef struct {
  Tuple_float4_float4 _0;
  float4 _1;
} Tuple_Tuple_float4_float4_float4;
#endif

#ifndef Tuple_float4_float4_DEFINED
#define Tuple_float4_float4_DEFINED
typedef struct {
  float4 _0;
  float4 _1;
} Tuple_float4_float4;
#endif

float4 id(float4 x){
  { return x; }
}
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
kernel void KERNEL(const global float* restrict v__39, const global float* restrict v__40, float v__41, float v__42, global Tuple_float4_float4* v__58, int v_N_0){
  /* Static local memory */
  local float v__50[1024] __attribute__ ((aligned(16)));
  /* Typed Value memory */
  float4 v__47;
  /* Private Memory */
  float4 v__45_0;

  float4 v__48_0;

  /* iteration count is exactly 1, no loop emitted */
  {
    int v_wg_id_25 = get_group_id(1);
    /* iteration count is exactly 1, no loop emitted */
    {
      int v_wg_id_26 = get_group_id(0);
      /* unroll */
      v__45_0 = id(vload4(((v_N_0 * v_wg_id_25) + (256 * v_wg_id_26) + get_local_id(0)),v__39));
      /* end unroll */
      barrier(CLK_LOCAL_MEM_FENCE | CLK_GLOBAL_MEM_FENCE);

      float4 v_tmp_113 = 0.0f;
      v__47 = v_tmp_113;
      /* unroll */
      /* unroll */
      v__48_0 = id(v__47);
      /* end unroll */
      /* end unroll */
      /* reduce_seq */
      for (int v_i_31 = 0;v_i_31<(v_N_0 / (256));v_i_31 = (1 + v_i_31)){
        /* iteration count is exactly 1, no loop emitted */
        {
          int v_l_id_32 = get_local_id(1);
          /* iteration count is exactly 1, no loop emitted */
          {
            int v_l_id_33 = get_local_id(0);
            vstore4(id(vload4((v_l_id_33 + (256 * v_i_31)),v__39)),(v_l_id_33 + (256 * v_l_id_32)),v__50);;
          }
        }
        barrier(CLK_LOCAL_MEM_FENCE);

        /* unroll */
        /* unroll */
        /* reduce_seq */
        for (int v_i_36 = 0;v_i_36<256;v_i_36 = (1 + v_i_36)){
          v__48_0 = calcAcc(v__45_0, vload4((v_i_36 + (256 * get_local_id(1))),v__50), v__42, v__41, v__48_0);
        }
        /* end reduce_seq */
        /* end unroll */
        /* end unroll */
        barrier(CLK_LOCAL_MEM_FENCE | CLK_GLOBAL_MEM_FENCE);

      }
      /* end reduce_seq */
      /* unroll */
      /* unroll */
      v__58[((256 * get_local_id(1)) + (v_N_0 * v_wg_id_25) + (256 * v_wg_id_26) + get_local_id(0))] = update(v__45_0, vload4(((v_N_0 * v_wg_id_25) + (256 * v_wg_id_26) + get_local_id(0)),v__40), v__42, v__48_0);
      /* end unroll */
      /* end unroll */
    }
  }
}
