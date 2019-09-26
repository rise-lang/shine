#ifndef Tuple2_float4_float4_DEFINED
#define Tuple2_float4_float4_DEFINED
typedef struct __attribute__((aligned(16))){
    float4 _0;
    float4 _1;
} Tuple2_float4_float4;
#endif

float4 calcAcc(float4 p1, float4 p2, float deltaT, float espSqr, float4 acc){
    {
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
 ; 
    }
}
Tuple2_float4_float4 update(float4 pos, float4 vel, float deltaT, float4 acceleration){
    {
        typedef Tuple2_float4_float4 Tuple;
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
      ; 
    }
}
kernel void KERNEL(const global float* restrict v__25, const global float* restrict v__26, float v__27, float v__28, global Tuple2_float4_float4* v__35, int v_N_1){
    #ifndef WORKGROUP_GUARD
    #define WORKGROUP_GUARD
    #endif
    WORKGROUP_GUARD
    {
        // Static local memory
        // Typed Value memory
        float4 v__30; 
        // Private Memory
        for (int v_gl_id_22 = get_global_id(0); (v_gl_id_22 < v_N_1); v_gl_id_22 = (v_gl_id_22 + get_global_size(0))){
            float4 v_tmp_50 = (float4) 0.0f; 
            v__30 = v_tmp_50; 
            // reduce_seq
            for (int v_i_23 = 0; (v_i_23 < v_N_1); v_i_23 = (1 + v_i_23)){
                v__30 = calcAcc(vload4(v_gl_id_22,v__25 + 0), vload4(v_i_23,v__25 + 0), v__28, v__27, v__30); 
            }
            // end reduce_seq
            // map_seq
            // iteration count is exactly 1, no loop emitted
            {
                int v_i_24 = 0; 
                v__35[v_gl_id_22] = update(vload4(v_gl_id_22,v__25 + 0), vload4(v_gl_id_22,v__26 + 0), v__28, v__30); 
            }
            // end map_seq
        }
    }
}
