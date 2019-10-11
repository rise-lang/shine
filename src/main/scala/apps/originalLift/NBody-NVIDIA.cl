#ifndef Tuple2_float4_float4_DEFINED
#define Tuple2_float4_float4_DEFINED
typedef struct __attribute__((aligned(16))){
    float4 _0;
    float4 _1;
} Tuple2_float4_float4;
#endif

#ifndef Tuple2_Tuple2_float4_float4_float4_DEFINED
#define Tuple2_Tuple2_float4_float4_float4_DEFINED
typedef struct __attribute__((aligned(16))){
    Tuple2_float4_float4 _0;
    float4 _1;
} Tuple2_Tuple2_float4_float4_float4;
#endif

#ifndef Tuple2_float4_float4_DEFINED
#define Tuple2_float4_float4_DEFINED
typedef struct __attribute__((aligned(16))){
    float4 _0;
    float4 _1;
} Tuple2_float4_float4;
#endif

float4 idF4(float4 x){
    {
        { return x; }; 
    }
}
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
kernel __attribute((reqd_work_group_size(256,1,1)))
void KERNEL(const global float* restrict v__36, const global float* restrict v__37, float v__38, float v__39, global Tuple2_float4_float4* v__54, int v_N_1){

        // Static local memory
        local float v__46[1024];
        // Typed Value memory
        float4 v__43; 
        // Private Memory
        float4 v_v__41_1_104; 
        float4 v_v__44_1_105; 
        // iteration count is exactly 1, no loop emitted
        {
            int v_wg_id_22 = get_group_id(1); 
            // iteration count is exactly 1, no loop emitted
            {
                int v_wg_id_23 = get_group_id(0); 
                // unroll
                v_v__41_1_104 = idF4(vload4(((256 * v_wg_id_23) + (v_N_1 * v_wg_id_22) + get_local_id(0)),v__36 + 0)); 
                // end unroll
                barrier(CLK_LOCAL_MEM_FENCE | CLK_GLOBAL_MEM_FENCE);
                float4 v_tmp_102 = 0.0f; 
                v__43 = v_tmp_102; 
                // unroll
                // unroll
                v_v__44_1_105 = idF4(v__43); 
                // end unroll
                // end unroll
                // reduce_seq
                for (int v_i_28 = 0; (v_i_28 < ((v_N_1)/(256))); v_i_28 = (1 + v_i_28)){
                    // iteration count is exactly 1, no loop emitted
                    {
                        int v_l_id_29 = get_local_id(1); 
                        // iteration count is exactly 1, no loop emitted
                        {
                            int v_l_id_30 = get_local_id(0); 
                            vstore4(idF4(vload4((v_l_id_30 + (256 * v_i_28) + (256 * v_l_id_29)),v__36 + 0)),(v_l_id_30 + (256 * v_l_id_29)),v__46); 
                        }
                    }
                    barrier(CLK_LOCAL_MEM_FENCE);
                    // unroll
                    // unroll
                    // reduce_seq
                    for (int v_i_33 = 0; (v_i_33 < 256); v_i_33 = (1 + v_i_33)){
                        v_v__44_1_105 = calcAcc(v_v__41_1_104, vload4((v_i_33 + (256 * get_local_id(1))),v__46 + 0), v__39, v__38, v_v__44_1_105); 
                    }
                    // end reduce_seq
                    // end unroll
                    // end unroll
                    barrier(CLK_LOCAL_MEM_FENCE | CLK_GLOBAL_MEM_FENCE);
                }
                // end reduce_seq
                // unroll
                // unroll
                v__54[((256 * v_wg_id_23) + (256 * get_local_id(1)) + (v_N_1 * v_wg_id_22) + get_local_id(0))] = update(v_v__41_1_104, vload4(((256 * v_wg_id_23) + (v_N_1 * v_wg_id_22) + get_local_id(0)),v__37 + 0), v__39, v_v__44_1_105); 
                // end unroll
                // end unroll
            }
        }
}
