float4 idF4(float4 x){
    {
        { return x; }; 
    }
}
float4 updateF(float4 f, float4 ipos, float4 jpos, float cutsq, float lj1, float lj2){
    {
        {
  // Calculate distance
  float delx = ipos.x - jpos.x;
  float dely = ipos.y - jpos.y;
  float delz = ipos.z - jpos.z;
  float r2inv = delx*delx + dely*dely + delz*delz;
  // If distance is less than cutoff, calculate force
  if (r2inv < cutsq) {
    r2inv = 1.0f/r2inv;
    float r6inv = r2inv * r2inv * r2inv;
    float forceC = r2inv*r6inv*(lj1*r6inv - lj2);
    f.x += delx * forceC;
    f.y += dely * forceC;
    f.z += delz * forceC;
  }
  return f;
}
    ; 
    }
}
float4 id4(float4 x){
    {
        { return x; }; 
    }
}
kernel void KERNEL(const global float* restrict v__11, const global int* restrict v__12, float v__13, float v__14, float v__15, global float* v__23, int v_M_1, int v_N_0){
    #ifndef WORKGROUP_GUARD
    #define WORKGROUP_GUARD
    #endif
    WORKGROUP_GUARD
    {
        // Static local memory
        // Typed Value memory
        float4 v__18; 
        // Private Memory
        float4 v_v__17_1_47; 
        for (int v_wg_id_7 = get_group_id(0); (v_wg_id_7 < ((v_N_0)/(128))); v_wg_id_7 = (v_wg_id_7 + get_num_groups(0))){
            for (int v_l_id_8 = get_local_id(0); (v_l_id_8 < 128); v_l_id_8 = (v_l_id_8 + get_local_size(0))){
                v_v__17_1_47 = idF4(vload4((v_l_id_8 + (128 * v_wg_id_7)),v__11 + 0)); 
                float4 v_tmp_37 = 0.0f; 
                v__18 = v_tmp_37; 
                // reduce_seq
                for (int v_i_9 = 0; (v_i_9 < v_M_1); v_i_9 = (1 + v_i_9)){
                    v__18 = updateF(v__18, v_v__17_1_47, vload4(v__12[(v_l_id_8 + (128 * v_wg_id_7) + (v_N_0 * v_i_9))],v__11 + 0), v__13, v__14, v__15); 
                }
                // end reduce_seq
                // map_seq
                // iteration count is exactly 1, no loop emitted
                {
                    int v_i_10 = 0; 
                    vstore4(id4(v__18),(v_l_id_8 + (128 * v_wg_id_7)),v__23); 
                }
                // end map_seq
            }
        }
    }
}
