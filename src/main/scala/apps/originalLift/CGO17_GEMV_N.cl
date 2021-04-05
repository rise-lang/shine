#ifndef Tuple_float_float_DEFINED
#define Tuple_float_float_DEFINED
typedef struct {
  float _0;
  float _1;
} Tuple_float_float;
#endif

float id(float x){
  { return x; }
}
float multAndSumUp(float acc, float l, float r){
  { return acc + (l * r); }
}
float add(float x, float y){
  { return x+y; }
}
float mult(float l, float r){
  { return l * r; }
}
kernel void KERNEL(const global float* restrict v__26, const global float* restrict v__27, const global float* restrict v__28, float v__29, float v__30, global float* v__51, int v_M_1, int v_N_0){
  /* Static local memory */
  local float v__37[64] __attribute__ ((aligned(16)));
  /* Typed Value memory */
  float v__33;
  /* Private Memory */
  float v__34_0;
  
  float v__46_0;
  
  float v__49_0;
  
  /* iteration count is exactly 1, no loop emitted */
  {
    int v_wg_id_18 = get_group_id(0);
    float v_tmp_119 = 0.0f;
    v__33 = v_tmp_119;
    /* unroll */
    v__34_0 = id(v__33);
    /* end unroll */
    /* reduce_seq */
    for (int v_i_20 = 0;v_i_20<(v_N_0 / (64));v_i_20 = (1 + v_i_20)){
      /* iteration count is exactly 1, no loop emitted */
      {
        int v_l_id_21 = get_local_id(0);
        v__37[v_l_id_21] = id(v__27[(v_l_id_21 + (64 * v_i_20))]);
      }
      barrier(CLK_LOCAL_MEM_FENCE);
      
      /* unroll */
      /* reduce_seq */
      for (int v_i_23 = 0;v_i_23<64;v_i_23 = (1 + v_i_23)){
        v__34_0 = multAndSumUp(v__34_0, v__26[(v_i_23 + (64 * v_N_0 * v_wg_id_18) + (64 * v_i_20) + (v_N_0 * get_local_id(0)))], v__37[v_i_23]);
      }
      /* end reduce_seq */
      /* end unroll */
      barrier(CLK_LOCAL_MEM_FENCE | CLK_GLOBAL_MEM_FENCE);
      
    }
    /* end reduce_seq */
    /* map_seq */
    /* unroll */
    /* unroll */
    v__46_0 = mult(v__34_0, v__29);
    v__49_0 = mult(v__28[((64 * v_wg_id_18) + get_local_id(0))], v__30);
    v__51[((64 * v_wg_id_18) + get_local_id(0))] = add(v__46_0, v__49_0);
    /* end unroll */
    /* end unroll */
    /* end map_seq */
  }
}
