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
float add(float x, float y){
  { return x+y; }
}
float mult(float l, float r){
  { return l * r; }
}
kernel void KERNEL(const global float* restrict v__22, const global float* restrict v__23, const global float* restrict v__24, float v__25, float v__26, global float* v__52, global float* v__48, global float* v__50, int v_M_0, int v_N_1){ 
#ifndef WORKGROUP_GUARD
#define WORKGROUP_GUARD
#endif
WORKGROUP_GUARD
{
  /* Static local memory */
  local float v__46[1];
  local float v__42[128];
  /* Typed Value memory */
  float v__29;
  float v__32;
  /* Private Memory */
  float v__30_0;
  
  float v__33_0;
  
  float v__35_0;
  
  float v__36_0;
  
  float v__39_0;
  
  /* iteration count is exactly 1, no loop emitted */
  {
    int v_wg_id_15 = get_group_id(0);
    float v_tmp_102 = 0.0f;
    v__29 = v_tmp_102;
    v__30_0 = id(v__29);
    /* iteration count is exactly 1, no loop emitted */
    {
      int v_l_id_16 = get_local_id(0);
      float v_tmp_103 = 0.0f;
      v__32 = v_tmp_103;
      v__33_0 = id(v__32);
      /* reduce_seq */
      for (int v_i_17 = 0;v_i_17<(v_M_0 / (128));v_i_17 = (1 + v_i_17)){
        v__35_0 = id(v__23[(v_l_id_16 + (128 * v_i_17))]);
        v__36_0 = id(v__22[(v_l_id_16 + (128 * v_i_17) + (v_M_0 * v_wg_id_15))]);
        v__39_0 = mult(v__35_0, v__36_0);
        v__33_0 = add(v__33_0, v__39_0);
      }
      /* end reduce_seq */
      /* map_seq */
      /* unroll */
      v__42[v_l_id_16] = id(v__33_0);
      /* end unroll */
      /* end map_seq */
    }
    barrier(CLK_LOCAL_MEM_FENCE);
    
    /* reduce_seq */
    for (int v_i_19 = 0;v_i_19<128;v_i_19 = (1 + v_i_19)){
      v__30_0 = add(v__30_0, v__42[v_i_19]);
    }
    /* end reduce_seq */
    /* map_seq */
    /* unroll */
    v__46[0] = id(v__30_0);
    /* end unroll */
    /* end map_seq */
    /* iteration count is exactly 1 or less, no loop emitted */
    if(get_local_id(0)<1)
    {
      int v_l_id_21 = get_local_id(0);
      v__48[v_wg_id_15] = mult(v__46[v_l_id_21], v__25);
      v__50[v_wg_id_15] = mult(v__24[v_wg_id_15], v__26);
      v__52[v_wg_id_15] = add(v__48[v_wg_id_15], v__50[v_wg_id_15]);
    }
  }
}}
