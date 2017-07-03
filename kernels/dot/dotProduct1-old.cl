// Allocated (4*v_N_0) bytes for variable v__55 in global memory
// Allocated (4*v_N_0) bytes for variable v__56 in global memory
// Allocated (v_N_0*1/^(512)) bytes for variable v__63 in global memory
// Kernel code:

#ifndef Tuple_float_float_DEFINED
#define Tuple_float_float_DEFINED
typedef struct {
  float _0;
  float _1;
} Tuple_float_float;
#endif

float multAndSumUp(float acc, float l, float r){
  { return acc + (l * r); }
}
float id(float x){
  { return x; }
}
kernel void KERNEL(const global float* restrict v__55, const global float* restrict v__56, global float* v__63, int v_N_0){ 
#ifndef WORKGROUP_GUARD
#define WORKGROUP_GUARD
#endif
WORKGROUP_GUARD
{
  /* Static local memory */
  /* Typed Value memory */
  float v__59;
  /* Private Memory */
  for (int v_wg_id_51 = get_group_id(0);v_wg_id_51<(v_N_0 / (262144));v_wg_id_51 = (v_wg_id_51 + get_num_groups(0))){
    for (int v_l_id_52 = get_local_id(0);v_l_id_52<128;v_l_id_52 = (v_l_id_52 + get_local_size(0))){
      float v_tmp_80 = 0.0f;
      v__59 = v_tmp_80;
      /* reduce_seq */
      for (int v_i_53 = 0;v_i_53<2048;v_i_53 = (1 + v_i_53)){
        v__59 = multAndSumUp(v__59, v__55[(v_l_id_52 + (128 * v_i_53) + (262144 * v_wg_id_51))], v__56[(v_l_id_52 + (128 * v_i_53) + (262144 * v_wg_id_51))]);
      }
      /* end reduce_seq */
      /* map_seq */
      /* iteration count is exactly 1, no loop emitted */
      {
        int v_i_54 = 0;
        v__63[(v_l_id_52 + (128 * v_wg_id_51))] = id(v__59);
      }
      /* end map_seq */
    }
  }
}}
