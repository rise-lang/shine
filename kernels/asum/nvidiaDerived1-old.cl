// Allocated (4*v_N_21) bytes for variable v__68 in global memory
// Allocated (v_N_21*1/^(512)) bytes for variable v__74 in global memory
// Kernel code:

float add(float x, float y){
  { return x+y; }
}
float id(float x){
  { return x; }
}
kernel void KERNEL(const global float* restrict v__68, global float* v__74, int v_N_21){ 
#ifndef WORKGROUP_GUARD
#define WORKGROUP_GUARD
#endif
WORKGROUP_GUARD
{
  /* Static local memory */
  /* Typed Value memory */
  float v__70;
  /* Private Memory */
  for (int v_wg_id_64 = get_group_id(0);v_wg_id_64<(v_N_21 / (262144));v_wg_id_64 = (v_wg_id_64 + get_num_groups(0))){
    for (int v_l_id_65 = get_local_id(0);v_l_id_65<128;v_l_id_65 = (v_l_id_65 + get_local_size(0))){
      float v_tmp_87 = 0.0f;
      v__70 = v_tmp_87;
      /* reduce_seq */
      for (int v_i_66 = 0;v_i_66<2048;v_i_66 = (1 + v_i_66)){
        v__70 = add(v__70, v__68[(v_l_id_65 + (128 * v_i_66) + (262144 * v_wg_id_64))]);
      }
      /* end reduce_seq */
      /* map_seq */
      /* iteration count is exactly 1, no loop emitted */
      {
        int v_i_67 = 0;
        v__74[(v_l_id_65 + (128 * v_wg_id_64))] = id(v__70);
      }
      /* end map_seq */
    }
  }
}}
