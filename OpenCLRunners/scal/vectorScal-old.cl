// Allocated (4*v_N_0) bytes for variable v__38 in global memory
// Allocated 4 bytes for variable v__39 in private memory
// Allocated (4*v_N_0) bytes for variable v__41 in global memory
// Kernel code:

float mult(float l, float r){
  { return l * r; }
}
kernel void KERNEL(const global float* restrict  v__38, float v__39, global float*  v__41, int v_N_0){ 
#ifndef WORKGROUP_GUARD
#define WORKGROUP_GUARD
#endif
WORKGROUP_GUARD
{
  /* Static local memory */
  /* Typed Value memory */
  /* Private Memory */
  for (int v_wg_id_35 = get_group_id(0);v_wg_id_35<(v_N_0 / (1024));v_wg_id_35 = (v_wg_id_35 + get_num_groups(0))){
    for (int v_l_id_36 = get_local_id(0);v_l_id_36<256;v_l_id_36 = (v_l_id_36 + get_local_size(0))){
      /* map_seq */
      for (int v_i_37 = 0;v_i_37<4;v_i_37 = (1 + v_i_37)){
        v__41[(v_i_37 + (4 * v_l_id_36) + (1024 * v_wg_id_35))] = mult(v__39, v__38[(v_i_37 + (4 * v_l_id_36) + (1024 * v_wg_id_35))]);
      }
      /* end map_seq */
    }
  }
}}
