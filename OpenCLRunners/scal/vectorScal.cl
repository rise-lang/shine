// Allocations on the host: 
// output (global): 67108864 bytes
// input (global): 67108864 bytes
// value (private): 4 bytes
// length (private): 4 bytes

kernel void KERNEL(global float* output, const global float* restrict v5, float v6, int v_N_0){ 
#ifndef WORKGROUP_GUARD
#define WORKGROUP_GUARD
#endif
WORKGROUP_GUARD
{
  for (int v49 = get_group_id(0);v49<(v_N_0 / (1024));v49 = (get_num_groups(0) + v49)){
    for (int v51 = get_local_id(0);v51<256;v51 = (get_local_size(0) + v51)){
      for (int v52 = 0;v52<4;v52 = (1 + v52)){
        output[((4 * v51) + (1024 * v49) + v52)] = (v6 * v5[((4 * v51) + (1024 * v49) + v52)]);
      }
    }
    barrier(CLK_LOCAL_MEM_FENCE | CLK_GLOBAL_MEM_FENCE);
  }
  /* par for workgroup sync */
}}
