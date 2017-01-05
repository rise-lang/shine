// Allocations on the host: 
// output (global): 67108864 bytes
// input (global): 67108864 bytes
// value (private): 4 bytes
// length (private): 4 bytes

kernel void KERNEL(global float* output, const global float* restrict v101, float v102, int v_N_0){ 
#ifndef WORKGROUP_GUARD
#define WORKGROUP_GUARD
#endif
WORKGROUP_GUARD
{
  for (int v145 = get_group_id(0);v145<(v_N_0 / (2048));v145 = (get_num_groups(0) + v145)){
    for (int v147 = get_local_id(0);v147<2048;v147 = (get_local_size(0) + v147)){
      /* iteration count is exactly 1, no loop emitted */
      {
        int v148 = 0;
        output[((2048 * v145) + v147)] = (v102 * v101[((2048 * v145) + v147)]);
      }
    }
    barrier(CLK_LOCAL_MEM_FENCE | CLK_GLOBAL_MEM_FENCE);
  }
  /* par for workgroup sync */
}}
