// Allocations on the host: 
// output (global): 67108864 bytes
// input (global): 67108864 bytes
// value (private): 4 bytes
// length (private): 4 bytes

kernel void KERNEL(global float* output, const global float* restrict v149, float v150, int v_N_0){ 
#ifndef WORKGROUP_GUARD
#define WORKGROUP_GUARD
#endif
WORKGROUP_GUARD
{
  for (int v199 = get_group_id(0);v199<(v_N_0 / (65536));v199 = (get_num_groups(0) + v199)){
    /* iteration count is exactly 1, no loop emitted */
    {
      int v201 = get_local_id(0);
      for (int v202 = 0;v202<128;v202 = (1 + v202)){
        vstore4(((float4)(v150) * vload4(((128 * v201) + (16384 * v199) + v202),v149)),((128 * v201) + (16384 * v199) + v202),output);
      }
    }
    barrier(CLK_LOCAL_MEM_FENCE | CLK_GLOBAL_MEM_FENCE);
  }
  /* par for workgroup sync */
}}
