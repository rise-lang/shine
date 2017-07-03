// Allocations on the host: 
// output (global): 67108864 bytes
// input (global): 67108864 bytes
// value (private): 4 bytes
// length (private): 4 bytes

kernel void KERNEL(global float* output, const global float* restrict v53, float v54, int v_N_0){ 
#ifndef WORKGROUP_GUARD
#define WORKGROUP_GUARD
#endif
WORKGROUP_GUARD
{
  for (int v97 = get_group_id(0);v97<(v_N_0 / (128));v97 = (get_num_groups(0) + v97)){
    /* iteration count is exactly 1, no loop emitted */
    {
      int v99 = get_local_id(0);
      /* iteration count is exactly 1, no loop emitted */
      {
        int v100 = 0;
        output[((128 * v97) + v99)] = (v54 * v53[((128 * v97) + v99)]);
      }
    }
    barrier(CLK_LOCAL_MEM_FENCE | CLK_GLOBAL_MEM_FENCE);
  }
  /* par for workgroup sync */
}}
