// Allocations on the host: 
// output (global): 16 bytes
// input (global): 2048 bytes
// length (private): 4 bytes

kernel void KERNEL(global float* output, const global float* restrict v138, int v_N_0){ 
#ifndef WORKGROUP_GUARD
#define WORKGROUP_GUARD
#endif
WORKGROUP_GUARD
{
  for (int v185 = get_group_id(0);v185<(v_N_0 / (128));v185 = (get_num_groups(0) + v185)){
    /* iteration count is 1 or less, no loop emitted */
    {
      int v187 = get_local_id(0);
      if(get_local_id(0)<1)
      {
        float v188;
        v188 = 0.0;
        for (int v189 = 0;v189<128;v189 = (1 + v189)){
          v188 = (v138[((128 * v185) + v189)] + v188);
        }
        output[v185] = v188;
      }
    }
    barrier(CLK_LOCAL_MEM_FENCE | CLK_GLOBAL_MEM_FENCE);
  }
  /* par for workgroup sync */
}}
