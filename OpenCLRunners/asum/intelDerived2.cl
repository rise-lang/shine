// Allocations on the host: 
// output (global): 2048 bytes
// input (global): 4194304 bytes
// length (private): 4 bytes

kernel void KERNEL(global float* output, const global float* restrict v70, int v_N_0){ 
#ifndef WORKGROUP_GUARD
#define WORKGROUP_GUARD
#endif
WORKGROUP_GUARD
{
  for (int v115 = get_group_id(0);v115<(v_N_0 / (2048));v115 = (get_num_groups(0) + v115)){
    /* iteration count is 1 or less, no loop emitted */
    {
      int v117 = get_local_id(0);
      if(get_local_id(0)<1)
      {
        float v118;
        v118 = 0.0;
        for (int v119 = 0;v119<2048;v119 = (1 + v119)){
          v118 = (v70[((2048 * v115) + v119)] + v118);
        }
        output[v115] = v118;
      }
    }
    barrier(CLK_LOCAL_MEM_FENCE | CLK_GLOBAL_MEM_FENCE);
  }
  /* par for workgroup sync */
}}
