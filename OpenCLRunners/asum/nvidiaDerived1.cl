// Allocations on the host: 
// output (global): 2048 bytes
// input (global): 4194304 bytes
// length (private): 4 bytes

kernel void KERNEL(global float* output, const global float* restrict v120, int v_N_0){ 
#ifndef WORKGROUP_GUARD
#define WORKGROUP_GUARD
#endif
WORKGROUP_GUARD
{
  for (int v176 = get_group_id(0);v176<(v_N_0 / (262144));v176 = (get_num_groups(0) + v176)){
    /* iteration count is exactly 1, no loop emitted */
    {
      int v178 = get_local_id(0);
      float v179;
      v179 = 0.0;
      for (int v180 = 0;v180<2048;v180 = (1 + v180)){
        v179 = (fabs(v120[((262144 * v176) + (128 * v180) + v178)]) + v179);
      }
      output[((128 * v176) + v178)] = v179;
    }
    barrier(CLK_LOCAL_MEM_FENCE | CLK_GLOBAL_MEM_FENCE);
  }
  /* par for workgroup sync */
}}
