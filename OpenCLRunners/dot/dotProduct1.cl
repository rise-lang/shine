// Allocations on the host: 
// output (global): 2048 bytes
// input (global): 4194304 bytes
// input (global): 4194304 bytes
// length (private): 4 bytes

kernel void KERNEL(global float* output, const global float* restrict v190, const global float* restrict v191, int v_N_0){ 
#ifndef WORKGROUP_GUARD
#define WORKGROUP_GUARD
#endif
WORKGROUP_GUARD
{
  for (int v251 = get_group_id(0);v251<(v_N_0 / (262144));v251 = (get_num_groups(0) + v251)){
    /* iteration count is exactly 1, no loop emitted */
    {
      int v253 = get_local_id(0);
      float v254;
      v254 = 0.0;
      for (int v255 = 0;v255<2048;v255 = (1 + v255)){
        v254 = ((v190[((262144 * v251) + (128 * v255) + v253)] * v191[((262144 * v251) + (128 * v255) + v253)]) + v254);
      }
      output[((128 * v251) + v253)] = v254;
    }
    barrier(CLK_LOCAL_MEM_FENCE | CLK_GLOBAL_MEM_FENCE);
  }
  /* par for workgroup sync */
}}
