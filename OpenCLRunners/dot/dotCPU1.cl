// Allocations on the host: 
// output (global): 2048 bytes
// input (global): 4194304 bytes
// input (global): 4194304 bytes
// length (private): 4 bytes

kernel void KERNEL(global float* output, const global float* restrict v79, const global float* restrict v80, int v_N_0){ 
#ifndef WORKGROUP_GUARD
#define WORKGROUP_GUARD
#endif
WORKGROUP_GUARD
{
  for (int v133 = get_group_id(0);v133<(v_N_0 / (262144));v133 = (get_num_groups(0) + v133)){
    /* iteration count is exactly 1, no loop emitted */
    {
      int v135 = get_local_id(0);
      float v136;
      v136 = 0.0;
      for (int v137 = 0;v137<2048;v137 = (1 + v137)){
        v136 = ((v79[((2048 * v135) + (262144 * v133) + v137)] * v80[((2048 * v135) + (262144 * v133) + v137)]) + v136);
      }
      output[((128 * v133) + v135)] = v136;
    }
    barrier(CLK_LOCAL_MEM_FENCE | CLK_GLOBAL_MEM_FENCE);
  }
  /* par for workgroup sync */
}}
