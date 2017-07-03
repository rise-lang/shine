// Allocations on the host: 
// output (global): 2048 bytes
// input (global): 4194304 bytes
// length (private): 4 bytes

kernel void KERNEL(global float* output, const global float* restrict v312, int v_N_0){ 
#ifndef WORKGROUP_GUARD
#define WORKGROUP_GUARD
#endif
WORKGROUP_GUARD
{
  for (int v370 = get_group_id(0);v370<(v_N_0 / (524288));v370 = (get_num_groups(0) + v370)){
    /* iteration count is exactly 1, no loop emitted */
    {
      int v372 = get_local_id(0);
      float2 v373;
      v373 = (float2)(0.0, 0.0);
      for (int v374 = 0;v374<2048;v374 = (1 + v374)){
        v373 = (fabs(vload2(((((2048 * v372) + v374) / 4096) + ((((2048 * v372) + v374) % 4096) * 64) + (262144 * v370)),v312)) + v373);
      }
      vstore2(v373,((128 * v370) + v372),output);
    }
    barrier(CLK_LOCAL_MEM_FENCE | CLK_GLOBAL_MEM_FENCE);
  }
  /* par for workgroup sync */
}}
