// Allocations on the host: 
// output (global): 512 bytes
// input (global): 4194304 bytes
// length (private): 4 bytes

kernel void KERNEL(global float* output, const global float* restrict v11, int v_N_0){ 
#ifndef WORKGROUP_GUARD
#define WORKGROUP_GUARD
#endif
WORKGROUP_GUARD
{
  for (int v65 = get_group_id(0);v65<(v_N_0 / (32768));v65 = (get_num_groups(0) + v65)){
    /* iteration count is 1 or less, no loop emitted */
    {
      int v67 = get_local_id(0);
      if(get_local_id(0)<1)
      {
        float4 v68;
        v68 = (float4)(0.0, 0.0, 0.0, 0.0);
        for (int v69 = 0;v69<8192;v69 = (1 + v69)){
          v68 = (fabs(vload4(((8192 * v65) + v69),v11)) + v68);
        }
        vstore4(v68,v65,output);
      }
    }
    barrier(CLK_LOCAL_MEM_FENCE | CLK_GLOBAL_MEM_FENCE);
  }
  /* par for workgroup sync */
}}
