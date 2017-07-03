// Allocations on the host: 
// output (global): 2048 bytes
// input (global): 1048576 bytes
// input (global): 2048 bytes
// input (global): 2048 bytes
// value (private): 4 bytes
// value (private): 4 bytes
// intermediate (local): 4 bytes
// length (private): 4 bytes
// length (private): 4 bytes

kernel void KERNEL(global float* output, const global float* restrict v26, const global float* restrict v27, const global float* restrict v28, float v29, float v30, local float* v70, int v_M_1, int v_N_0){ 
#ifndef WORKGROUP_GUARD
#define WORKGROUP_GUARD
#endif
WORKGROUP_GUARD
{
  for (int v109 = get_group_id(0);v109<v_M_1;v109 = (get_num_groups(0) + v109)){
    /* iteration count is 1 or less, no loop emitted */
    {
      int v112 = get_local_id(0);
      if(get_local_id(0)<1)
      {
        float v114;
        v114 = 0.0;
        for (int v115 = 0;v115<v_N_0;v115 = (1 + v115)){
          v114 = ((v27[((v112 * v_N_0) + v115)] * v26[((v109 * v_N_0) + v115)]) + v114);
        }
        v70[v112] = v114;
      }
    }
    barrier(CLK_LOCAL_MEM_FENCE | CLK_GLOBAL_MEM_FENCE);
    /* iteration count is 1 or less, no loop emitted */
    {
      int v113 = get_local_id(0);
      if(get_local_id(0)<1)
      {
        output[v109] = ((v29 * v70[v113]) + (v28[v109] * v30));
      }
    }
    barrier(CLK_LOCAL_MEM_FENCE | CLK_GLOBAL_MEM_FENCE);
  }
  /* par for workgroup sync */
}}
