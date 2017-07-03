// output (global): 2048 bytes
// input (global): 1048576 bytes
// input (global): 2048 bytes
// input (global): 2048 bytes
// value (private): 4 bytes
// value (private): 4 bytes
// intermediate (local): 512 bytes
// intermediate (local): 4 bytes
// length (private): 4 bytes
// length (private): 4 bytes

kernel void KERNEL(global float* output, const global float* restrict v116, const global float* restrict v117, const global float* restrict v118, float v119, float v120, local float* v192, local float* v180, int v_M_1, int v_N_0){ 
#ifndef WORKGROUP_GUARD
#define WORKGROUP_GUARD
#endif
WORKGROUP_GUARD
{
  for (int v240 = get_group_id(0);v240<v_M_1;v240 = (get_num_groups(0) + v240)){
    for (int v244 = get_local_id(0);v244<128;v244 = (get_local_size(0) + v244)){
      float v247;
      v247 = 0.0;
      for (int v248 = 0;v248<(v_N_0 / (128));v248 = (1 + v248)){
        v247 = ((v117[((128 * v248) + v244)] * v116[((v240 * v_N_0) + (128 * v248) + v244)]) + v247);
      }
      v192[v244] = v247;
    }
    barrier(CLK_LOCAL_MEM_FENCE | CLK_GLOBAL_MEM_FENCE);
    /* iteration count is 1 or less, no loop emitted */
    {
      int v245 = get_local_id(0);
      if(get_local_id(0)<1)
      {
        float v249;
        v249 = 0.0;
        for (int v250 = 0;v250<128;v250 = (1 + v250)){
          v249 = (v192[((128 * v245) + v250)] + v249);
        }
        v180[v245] = v249;
      }
    }
    barrier(CLK_LOCAL_MEM_FENCE | CLK_GLOBAL_MEM_FENCE);
    /* iteration count is 1 or less, no loop emitted */
    {
      int v246 = get_local_id(0);
      if(get_local_id(0)<1)
      {
        output[v240] = ((v119 * v180[v246]) + (v118[v240] * v120));
      }
    }
    barrier(CLK_LOCAL_MEM_FENCE | CLK_GLOBAL_MEM_FENCE);
  }
  /* par for workgroup sync */
}}
