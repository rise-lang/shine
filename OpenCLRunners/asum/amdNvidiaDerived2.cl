// Allocations on the host: 
// output (global): 512 bytes
// input (global): 4194304 bytes
// intermediate (local): 256 bytes
// intermediate (local): 256 bytes
// intermediate (local): 256 bytes
// intermediate (local): 4 bytes
// length (private): 4 bytes

kernel void KERNEL(global float* output, const global float* restrict v181, local float* v264, local float* v263, local float* v242, local float* v226, int v_N_0){ 
#ifndef WORKGROUP_GUARD
#define WORKGROUP_GUARD
#endif
WORKGROUP_GUARD
{
  for (int v293 = get_group_id(0);v293<(v_N_0 / (8192));v293 = (get_num_groups(0) + v293)){
    /* iteration count is 1 or less, no loop emitted */
    {
      int v296 = get_local_id(0);
      if(get_local_id(0)<64)
      {
        float v298;
        v298 = 0.0;
        for (int v299 = 0;v299<128;v299 = (1 + v299)){
          v298 = (v181[((128 * v296) + (8192 * v293) + v299)] + v298);
        }
        v242[v296] = v298;
      }
    }
    barrier(CLK_LOCAL_MEM_FENCE | CLK_GLOBAL_MEM_FENCE);
    for (int v300 = 0;v300<64;v300 = (1 + v300)){
      v263[v300] = v242[v300];
    }
    
    local float* v301 = v263;
    local float* v302 = v264;
    for (int v303 = 0;v303<6;v303 = (1 + v303)){
      for (int v306 = get_local_id(0);v306<(32 * (int)pow((float)2, (-1 * v303)));v306 = (get_local_size(0) + v306)){
        float v307;
        v307 = 0.0;
        for (int v308 = 0;v308<2;v308 = (1 + v308)){
          v307 = (v301[((2 * v306) + v308)] + v307);
        }
        v302[v306] = v307;
      }
      barrier(CLK_LOCAL_MEM_FENCE | CLK_GLOBAL_MEM_FENCE);
      local float* v309 = v301;
      v301 = v302;
      v302 = v309;
    }
    {
      /* iteration count is exactly 1, no loop emitted */
      {
        int v310 = 0;
        v226[v310] = v301[v310];
      }
      
    }
    /* iteration count is exactly 1, no loop emitted */
    {
      int v311 = 0;
      output[v293] = v226[v311];
    }
    
  }
  /* par for workgroup sync */
}}
