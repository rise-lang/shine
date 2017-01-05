// Allocations on the host: 
// output (global): 16 bytes
// input (global): 2048 bytes
// intermediate (local): 128 bytes
// intermediate (global): 128 bytes
// intermediate (global): 128 bytes
// intermediate (local): 256 bytes
// length (private): 4 bytes

kernel void KERNEL(global float* output, const global float* restrict v256, local float* v339, local float* v338, local float* v317, local float* v301, int v_N_0){ 
#ifndef WORKGROUP_GUARD
#define WORKGROUP_GUARD
#endif
WORKGROUP_GUARD
{
  for (int v368 = get_group_id(0);v368<(v_N_0 / (128));v368 = (get_num_groups(0) + v368)){
    /* iteration count is 1 or less, no loop emitted */
    {
      int v371 = get_local_id(0);
      if(get_local_id(0)<64)
      {
        float v373;
        v373 = 0.0;
        for (int v374 = 0;v374<2;v374 = (1 + v374)){
          v373 = (v256[((2 * v371) + (128 * v368) + v374)] + v373);
        }
        v317[v371] = v373;
      }
    }
    barrier(CLK_LOCAL_MEM_FENCE | CLK_GLOBAL_MEM_FENCE);
    for (int v375 = 0;v375<64;v375 = (1 + v375)){
      v338[v375] = v317[v375];
    }
    
    local float* v376 = v338;
    local float* v377 = v339;
    for (int v378 = 0;v378<6;v378 = (1 + v378)){
      for (int v381 = get_local_id(0);v381<(32 * (int)pow((float)2, (-1 * v378)));v381 = (get_local_size(0) + v381)){
        float v382;
        v382 = 0.0;
        for (int v383 = 0;v383<2;v383 = (1 + v383)){
          v382 = (v376[((2 * v381) + v383)] + v382);
        }
        v377[v381] = v382;
      }
      barrier(CLK_LOCAL_MEM_FENCE | CLK_GLOBAL_MEM_FENCE);
      local float* v384 = v376;
      v376 = v377;
      v377 = v384;
    }
    {
      /* iteration count is exactly 1, no loop emitted */
      {
        int v385 = 0;
        v301[v385] = v376[v385];
      }
      
    }
    /* iteration count is exactly 1, no loop emitted */
    {
      int v386 = 0;
      output[v368] = v301[v386];
    }
    
  }
  /* par for workgroup sync */
}}
