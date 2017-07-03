// Allocations on the host: 
// output (global): 16 bytes
// input (global): 2048 bytes
// intermediate (local): 128 bytes
// intermediate (global): 1024 bytes
// intermediate (global): 1024 bytes
// intermediate (local): 256 bytes
// length (private): 4 bytes

kernel void KERNEL(global float* output, const global float* restrict v256, local float* v305, global float* v339, global float* v338, local float* v317, int v_N_0){ 
#ifndef WORKGROUP_GUARD
#define WORKGROUP_GUARD
#endif
WORKGROUP_GUARD
{
  for (int v370 = get_group_id(0);v370<(v_N_0 / (128));v370 = (get_num_groups(0) + v370)){
    /* iteration count is 1 or less, no loop emitted */
    {
      int v373 = get_local_id(0);
      if(get_local_id(0)<64)
      {
        float v375;
        v375 = 0.0;
        for (int v376 = 0;v376<2;v376 = (1 + v376)){
          v375 = (v256[((2 * v373) + (128 * v370) + v376)] + v375);
        }
        v317[v373] = v375;
      }
    }
    barrier(CLK_LOCAL_MEM_FENCE | CLK_GLOBAL_MEM_FENCE);
    for (int v377 = 0;v377<64;v377 = (1 + v377)){
      v338[((64 * v370) + v377)] = v317[v377];
    }
    
    global float* v378 = v338;
    global float* v379 = v339;
    for (int v380 = 0;v380<6;v380 = (1 + v380)){
      for (int v383 = get_local_id(0);v383<(32 * (int)pow((float)2, (-1 * v380)));v383 = (get_local_size(0) + v383)){
        float v384;
        v384 = 0.0;
        for (int v385 = 0;v385<2;v385 = (1 + v385)){
          v384 = (v378[((2 * v383) + v385)] + v384);
        }
        v305[v383] = v384;
      }
      barrier(CLK_LOCAL_MEM_FENCE | CLK_GLOBAL_MEM_FENCE);
      for (int v386 = 0;v386<(32 * (int)pow((float)2, (-1 * v380)));v386 = (1 + v386)){
        v379[v386] = v305[v386];
      }
      
      global float* v387 = v378;
      v378 = v379;
      v379 = v387;
    }
    {
      /* iteration count is exactly 1, no loop emitted */
      {
        int v388 = 0;
        output[v370] = v378[v388];
      }
      
    }
  }
  /* par for workgroup sync */
}}
