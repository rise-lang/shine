// Allocations on the host: 
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

kernel void KERNEL(global float* output, const global float* restrict v251, const global float* restrict v252, const global float* restrict v253, float v254, float v255, local float* v307, local float* v299, int v_M_1, int v_N_0){ 
#ifndef WORKGROUP_GUARD
#define WORKGROUP_GUARD
#endif
WORKGROUP_GUARD
{
  for (int v343 = get_group_id(0);v343<v_M_1;v343 = (get_num_groups(0) + v343)){
    for (int v345 = get_local_id(0);v345<128;v345 = (get_local_size(0) + v345)){
      float v346;
      v346 = 0.0;
      for (int v347 = 0;v347<(v_N_0 / (128));v347 = (1 + v347)){
        v346 = ((v252[((128 * v347) + v345)] * v251[((v343 * v_N_0) + (128 * v347) + v345)]) + v346);
      }
      v307[v345] = v346;
    }
    barrier(CLK_LOCAL_MEM_FENCE | CLK_GLOBAL_MEM_FENCE);
    float v348;
    v348 = 0.0;
    for (int v349 = 0;v349<128;v349 = (1 + v349)){
      v348 = (v307[v349] + v348);
    }
    v299[0] = v348;
    output[v343] = ((v299[0] * v254) + (v253[v343] * v255));
  }
  /* par for workgroup sync */
}}
