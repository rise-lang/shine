// Allocations on the host: 
// output (global): 1048576 bytes
// input (global): 4194304 bytes
// input (global): 4194304 bytes
// intermediate (global): 4194304 bytes
// length (private): 4 bytes

kernel void KERNEL(global float* output, const global float* restrict v12, const global float* restrict v13, global float* v48, int v_N_0){ 
#ifndef WORKGROUP_GUARD
#define WORKGROUP_GUARD
#endif
WORKGROUP_GUARD
{
  for (int v73 = get_group_id(0);v73<(v_N_0 / (1024));v73 = (get_num_groups(0) + v73)){
    for (int v75 = get_local_id(0);v75<256;v75 = (get_local_size(0) + v75)){
      for (int v76 = 0;v76<4;v76 = (1 + v76)){
        v48[((4 * v75) + (1024 * v73) + v76)] = (v12[((4 * v75) + (1024 * v73) + v76)] * v13[((4 * v75) + (1024 * v73) + v76)]);
      }
      float v77;
      v77 = 0.0;
      for (int v78 = 0;v78<4;v78 = (1 + v78)){
        v77 = (v48[((4 * v75) + (1024 * v73) + v78)] + v77);
      }
      output[((256 * v73) + v75)] = v77;
    }
    barrier(CLK_LOCAL_MEM_FENCE | CLK_GLOBAL_MEM_FENCE);
  }
  /* par for workgroup sync */
}}
