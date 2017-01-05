float add(float x, float y){
  { return x+y; }
}
float id(float x){
  { return x; }
}
kernel void KERNEL(const global float* restrict v__94, global float* v__106, int v_N_0){ 
#ifndef WORKGROUP_GUARD
#define WORKGROUP_GUARD
#endif
WORKGROUP_GUARD
{
  /* Static local memory */
  local float v__100[64];
  local float v__105[32];
  local float v__99[64];
  /* Typed Value memory */
  float v__95;
  float v__101;
  /* Private Memory */
  for (int v_wg_id_84 = get_group_id(0);v_wg_id_84<(v_N_0 / (128));v_wg_id_84 = (v_wg_id_84 + get_num_groups(0))){
    for (int v_l_id_85 = get_local_id(0);v_l_id_85<64;v_l_id_85 = (v_l_id_85 + get_local_size(0))){
      float v_tmp_126 = 0.0f;
      v__95 = v_tmp_126;
      /* reduce_seq */
      for (int v_i_86 = 0;v_i_86<2;v_i_86 = (1 + v_i_86)){
        v__95 = add(v__95, v__94[(v_i_86 + (2 * v_l_id_85) + (128 * v_wg_id_84))]);
      }
      /* end reduce_seq */
      /* map_seq */
      /* iteration count is exactly 1, no loop emitted */
      {
        int v_i_87 = 0;
        v__99[v_l_id_85] = id(v__95);
      }
      /* end map_seq */
    }
    barrier(CLK_LOCAL_MEM_FENCE);
    
    int tv__82 = 64;
    local float *v_tin_127 = v__99;
    local float *v_tout_128 = v__100;
    for (int v_i_88 = 0;v_i_88<6;v_i_88 = (1 + v_i_88)){
      for (int v_l_id_89 = get_local_id(0);v_l_id_89<(tv__82 / (2));v_l_id_89 = (v_l_id_89 + get_local_size(0))){
        float v_tmp_129 = 0.0f;
        v__101 = v_tmp_129;
        /* reduce_seq */
        for (int v_i_90 = 0;v_i_90<2;v_i_90 = (1 + v_i_90)){
          v__101 = add(v__101, v_tin_127[(v_i_90 + (2 * v_l_id_89))]);
        }
        /* end reduce_seq */
        /* map_seq */
        /* iteration count is exactly 1, no loop emitted */
        {
          int v_i_91 = 0;
          v_tout_128[v_l_id_89] = id(v__101);
        }
        /* end map_seq */
      }
      barrier(CLK_LOCAL_MEM_FENCE);
      
      tv__82 = (tv__82 / (2));
      v_tin_127 = ( (v_tout_128 == v__100) ? v__100 : v__105 );
      v_tout_128 = ( (v_tout_128 == v__100) ? v__105 : v__100 );
      barrier(CLK_LOCAL_MEM_FENCE);
      
    }
    /* iteration count is exactly 1 or less, no loop emitted */
    if(get_local_id(0)<1)
    {
      int v_l_id_92 = get_local_id(0);
      /* map_seq */
      /* iteration count is exactly 1, no loop emitted */
      {
        int v_i_93 = 0;
        v__106[v_wg_id_84] = id(v__105[v_l_id_92]);
      }
      /* end map_seq */
    }
    barrier(CLK_GLOBAL_MEM_FENCE);
    
  }
}}
