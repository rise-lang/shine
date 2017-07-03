float add(float x, float y){
  { return x+y; }
}
float id(float x){
  { return x; }
}
kernel void KERNEL(const global float* restrict v__101, global float* v__113, int v_N_31){ 
#ifndef WORKGROUP_GUARD
#define WORKGROUP_GUARD
#endif
WORKGROUP_GUARD
{
  /* Static local memory */
  local float v__107[64];
  local float v__112[32];
  local float v__106[64];
  /* Typed Value memory */
  float v__102;
  float v__108;
  /* Private Memory */
  for (int v_wg_id_91 = get_group_id(0);v_wg_id_91<(v_N_31 / (8192));v_wg_id_91 = (v_wg_id_91 + get_num_groups(0))){
    for (int v_l_id_92 = get_local_id(0);v_l_id_92<64;v_l_id_92 = (v_l_id_92 + get_local_size(0))){
      float v_tmp_133 = 0.0f;
      v__102 = v_tmp_133;
      /* reduce_seq */
      for (int v_i_93 = 0;v_i_93<128;v_i_93 = (1 + v_i_93)){
        v__102 = add(v__102, v__101[(v_i_93 + (128 * v_l_id_92) + (8192 * v_wg_id_91))]);
      }
      /* end reduce_seq */
      /* map_seq */
      /* iteration count is exactly 1, no loop emitted */
      {
        int v_i_94 = 0;
        v__106[v_l_id_92] = id(v__102);
      }
      /* end map_seq */
    }
    barrier(CLK_LOCAL_MEM_FENCE);
    
    int tv__89 = 64;
    local float *v_tin_134 = v__106;
    local float *v_tout_135 = v__107;
    for (int v_i_95 = 0;v_i_95<6;v_i_95 = (1 + v_i_95)){
      for (int v_l_id_96 = get_local_id(0);v_l_id_96<(tv__89 / (2));v_l_id_96 = (v_l_id_96 + get_local_size(0))){
        float v_tmp_136 = 0.0f;
        v__108 = v_tmp_136;
        /* reduce_seq */
        for (int v_i_97 = 0;v_i_97<2;v_i_97 = (1 + v_i_97)){
          v__108 = add(v__108, v_tin_134[(v_i_97 + (2 * v_l_id_96))]);
        }
        /* end reduce_seq */
        /* map_seq */
        /* iteration count is exactly 1, no loop emitted */
        {
          int v_i_98 = 0;
          v_tout_135[v_l_id_96] = id(v__108);
        }
        /* end map_seq */
      }
      barrier(CLK_LOCAL_MEM_FENCE);
      
      tv__89 = (tv__89 / (2));
      v_tin_134 = ( (v_tout_135 == v__107) ? v__107 : v__112 );
      v_tout_135 = ( (v_tout_135 == v__107) ? v__112 : v__107 );
      barrier(CLK_LOCAL_MEM_FENCE);
      
    }
    /* iteration count is exactly 1 or less, no loop emitted */
    if(get_local_id(0)<1)
    {
      int v_l_id_99 = get_local_id(0);
      /* map_seq */
      /* iteration count is exactly 1, no loop emitted */
      {
        int v_i_100 = 0;
        v__113[v_wg_id_91] = id(v__112[v_l_id_99]);
      }
      /* end map_seq */
    }
    barrier(CLK_GLOBAL_MEM_FENCE);
    
  }
}}
