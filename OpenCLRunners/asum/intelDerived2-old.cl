float add(float x, float y){
  { return x+y; }
}
float id(float x){
  { return x; }
}
kernel void KERNEL(const global float* restrict v__94, global float* v__99, int v_N_11){ 
#ifndef WORKGROUP_GUARD
#define WORKGROUP_GUARD
#endif
WORKGROUP_GUARD
{
  /* Static local memory */
  /* Typed Value memory */
  float v__95;
  /* Private Memory */
  for (int v_wg_id_90 = get_group_id(0);v_wg_id_90<(v_N_11 / (2048));v_wg_id_90 = (v_wg_id_90 + get_num_groups(0))){
    /* iteration count is exactly 1 or less, no loop emitted */
    if(get_local_id(0)<1)
    {
      int v_l_id_91 = get_local_id(0);
      float v_tmp_111 = 0.0f;
      v__95 = v_tmp_111;
      /* reduce_seq */
      for (int v_i_92 = 0;v_i_92<2048;v_i_92 = (1 + v_i_92)){
        v__95 = add(v__95, v__94[(v_i_92 + (2048 * v_wg_id_90))]);
      }
      /* end reduce_seq */
      /* map_seq */
      /* iteration count is exactly 1, no loop emitted */
      {
        int v_i_93 = 0;
        v__99[v_wg_id_90] = id(v__95);
      }
      /* end map_seq */
    }
  }
}}
