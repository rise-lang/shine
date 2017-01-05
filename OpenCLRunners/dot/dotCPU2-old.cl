float add(float x, float y){
  { return x+y; }
}
float id(float x){
  { return x; }
}
kernel void KERNEL(const global float* restrict v__85, global float* v__90, int v_N_0){ 
#ifndef WORKGROUP_GUARD
#define WORKGROUP_GUARD
#endif
WORKGROUP_GUARD
{
  /* Static local memory */
  /* Typed Value memory */
  float v__86;
  /* Private Memory */
  for (int v_wg_id_81 = get_group_id(0);v_wg_id_81<(v_N_0 / (128));v_wg_id_81 = (v_wg_id_81 + get_num_groups(0))){
    /* iteration count is exactly 1 or less, no loop emitted */
    if(get_local_id(0)<1)
    {
      int v_l_id_82 = get_local_id(0);
      float v_tmp_102 = 0.0f;
      v__86 = v_tmp_102;
      /* reduce_seq */
      for (int v_i_83 = 0;v_i_83<128;v_i_83 = (1 + v_i_83)){
        v__86 = add(v__86, v__85[(v_i_83 + (128 * v_wg_id_81))]);
      }
      /* end reduce_seq */
      /* map_seq */
      /* iteration count is exactly 1, no loop emitted */
      {
        int v_i_84 = 0;
        v__90[v_wg_id_81] = id(v__86);
      }
      /* end map_seq */
    }
  }
}}
