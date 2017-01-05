// Allocated (4*v_N_0) bytes for variable v__68 in global memory
// Allocated (v_N_0*1/^(2048)) bytes for variable v__74 in global memory
// Kernel code:

float4 absAndSumUp4(float4 acc, float4 x){
  { return acc + fabs(x); }
}
float4 id4(float4 x){
  { return x; }
}
kernel void KERNEL(const global float* restrict v__68, global float* v__74, int v_N_0){ 
#ifndef WORKGROUP_GUARD
#define WORKGROUP_GUARD
#endif
WORKGROUP_GUARD
{
  /* Static local memory */
  /* Typed Value memory */
  float4 v__70;
  /* Private Memory */
  for (int v_wg_id_64 = get_group_id(0);v_wg_id_64<(v_N_0 / (32768));v_wg_id_64 = (v_wg_id_64 + get_num_groups(0))){
    /* iteration count is exactly 1 or less, no loop emitted */
    if(get_local_id(0)<1)
    {
      int v_l_id_65 = get_local_id(0);
      float4 v_tmp_88 = 0.0f;
      v__70 = v_tmp_88;
      /* reduce_seq */
      for (int v_i_66 = 0;v_i_66<8192;v_i_66 = (1 + v_i_66)){
        v__70 = absAndSumUp4(v__70, vload4((v_i_66 + (8192 * v_wg_id_64)),v__68));
      }
      /* end reduce_seq */
      /* map_seq */
      /* iteration count is exactly 1, no loop emitted */
      {
        int v_i_67 = 0;
        vstore4(id4(v__70),v_wg_id_64,v__74);;
      }
      /* end map_seq */
    }
  }
}}
