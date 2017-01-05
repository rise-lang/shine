// Allocated (4*v_N_53) bytes for variable v__68 in global memory
// Allocated (v_N_53*1/^(512)) bytes for variable v__74 in global memory
// Kernel code:

float2 add2(float2 x, float2 y){
  { return x+y; }
}
float2 id2(float2 x){
  { return x; }
}
kernel void KERNEL(const global float* restrict v__68, global float* v__74, int v_N_53){ 
#ifndef WORKGROUP_GUARD
#define WORKGROUP_GUARD
#endif
WORKGROUP_GUARD
{
  /* Static local memory */
  /* Typed Value memory */
  float2 v__70;
  /* Private Memory */
  for (int v_wg_id_64 = get_group_id(0);v_wg_id_64<(v_N_53 / (524288));v_wg_id_64 = (v_wg_id_64 + get_num_groups(0))){
    for (int v_l_id_65 = get_local_id(0);v_l_id_65<128;v_l_id_65 = (v_l_id_65 + get_local_size(0))){
      float2 v_tmp_89 = 0.0f;
      v__70 = v_tmp_89;
      /* reduce_seq */
      for (int v_i_66 = 0;v_i_66<2048;v_i_66 = (1 + v_i_66)){
        v__70 = add2(v__70, vload2((((v_i_66 + (2048 * v_l_id_65)) / 4096) + (64 * ((v_i_66 + (2048 * v_l_id_65)) % 4096)) + (262144 * v_wg_id_64)),v__68));
      }
      /* end reduce_seq */
      /* map_seq */
      /* iteration count is exactly 1, no loop emitted */
      {
        int v_i_67 = 0;
        vstore2(id2(v__70),(v_l_id_65 + (128 * v_wg_id_64)),v__74);;
      }
      /* end map_seq */
    }
  }
}}
