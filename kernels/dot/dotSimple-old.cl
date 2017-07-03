// Allocated (4*v_N_0) bytes for variable v__56 in global memory
// Allocated (4*v_N_0) bytes for variable v__57 in global memory
// Allocated v_N_0 bytes for variable v__65 in global memory
// Allocated (4*v_N_0) bytes for variable v__61 in global memory
// Kernel code:

#ifndef Tuple_float_float_DEFINED
#define Tuple_float_float_DEFINED
typedef struct {
  float _0;
  float _1;
} Tuple_float_float;
#endif

float mult(float l, float r){
  { return l * r; }
}
float add(float x, float y){
  { return x+y; }
}
float id(float x){
  { return x; }
}
kernel void KERNEL(const global float* restrict  v__56, const global float* restrict  v__57, global float*  v__65, global float*  v__61, int v_N_0){ 
#ifndef WORKGROUP_GUARD
#define WORKGROUP_GUARD
#endif
WORKGROUP_GUARD
{
  /* Static local memory */
  /* Typed Value memory */
  float v__59;
  /* Private Memory */
  for (int v_wg_id_51 = get_group_id(0);v_wg_id_51<(v_N_0 / (1024));v_wg_id_51 = (v_wg_id_51 + get_num_groups(0))){
    for (int v_l_id_52 = get_local_id(0);v_l_id_52<256;v_l_id_52 = (v_l_id_52 + get_local_size(0))){
      float v_tmp_90 = 0.0f;
      v__59 = v_tmp_90;
      /* map_seq */
      for (int v_i_53 = 0;v_i_53<4;v_i_53 = (1 + v_i_53)){
        v__61[(v_i_53 + (4 * v_l_id_52) + (1024 * v_wg_id_51))] = mult(v__56[(v_i_53 + (4 * v_l_id_52) + (1024 * v_wg_id_51))], v__57[(v_i_53 + (4 * v_l_id_52) + (1024 * v_wg_id_51))]);
      }
      /* end map_seq */
      /* reduce_seq */
      for (int v_i_54 = 0;v_i_54<4;v_i_54 = (1 + v_i_54)){
        v__59 = add(v__59, v__61[(v_i_54 + (4 * v_l_id_52) + (1024 * v_wg_id_51))]);
      }
      /* end reduce_seq */
      /* map_seq */
      /* iteration count is exactly 1, no loop emitted */
      {
        int v_i_55 = 0;
        v__65[(v_l_id_52 + (256 * v_wg_id_51))] = id(v__59);
      }
      /* end map_seq */
    }
  }
}}
