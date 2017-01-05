// Allocated (4*v_M_1*v_N_0) bytes for variable v__85 in global memory
// Allocated (4*v_N_0) bytes for variable v__86 in global memory
// Allocated (4*v_M_1) bytes for variable v__87 in global memory
// Allocated 4 bytes for variable v__88 in private memory
// Allocated 4 bytes for variable v__89 in private memory
// Allocated (4*v_M_1) bytes for variable v__107 in global memory
// Kernel code:

#ifndef Tuple_float_float_DEFINED
#define Tuple_float_float_DEFINED
typedef struct {
  float _0;
  float _1;
} Tuple_float_float;
#endif

float multAndSumUp(float acc, float l, float r){
  { return acc + (l * r); }
}
float id(float x){
  { return x; }
}
float mult(float l, float r){
  { return l * r; }
}
float add(float x, float y){
  { return x+y; }
}
kernel void KERNEL(const global float* restrict v__85, const global float* restrict v__86, const global float* restrict v__87, float v__88, float v__89, global float* v__107, int v_M_1, int v_N_0){ 
#ifndef WORKGROUP_GUARD
#define WORKGROUP_GUARD
#endif
WORKGROUP_GUARD
{
  /* Static local memory */
  local float v__99[128];
  local float v__104[1];
  local float v__97[128];
  /* Typed Value memory */
  float v__93;
  float v__100;
  /* Private Memory */
  for (int v_wg_id_66 = get_group_id(0);v_wg_id_66<v_M_1;v_wg_id_66 = (v_wg_id_66 + get_num_groups(0))){
    for (int v_l_id_75 = get_local_id(0);v_l_id_75<128;v_l_id_75 = (v_l_id_75 + get_local_size(0))){
      float v_tmp_158 = 0.0f;
      v__93 = v_tmp_158;
      /* reduce_seq */
      for (int v_i_76 = 0;v_i_76<(v_N_0 / (128));v_i_76 = (1 + v_i_76)){
        v__93 = multAndSumUp(v__93, v__86[(v_l_id_75 + (128 * v_i_76))], v__85[(v_l_id_75 + (128 * v_i_76) + (v_N_0 * v_wg_id_66))]);
      }
      /* end reduce_seq */
      /* map_seq */
      /* iteration count is exactly 1, no loop emitted */
      {
        int v_i_77 = 0;
        v__97[v_l_id_75] = id(v__93);
      }
      /* end map_seq */
    }
    barrier(CLK_LOCAL_MEM_FENCE);
    
    for (int v_l_id_78 = get_local_id(0);v_l_id_78<128;v_l_id_78 = (v_l_id_78 + get_local_size(0))){
      /* map_seq */
      /* iteration count is exactly 1, no loop emitted */
      {
        int v_i_79 = 0;
        v__99[v_l_id_78] = mult(v__88, v__97[v_l_id_78]);
      }
      /* end map_seq */
    }
    barrier(CLK_LOCAL_MEM_FENCE);
    
    /* iteration count is exactly 1 or less, no loop emitted */
    if(get_local_id(0)<1)
    {
      int v_l_id_80 = get_local_id(0);
      float v_tmp_167 = 0.0f;
      v__100 = v_tmp_167;
      /* reduce_seq */
      for (int v_i_81 = 0;v_i_81<128;v_i_81 = (1 + v_i_81)){
        v__100 = add(v__100, v__99[(v_i_81 + (128 * v_l_id_80))]);
      }
      /* end reduce_seq */
      /* map_seq */
      /* iteration count is exactly 1, no loop emitted */
      {
        int v_i_82 = 0;
        v__104[v_l_id_80] = id(v__100);
      }
      /* end map_seq */
    }
    barrier(CLK_LOCAL_MEM_FENCE);
    
    /* iteration count is exactly 1 or less, no loop emitted */
    if(get_local_id(0)<1)
    {
      int v_l_id_83 = get_local_id(0);
      /* map_seq */
      /* iteration count is exactly 1, no loop emitted */
      {
        int v_i_84 = 0;
        v__107[v_wg_id_66] = multAndSumUp(v__104[v_l_id_83], v__87[v_wg_id_66], v__89);
      }
      /* end map_seq */
    }
    barrier(CLK_GLOBAL_MEM_FENCE);
    
  }
}}
