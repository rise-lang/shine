// Allocated (4*v_M_1*v_N_0) bytes for variable v__79 in global memory
// Allocated (4*v_N_0) bytes for variable v__80 in global memory
// Allocated (4*v_M_1) bytes for variable v__81 in global memory
// Allocated 4 bytes for variable v__82 in private memory
// Allocated 4 bytes for variable v__83 in private memory
// Allocated (4*v_M_1) bytes for variable v__96 in global memory
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
kernel void KERNEL(const global float* restrict v__79, const global float* restrict v__80, const global float* restrict v__81, float v__82, float v__83, global float* v__96, int v_M_1, int v_N_0){ 
#ifndef WORKGROUP_GUARD
#define WORKGROUP_GUARD
#endif
WORKGROUP_GUARD
{
  /* Static local memory */
  local float v__93[1];
  local float v__91[1];
  /* Typed Value memory */
  float v__87;
  /* Private Memory */
  for (int v_wg_id_66 = get_group_id(0);v_wg_id_66<v_M_1;v_wg_id_66 = (v_wg_id_66 + get_num_groups(0))){
    /* iteration count is exactly 1 or less, no loop emitted */
    if(get_local_id(0)<1)
    {
      int v_l_id_72 = get_local_id(0);
      float v_tmp_137 = 0.0f;
      v__87 = v_tmp_137;
      /* reduce_seq */
      for (int v_i_73 = 0;v_i_73<v_N_0;v_i_73 = (1 + v_i_73)){
        v__87 = multAndSumUp(v__87, v__80[(v_i_73 + (v_N_0 * v_l_id_72))], v__79[(v_i_73 + (v_N_0 * v_wg_id_66))]);
      }
      /* end reduce_seq */
      /* map_seq */
      /* iteration count is exactly 1, no loop emitted */
      {
        int v_i_74 = 0;
        v__91[v_l_id_72] = id(v__87);
      }
      /* end map_seq */
    }
    barrier(CLK_LOCAL_MEM_FENCE);
    
    /* iteration count is exactly 1 or less, no loop emitted */
    if(get_local_id(0)<1)
    {
      int v_l_id_75 = get_local_id(0);
      /* map_seq */
      /* iteration count is exactly 1, no loop emitted */
      {
        int v_i_76 = 0;
        v__93[v_l_id_75] = mult(v__82, v__91[v_l_id_75]);
      }
      /* end map_seq */
    }
    barrier(CLK_LOCAL_MEM_FENCE);
    
    /* iteration count is exactly 1 or less, no loop emitted */
    if(get_local_id(0)<1)
    {
      int v_l_id_77 = get_local_id(0);
      /* map_seq */
      /* iteration count is exactly 1, no loop emitted */
      {
        int v_i_78 = 0;
        v__96[v_wg_id_66] = multAndSumUp(v__93[v_l_id_77], v__81[v_wg_id_66], v__83);
      }
      /* end map_seq */
    }
    barrier(CLK_GLOBAL_MEM_FENCE);
    
  }
}}
