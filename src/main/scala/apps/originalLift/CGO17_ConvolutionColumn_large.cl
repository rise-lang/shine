#ifndef Tuple_float_float_DEFINED
#define Tuple_float_float_DEFINED
typedef struct {
  float _0;
  float _1;
} Tuple_float_float;
#endif

float id(float x){
  { return x; }
}
float multAndSumUp(float acc, float l, float r){
  { return acc + (l * r); }
}
kernel void KERNEL(const global float* restrict v__144, const global float* restrict v__145, global float* v__152){
  /* Static local memory */
  local float v__146[576];
  /* Typed Value memory */
  float v__147;
  /* Private Memory */
  /* iteration count is exactly 1, no loop emitted */
  {
    int v_wg_id_136 = get_group_id(1);
    /* iteration count is exactly 1, no loop emitted */
    {
      int v_wg_id_137 = get_group_id(0);
      /* iteration count is exactly 1, no loop emitted */
      {
        int v_l_id_138 = get_local_id(1);
        for (int v_l_id_139 = get_local_id(0);v_l_id_139<144;v_l_id_139 = (16 + v_l_id_139)){
          v__146[(v_l_id_139 + (144 * v_l_id_138))] = id(v__144[((8192 * v_l_id_138) + (32768 * v_wg_id_136) + ( ((-8 + v_l_id_139 + (128 * v_wg_id_137)) >= 0) ? ( ((-8 + v_l_id_139 + (128 * v_wg_id_137)) < 8192) ? (-8 + v_l_id_139 + (128 * v_wg_id_137)) : 8191 ) : 0 ))]);
        }
      }
      barrier(CLK_LOCAL_MEM_FENCE);
      
      /* iteration count is exactly 1, no loop emitted */
      {
        int v_l_id_140 = get_local_id(1);
        for (int v_l_id_141 = get_local_id(0);v_l_id_141<128;v_l_id_141 = (16 + v_l_id_141)){
          float v_tmp_171 = 0.0f;
          v__147 = v_tmp_171;
          /* reduce_seq */
          /* unroll */
          v__147 = multAndSumUp(v__147, v__146[(v_l_id_141 + (144 * v_l_id_140))], v__145[0]);
          v__147 = multAndSumUp(v__147, v__146[(1 + v_l_id_141 + (144 * v_l_id_140))], v__145[1]);
          v__147 = multAndSumUp(v__147, v__146[(2 + v_l_id_141 + (144 * v_l_id_140))], v__145[2]);
          v__147 = multAndSumUp(v__147, v__146[(3 + v_l_id_141 + (144 * v_l_id_140))], v__145[3]);
          v__147 = multAndSumUp(v__147, v__146[(4 + v_l_id_141 + (144 * v_l_id_140))], v__145[4]);
          v__147 = multAndSumUp(v__147, v__146[(5 + v_l_id_141 + (144 * v_l_id_140))], v__145[5]);
          v__147 = multAndSumUp(v__147, v__146[(6 + v_l_id_141 + (144 * v_l_id_140))], v__145[6]);
          v__147 = multAndSumUp(v__147, v__146[(7 + v_l_id_141 + (144 * v_l_id_140))], v__145[7]);
          v__147 = multAndSumUp(v__147, v__146[(8 + v_l_id_141 + (144 * v_l_id_140))], v__145[8]);
          v__147 = multAndSumUp(v__147, v__146[(9 + v_l_id_141 + (144 * v_l_id_140))], v__145[9]);
          v__147 = multAndSumUp(v__147, v__146[(10 + v_l_id_141 + (144 * v_l_id_140))], v__145[10]);
          v__147 = multAndSumUp(v__147, v__146[(11 + v_l_id_141 + (144 * v_l_id_140))], v__145[11]);
          v__147 = multAndSumUp(v__147, v__146[(12 + v_l_id_141 + (144 * v_l_id_140))], v__145[12]);
          v__147 = multAndSumUp(v__147, v__146[(13 + v_l_id_141 + (144 * v_l_id_140))], v__145[13]);
          v__147 = multAndSumUp(v__147, v__146[(14 + v_l_id_141 + (144 * v_l_id_140))], v__145[14]);
          v__147 = multAndSumUp(v__147, v__146[(15 + v_l_id_141 + (144 * v_l_id_140))], v__145[15]);
          v__147 = multAndSumUp(v__147, v__146[(16 + v_l_id_141 + (144 * v_l_id_140))], v__145[16]);
          /* end unroll */
          /* end reduce_seq */
          /* map_seq */
          /* unroll */
          v__152[(v_l_id_141 + (8192 * v_l_id_140) + (32768 * v_wg_id_136) + (128 * v_wg_id_137))] = id(v__147);
          /* end unroll */
          /* end map_seq */
        }
      }
    }
  }
}

