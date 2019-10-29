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
kernel void KERNEL(const global float* restrict v__186, const global float* restrict v__187, global float* v__195){
  /* Static local memory */
  local float v__189[576];
  /* Typed Value memory */
  float v__190;
  /* Private Memory */
  /* iteration count is exactly 1, no loop emitted */
  {
    int v_wg_id_178 = get_group_id(1);
    /* iteration count is exactly 1, no loop emitted */
    {
      int v_wg_id_179 = get_group_id(0);
      /* iteration count is exactly 1, no loop emitted */
      {
        int v_l_id_180 = get_local_id(1);
        for (int v_l_id_181 = get_local_id(0);v_l_id_181<144;v_l_id_181 = (16 + v_l_id_181)){
          v__189[(v_l_id_181 + (144 * v_l_id_180))] = id(v__186[((4096 * v_l_id_180) + (16384 * v_wg_id_178) + ( ((-8 + v_l_id_181 + (128 * v_wg_id_179)) >= 0) ? ( ((-8 + v_l_id_181 + (128 * v_wg_id_179)) < 4096) ? (-8 + v_l_id_181 + (128 * v_wg_id_179)) : 4095 ) : 0 ))]);
        }
        barrier(CLK_LOCAL_MEM_FENCE);

      }
      barrier(CLK_LOCAL_MEM_FENCE);

      /* iteration count is exactly 1, no loop emitted */
      {
        int v_l_id_182 = get_local_id(1);
        for (int v_l_id_183 = get_local_id(0);v_l_id_183<128;v_l_id_183 = (16 + v_l_id_183)){
          float v_tmp_199 = 0.0f;
          v__190 = v_tmp_199;
          /* reduce_seq */
          /* unroll */
          v__190 = multAndSumUp(v__190, v__189[(v_l_id_183 + (144 * v_l_id_182))], v__187[0]);
          v__190 = multAndSumUp(v__190, v__189[(1 + v_l_id_183 + (144 * v_l_id_182))], v__187[1]);
          v__190 = multAndSumUp(v__190, v__189[(2 + v_l_id_183 + (144 * v_l_id_182))], v__187[2]);
          v__190 = multAndSumUp(v__190, v__189[(3 + v_l_id_183 + (144 * v_l_id_182))], v__187[3]);
          v__190 = multAndSumUp(v__190, v__189[(4 + v_l_id_183 + (144 * v_l_id_182))], v__187[4]);
          v__190 = multAndSumUp(v__190, v__189[(5 + v_l_id_183 + (144 * v_l_id_182))], v__187[5]);
          v__190 = multAndSumUp(v__190, v__189[(6 + v_l_id_183 + (144 * v_l_id_182))], v__187[6]);
          v__190 = multAndSumUp(v__190, v__189[(7 + v_l_id_183 + (144 * v_l_id_182))], v__187[7]);
          v__190 = multAndSumUp(v__190, v__189[(8 + v_l_id_183 + (144 * v_l_id_182))], v__187[8]);
          v__190 = multAndSumUp(v__190, v__189[(9 + v_l_id_183 + (144 * v_l_id_182))], v__187[9]);
          v__190 = multAndSumUp(v__190, v__189[(10 + v_l_id_183 + (144 * v_l_id_182))], v__187[10]);
          v__190 = multAndSumUp(v__190, v__189[(11 + v_l_id_183 + (144 * v_l_id_182))], v__187[11]);
          v__190 = multAndSumUp(v__190, v__189[(12 + v_l_id_183 + (144 * v_l_id_182))], v__187[12]);
          v__190 = multAndSumUp(v__190, v__189[(13 + v_l_id_183 + (144 * v_l_id_182))], v__187[13]);
          v__190 = multAndSumUp(v__190, v__189[(14 + v_l_id_183 + (144 * v_l_id_182))], v__187[14]);
          v__190 = multAndSumUp(v__190, v__189[(15 + v_l_id_183 + (144 * v_l_id_182))], v__187[15]);
          v__190 = multAndSumUp(v__190, v__189[(16 + v_l_id_183 + (144 * v_l_id_182))], v__187[16]);
          /* end unroll */
          /* end reduce_seq */
          /* map_seq */
          /* unroll */
          v__195[(v_l_id_183 + (4096 * v_l_id_182) + (16384 * v_wg_id_178) + (128 * v_wg_id_179))] = id(v__190);
          /* end unroll */
          /* end map_seq */
        }
        barrier(CLK_GLOBAL_MEM_FENCE);

      }
      barrier(CLK_GLOBAL_MEM_FENCE);

    }
  }
}

