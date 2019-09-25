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
kernel void KERNEL(const global float* restrict v__174, const global float* restrict v__175, global float* v__183){
{
  /* Static local memory */
  local float v__177[1296];
  /* Typed Value memory */
  float v__178;
  /* Private Memory */
  /* iteration count is exactly 1, no loop emitted */
  {
    int v_wg_id_165 = get_group_id(1);
    /* iteration count is exactly 1, no loop emitted */
    {
      int v_wg_id_166 = get_group_id(0);
      /* iteration count is exactly 1, no loop emitted */
      {
        int v_l_id_167 = get_local_id(0);
        /* map_seq */
        /* unroll */
        /* iteration count is exactly 1, no loop emitted */
        {
          int v_l_id_169 = get_local_id(1);
          v__177[(v_l_id_169 + (81 * v_l_id_167))] = id(v__174[(v_l_id_167 + (16 * v_wg_id_166) + (4096 * ( ((-8 + v_l_id_169 + (64 * v_wg_id_165)) >= 0) ? ( ((-8 + v_l_id_169 + (64 * v_wg_id_165)) < 4096) ? (-8 + v_l_id_169 + (64 * v_wg_id_165)) : 4095 ) : 0 )))]);
        }
        
        /* iteration count is exactly 1, no loop emitted */
        {
          int v_l_id_169 = get_local_id(1);
          v__177[(8 + v_l_id_169 + (81 * v_l_id_167))] = id(v__174[(v_l_id_167 + (4096 * v_l_id_169) + (262144 * v_wg_id_165) + (16 * v_wg_id_166))]);
        }
        
        /* iteration count is exactly 1, no loop emitted */
        {
          int v_l_id_169 = get_local_id(1);
          v__177[(16 + v_l_id_169 + (81 * v_l_id_167))] = id(v__174[(32768 + v_l_id_167 + (4096 * v_l_id_169) + (262144 * v_wg_id_165) + (16 * v_wg_id_166))]);
        }
        
        /* iteration count is exactly 1, no loop emitted */
        {
          int v_l_id_169 = get_local_id(1);
          v__177[(24 + v_l_id_169 + (81 * v_l_id_167))] = id(v__174[(65536 + v_l_id_167 + (4096 * v_l_id_169) + (262144 * v_wg_id_165) + (16 * v_wg_id_166))]);
        }
        
        /* iteration count is exactly 1, no loop emitted */
        {
          int v_l_id_169 = get_local_id(1);
          v__177[(32 + v_l_id_169 + (81 * v_l_id_167))] = id(v__174[(98304 + v_l_id_167 + (4096 * v_l_id_169) + (262144 * v_wg_id_165) + (16 * v_wg_id_166))]);
        }
        
        /* iteration count is exactly 1, no loop emitted */
        {
          int v_l_id_169 = get_local_id(1);
          v__177[(40 + v_l_id_169 + (81 * v_l_id_167))] = id(v__174[(131072 + v_l_id_167 + (4096 * v_l_id_169) + (262144 * v_wg_id_165) + (16 * v_wg_id_166))]);
        }
        
        /* iteration count is exactly 1, no loop emitted */
        {
          int v_l_id_169 = get_local_id(1);
          v__177[(48 + v_l_id_169 + (81 * v_l_id_167))] = id(v__174[(163840 + v_l_id_167 + (4096 * v_l_id_169) + (262144 * v_wg_id_165) + (16 * v_wg_id_166))]);
        }
        
        /* iteration count is exactly 1, no loop emitted */
        {
          int v_l_id_169 = get_local_id(1);
          v__177[(56 + v_l_id_169 + (81 * v_l_id_167))] = id(v__174[(196608 + v_l_id_167 + (4096 * v_l_id_169) + (262144 * v_wg_id_165) + (16 * v_wg_id_166))]);
        }
        
        /* iteration count is exactly 1, no loop emitted */
        {
          int v_l_id_169 = get_local_id(1);
          v__177[(64 + v_l_id_169 + (81 * v_l_id_167))] = id(v__174[(229376 + v_l_id_167 + (4096 * v_l_id_169) + (262144 * v_wg_id_165) + (16 * v_wg_id_166))]);
        }
        
        /* iteration count is exactly 1, no loop emitted */
        {
          int v_l_id_169 = get_local_id(1);
          v__177[(72 + v_l_id_169 + (81 * v_l_id_167))] = id(v__174[(v_l_id_167 + (16 * v_wg_id_166) + (4096 * ( ((64 + v_l_id_169 + (64 * v_wg_id_165)) >= 0) ? ( ((64 + v_l_id_169 + (64 * v_wg_id_165)) < 4096) ? (64 + v_l_id_169 + (64 * v_wg_id_165)) : 4095 ) : 0 )))]);
        }
        
        /* end unroll */
        /* end map_seq */
      }
      barrier(CLK_LOCAL_MEM_FENCE);
      
      for (int v_l_id_170 = get_local_id(1);v_l_id_170<64;v_l_id_170 = (8 + v_l_id_170)){
        /* iteration count is exactly 1, no loop emitted */
        {
          int v_l_id_171 = get_local_id(0);
          float v_tmp_209 = 0.0f;
          v__178 = v_tmp_209;
          /* reduce_seq */
          /* unroll */
          v__178 = multAndSumUp(v__178, v__177[(v_l_id_170 + (81 * v_l_id_171))], v__175[0]);
          v__178 = multAndSumUp(v__178, v__177[(1 + v_l_id_170 + (81 * v_l_id_171))], v__175[1]);
          v__178 = multAndSumUp(v__178, v__177[(2 + v_l_id_170 + (81 * v_l_id_171))], v__175[2]);
          v__178 = multAndSumUp(v__178, v__177[(3 + v_l_id_170 + (81 * v_l_id_171))], v__175[3]);
          v__178 = multAndSumUp(v__178, v__177[(4 + v_l_id_170 + (81 * v_l_id_171))], v__175[4]);
          v__178 = multAndSumUp(v__178, v__177[(5 + v_l_id_170 + (81 * v_l_id_171))], v__175[5]);
          v__178 = multAndSumUp(v__178, v__177[(6 + v_l_id_170 + (81 * v_l_id_171))], v__175[6]);
          v__178 = multAndSumUp(v__178, v__177[(7 + v_l_id_170 + (81 * v_l_id_171))], v__175[7]);
          v__178 = multAndSumUp(v__178, v__177[(8 + v_l_id_170 + (81 * v_l_id_171))], v__175[8]);
          v__178 = multAndSumUp(v__178, v__177[(9 + v_l_id_170 + (81 * v_l_id_171))], v__175[9]);
          v__178 = multAndSumUp(v__178, v__177[(10 + v_l_id_170 + (81 * v_l_id_171))], v__175[10]);
          v__178 = multAndSumUp(v__178, v__177[(11 + v_l_id_170 + (81 * v_l_id_171))], v__175[11]);
          v__178 = multAndSumUp(v__178, v__177[(12 + v_l_id_170 + (81 * v_l_id_171))], v__175[12]);
          v__178 = multAndSumUp(v__178, v__177[(13 + v_l_id_170 + (81 * v_l_id_171))], v__175[13]);
          v__178 = multAndSumUp(v__178, v__177[(14 + v_l_id_170 + (81 * v_l_id_171))], v__175[14]);
          v__178 = multAndSumUp(v__178, v__177[(15 + v_l_id_170 + (81 * v_l_id_171))], v__175[15]);
          v__178 = multAndSumUp(v__178, v__177[(16 + v_l_id_170 + (81 * v_l_id_171))], v__175[16]);
          /* end unroll */
          /* end reduce_seq */
          /* map_seq */
          /* unroll */
          v__183[(v_l_id_171 + (4096 * v_l_id_170) + (262144 * v_wg_id_165) + (16 * v_wg_id_166))] = id(v__178);
          /* end unroll */
          /* end map_seq */
        }
      }
    }
  }
}}


