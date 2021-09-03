struct Record_8_float_4_float {
  float _fst[8];
  float _snd[4];
};

struct Record_16_64_float_16_128_float {
  float _fst[1024];
  float _snd[2048];
};


__kernel __attribute__ ((reqd_work_group_size(32, 8, 1)))
void KERNEL(global float* restrict output, int n868, int n869, int n870, const global float* restrict e871, const global float* restrict e872, local struct Record_16_64_float_16_128_float* restrict x1874){
  /* Start of moved local vars */
  /* End of moved local vars */
  /* mapWorkGroup */
  /* iteration count is exactly 1, no loop emitted */
  int wg_id_1970 = get_group_id(1);
  /* mapWorkGroup */
  /* iteration count is exactly 1, no loop emitted */
  int wg_id_1971 = get_group_id(0);
  /* oclReduceSeq */
  {
    float x1903[32];
    /* mapLocal */
    /* unrolling loop of 1 */
    /* mapLocal */
    /* unrolling loop of 1 */
    /* mapSeq */
    /* unrolling loop of 8 */
    /* mapSeq */
    /* unrolling loop of 4 */
    x1903[0] = 0.0f;
    x1903[1] = 0.0f;
    x1903[2] = 0.0f;
    x1903[3] = 0.0f;
    /* mapSeq */
    /* unrolling loop of 4 */
    x1903[4] = 0.0f;
    x1903[5] = 0.0f;
    x1903[6] = 0.0f;
    x1903[7] = 0.0f;
    /* mapSeq */
    /* unrolling loop of 4 */
    x1903[8] = 0.0f;
    x1903[9] = 0.0f;
    x1903[10] = 0.0f;
    x1903[11] = 0.0f;
    /* mapSeq */
    /* unrolling loop of 4 */
    x1903[12] = 0.0f;
    x1903[13] = 0.0f;
    x1903[14] = 0.0f;
    x1903[15] = 0.0f;
    /* mapSeq */
    /* unrolling loop of 4 */
    x1903[16] = 0.0f;
    x1903[17] = 0.0f;
    x1903[18] = 0.0f;
    x1903[19] = 0.0f;
    /* mapSeq */
    /* unrolling loop of 4 */
    x1903[20] = 0.0f;
    x1903[21] = 0.0f;
    x1903[22] = 0.0f;
    x1903[23] = 0.0f;
    /* mapSeq */
    /* unrolling loop of 4 */
    x1903[24] = 0.0f;
    x1903[25] = 0.0f;
    x1903[26] = 0.0f;
    x1903[27] = 0.0f;
    /* mapSeq */
    /* unrolling loop of 4 */
    x1903[28] = 0.0f;
    x1903[29] = 0.0f;
    x1903[30] = 0.0f;
    x1903[31] = 0.0f;
    for (int i_1972 = 0; i_1972 < (n870 / 16); i_1972 = 1 + i_1972) {
      /* mapLocal */
      /* iteration count is exactly 1, no loop emitted */
      int l_id_1973 = get_local_id(1);
      /* mapLocal */
      /* iteration count is exactly 1, no loop emitted */
      int l_id_1974 = get_local_id(0);
      vstore4(vload4(0, &e871[(((((4 * l_id_1974) % 64) + ((2 * l_id_1973) * n868)) + ((16 * i_1972) * n868)) + (64 * wg_id_1970)) + (n868 * (l_id_1974 / 16))]), 0, &x1874[0]._fst[(4 * l_id_1974) + (128 * l_id_1973)]);
      /* mapLocal */
      for (int l_id_1975 = get_local_id(1); l_id_1975 < 16; l_id_1975 = 8 + l_id_1975) {
        /* mapLocal */
        /* iteration count is exactly 1, no loop emitted */
        int l_id_1976 = get_local_id(0);
        vstore4(vload4(0, &e872[(((4 * l_id_1976) + ((16 * i_1972) * n869)) + (128 * wg_id_1971)) + (l_id_1975 * n869)]), 0, &x1874[0]._snd[(4 * l_id_1976) + (128 * l_id_1975)]);
      }

      barrier(CLK_LOCAL_MEM_FENCE);
      /* mapLocal */
      /* unrolling loop of 1 */
      /* mapLocal */
      /* unrolling loop of 1 */
      /* oclReduceSeq */
      {
        float x1837[32];
        /* mapSeq */
        /* unrolling loop of 8 */
        /* mapSeq */
        /* unrolling loop of 4 */
        x1837[0] = x1903[0];
        x1837[1] = x1903[1];
        x1837[2] = x1903[2];
        x1837[3] = x1903[3];
        /* mapSeq */
        /* unrolling loop of 4 */
        x1837[4] = x1903[4];
        x1837[5] = x1903[5];
        x1837[6] = x1903[6];
        x1837[7] = x1903[7];
        /* mapSeq */
        /* unrolling loop of 4 */
        x1837[8] = x1903[8];
        x1837[9] = x1903[9];
        x1837[10] = x1903[10];
        x1837[11] = x1903[11];
        /* mapSeq */
        /* unrolling loop of 4 */
        x1837[12] = x1903[12];
        x1837[13] = x1903[13];
        x1837[14] = x1903[14];
        x1837[15] = x1903[15];
        /* mapSeq */
        /* unrolling loop of 4 */
        x1837[16] = x1903[16];
        x1837[17] = x1903[17];
        x1837[18] = x1903[18];
        x1837[19] = x1903[19];
        /* mapSeq */
        /* unrolling loop of 4 */
        x1837[20] = x1903[20];
        x1837[21] = x1903[21];
        x1837[22] = x1903[22];
        x1837[23] = x1903[23];
        /* mapSeq */
        /* unrolling loop of 4 */
        x1837[24] = x1903[24];
        x1837[25] = x1903[25];
        x1837[26] = x1903[26];
        x1837[27] = x1903[27];
        /* mapSeq */
        /* unrolling loop of 4 */
        x1837[28] = x1903[28];
        x1837[29] = x1903[29];
        x1837[30] = x1903[30];
        x1837[31] = x1903[31];
        /* unrolling loop of 16 */
        {
          struct Record_8_float_4_float x1824;
          /* mapSeq */
          /* unrolling loop of 8 */
          x1824._fst[0] = x1874[0]._fst[8 * get_local_id(1)];
          x1824._fst[1] = x1874[0]._fst[1 + (8 * get_local_id(1))];
          x1824._fst[2] = x1874[0]._fst[2 + (8 * get_local_id(1))];
          x1824._fst[3] = x1874[0]._fst[3 + (8 * get_local_id(1))];
          x1824._fst[4] = x1874[0]._fst[4 + (8 * get_local_id(1))];
          x1824._fst[5] = x1874[0]._fst[5 + (8 * get_local_id(1))];
          x1824._fst[6] = x1874[0]._fst[6 + (8 * get_local_id(1))];
          x1824._fst[7] = x1874[0]._fst[7 + (8 * get_local_id(1))];
          /* mapSeq */
          /* unrolling loop of 4 */
          x1824._snd[0] = x1874[0]._snd[4 * get_local_id(0)];
          x1824._snd[1] = x1874[0]._snd[1 + (4 * get_local_id(0))];
          x1824._snd[2] = x1874[0]._snd[2 + (4 * get_local_id(0))];
          x1824._snd[3] = x1874[0]._snd[3 + (4 * get_local_id(0))];
          /* mapSeq */
          /* unrolling loop of 8 */
          /* mapSeq */
          /* unrolling loop of 4 */
          x1837[0] = x1837[0] + (x1824._fst[0] * x1824._snd[0]);
          x1837[1] = x1837[1] + (x1824._fst[0] * x1824._snd[1]);
          x1837[2] = x1837[2] + (x1824._fst[0] * x1824._snd[2]);
          x1837[3] = x1837[3] + (x1824._fst[0] * x1824._snd[3]);
          /* mapSeq */
          /* unrolling loop of 4 */
          x1837[4] = x1837[4] + (x1824._fst[1] * x1824._snd[0]);
          x1837[5] = x1837[5] + (x1824._fst[1] * x1824._snd[1]);
          x1837[6] = x1837[6] + (x1824._fst[1] * x1824._snd[2]);
          x1837[7] = x1837[7] + (x1824._fst[1] * x1824._snd[3]);
          /* mapSeq */
          /* unrolling loop of 4 */
          x1837[8] = x1837[8] + (x1824._fst[2] * x1824._snd[0]);
          x1837[9] = x1837[9] + (x1824._fst[2] * x1824._snd[1]);
          x1837[10] = x1837[10] + (x1824._fst[2] * x1824._snd[2]);
          x1837[11] = x1837[11] + (x1824._fst[2] * x1824._snd[3]);
          /* mapSeq */
          /* unrolling loop of 4 */
          x1837[12] = x1837[12] + (x1824._fst[3] * x1824._snd[0]);
          x1837[13] = x1837[13] + (x1824._fst[3] * x1824._snd[1]);
          x1837[14] = x1837[14] + (x1824._fst[3] * x1824._snd[2]);
          x1837[15] = x1837[15] + (x1824._fst[3] * x1824._snd[3]);
          /* mapSeq */
          /* unrolling loop of 4 */
          x1837[16] = x1837[16] + (x1824._fst[4] * x1824._snd[0]);
          x1837[17] = x1837[17] + (x1824._fst[4] * x1824._snd[1]);
          x1837[18] = x1837[18] + (x1824._fst[4] * x1824._snd[2]);
          x1837[19] = x1837[19] + (x1824._fst[4] * x1824._snd[3]);
          /* mapSeq */
          /* unrolling loop of 4 */
          x1837[20] = x1837[20] + (x1824._fst[5] * x1824._snd[0]);
          x1837[21] = x1837[21] + (x1824._fst[5] * x1824._snd[1]);
          x1837[22] = x1837[22] + (x1824._fst[5] * x1824._snd[2]);
          x1837[23] = x1837[23] + (x1824._fst[5] * x1824._snd[3]);
          /* mapSeq */
          /* unrolling loop of 4 */
          x1837[24] = x1837[24] + (x1824._fst[6] * x1824._snd[0]);
          x1837[25] = x1837[25] + (x1824._fst[6] * x1824._snd[1]);
          x1837[26] = x1837[26] + (x1824._fst[6] * x1824._snd[2]);
          x1837[27] = x1837[27] + (x1824._fst[6] * x1824._snd[3]);
          /* mapSeq */
          /* unrolling loop of 4 */
          x1837[28] = x1837[28] + (x1824._fst[7] * x1824._snd[0]);
          x1837[29] = x1837[29] + (x1824._fst[7] * x1824._snd[1]);
          x1837[30] = x1837[30] + (x1824._fst[7] * x1824._snd[2]);
          x1837[31] = x1837[31] + (x1824._fst[7] * x1824._snd[3]);
        }

        {
          struct Record_8_float_4_float x1824;
          /* mapSeq */
          /* unrolling loop of 8 */
          x1824._fst[0] = x1874[0]._fst[64 + (8 * get_local_id(1))];
          x1824._fst[1] = x1874[0]._fst[65 + (8 * get_local_id(1))];
          x1824._fst[2] = x1874[0]._fst[66 + (8 * get_local_id(1))];
          x1824._fst[3] = x1874[0]._fst[67 + (8 * get_local_id(1))];
          x1824._fst[4] = x1874[0]._fst[68 + (8 * get_local_id(1))];
          x1824._fst[5] = x1874[0]._fst[69 + (8 * get_local_id(1))];
          x1824._fst[6] = x1874[0]._fst[70 + (8 * get_local_id(1))];
          x1824._fst[7] = x1874[0]._fst[71 + (8 * get_local_id(1))];
          /* mapSeq */
          /* unrolling loop of 4 */
          x1824._snd[0] = x1874[0]._snd[128 + (4 * get_local_id(0))];
          x1824._snd[1] = x1874[0]._snd[129 + (4 * get_local_id(0))];
          x1824._snd[2] = x1874[0]._snd[130 + (4 * get_local_id(0))];
          x1824._snd[3] = x1874[0]._snd[131 + (4 * get_local_id(0))];
          /* mapSeq */
          /* unrolling loop of 8 */
          /* mapSeq */
          /* unrolling loop of 4 */
          x1837[0] = x1837[0] + (x1824._fst[0] * x1824._snd[0]);
          x1837[1] = x1837[1] + (x1824._fst[0] * x1824._snd[1]);
          x1837[2] = x1837[2] + (x1824._fst[0] * x1824._snd[2]);
          x1837[3] = x1837[3] + (x1824._fst[0] * x1824._snd[3]);
          /* mapSeq */
          /* unrolling loop of 4 */
          x1837[4] = x1837[4] + (x1824._fst[1] * x1824._snd[0]);
          x1837[5] = x1837[5] + (x1824._fst[1] * x1824._snd[1]);
          x1837[6] = x1837[6] + (x1824._fst[1] * x1824._snd[2]);
          x1837[7] = x1837[7] + (x1824._fst[1] * x1824._snd[3]);
          /* mapSeq */
          /* unrolling loop of 4 */
          x1837[8] = x1837[8] + (x1824._fst[2] * x1824._snd[0]);
          x1837[9] = x1837[9] + (x1824._fst[2] * x1824._snd[1]);
          x1837[10] = x1837[10] + (x1824._fst[2] * x1824._snd[2]);
          x1837[11] = x1837[11] + (x1824._fst[2] * x1824._snd[3]);
          /* mapSeq */
          /* unrolling loop of 4 */
          x1837[12] = x1837[12] + (x1824._fst[3] * x1824._snd[0]);
          x1837[13] = x1837[13] + (x1824._fst[3] * x1824._snd[1]);
          x1837[14] = x1837[14] + (x1824._fst[3] * x1824._snd[2]);
          x1837[15] = x1837[15] + (x1824._fst[3] * x1824._snd[3]);
          /* mapSeq */
          /* unrolling loop of 4 */
          x1837[16] = x1837[16] + (x1824._fst[4] * x1824._snd[0]);
          x1837[17] = x1837[17] + (x1824._fst[4] * x1824._snd[1]);
          x1837[18] = x1837[18] + (x1824._fst[4] * x1824._snd[2]);
          x1837[19] = x1837[19] + (x1824._fst[4] * x1824._snd[3]);
          /* mapSeq */
          /* unrolling loop of 4 */
          x1837[20] = x1837[20] + (x1824._fst[5] * x1824._snd[0]);
          x1837[21] = x1837[21] + (x1824._fst[5] * x1824._snd[1]);
          x1837[22] = x1837[22] + (x1824._fst[5] * x1824._snd[2]);
          x1837[23] = x1837[23] + (x1824._fst[5] * x1824._snd[3]);
          /* mapSeq */
          /* unrolling loop of 4 */
          x1837[24] = x1837[24] + (x1824._fst[6] * x1824._snd[0]);
          x1837[25] = x1837[25] + (x1824._fst[6] * x1824._snd[1]);
          x1837[26] = x1837[26] + (x1824._fst[6] * x1824._snd[2]);
          x1837[27] = x1837[27] + (x1824._fst[6] * x1824._snd[3]);
          /* mapSeq */
          /* unrolling loop of 4 */
          x1837[28] = x1837[28] + (x1824._fst[7] * x1824._snd[0]);
          x1837[29] = x1837[29] + (x1824._fst[7] * x1824._snd[1]);
          x1837[30] = x1837[30] + (x1824._fst[7] * x1824._snd[2]);
          x1837[31] = x1837[31] + (x1824._fst[7] * x1824._snd[3]);
        }

        {
          struct Record_8_float_4_float x1824;
          /* mapSeq */
          /* unrolling loop of 8 */
          x1824._fst[0] = x1874[0]._fst[128 + (8 * get_local_id(1))];
          x1824._fst[1] = x1874[0]._fst[129 + (8 * get_local_id(1))];
          x1824._fst[2] = x1874[0]._fst[130 + (8 * get_local_id(1))];
          x1824._fst[3] = x1874[0]._fst[131 + (8 * get_local_id(1))];
          x1824._fst[4] = x1874[0]._fst[132 + (8 * get_local_id(1))];
          x1824._fst[5] = x1874[0]._fst[133 + (8 * get_local_id(1))];
          x1824._fst[6] = x1874[0]._fst[134 + (8 * get_local_id(1))];
          x1824._fst[7] = x1874[0]._fst[135 + (8 * get_local_id(1))];
          /* mapSeq */
          /* unrolling loop of 4 */
          x1824._snd[0] = x1874[0]._snd[256 + (4 * get_local_id(0))];
          x1824._snd[1] = x1874[0]._snd[257 + (4 * get_local_id(0))];
          x1824._snd[2] = x1874[0]._snd[258 + (4 * get_local_id(0))];
          x1824._snd[3] = x1874[0]._snd[259 + (4 * get_local_id(0))];
          /* mapSeq */
          /* unrolling loop of 8 */
          /* mapSeq */
          /* unrolling loop of 4 */
          x1837[0] = x1837[0] + (x1824._fst[0] * x1824._snd[0]);
          x1837[1] = x1837[1] + (x1824._fst[0] * x1824._snd[1]);
          x1837[2] = x1837[2] + (x1824._fst[0] * x1824._snd[2]);
          x1837[3] = x1837[3] + (x1824._fst[0] * x1824._snd[3]);
          /* mapSeq */
          /* unrolling loop of 4 */
          x1837[4] = x1837[4] + (x1824._fst[1] * x1824._snd[0]);
          x1837[5] = x1837[5] + (x1824._fst[1] * x1824._snd[1]);
          x1837[6] = x1837[6] + (x1824._fst[1] * x1824._snd[2]);
          x1837[7] = x1837[7] + (x1824._fst[1] * x1824._snd[3]);
          /* mapSeq */
          /* unrolling loop of 4 */
          x1837[8] = x1837[8] + (x1824._fst[2] * x1824._snd[0]);
          x1837[9] = x1837[9] + (x1824._fst[2] * x1824._snd[1]);
          x1837[10] = x1837[10] + (x1824._fst[2] * x1824._snd[2]);
          x1837[11] = x1837[11] + (x1824._fst[2] * x1824._snd[3]);
          /* mapSeq */
          /* unrolling loop of 4 */
          x1837[12] = x1837[12] + (x1824._fst[3] * x1824._snd[0]);
          x1837[13] = x1837[13] + (x1824._fst[3] * x1824._snd[1]);
          x1837[14] = x1837[14] + (x1824._fst[3] * x1824._snd[2]);
          x1837[15] = x1837[15] + (x1824._fst[3] * x1824._snd[3]);
          /* mapSeq */
          /* unrolling loop of 4 */
          x1837[16] = x1837[16] + (x1824._fst[4] * x1824._snd[0]);
          x1837[17] = x1837[17] + (x1824._fst[4] * x1824._snd[1]);
          x1837[18] = x1837[18] + (x1824._fst[4] * x1824._snd[2]);
          x1837[19] = x1837[19] + (x1824._fst[4] * x1824._snd[3]);
          /* mapSeq */
          /* unrolling loop of 4 */
          x1837[20] = x1837[20] + (x1824._fst[5] * x1824._snd[0]);
          x1837[21] = x1837[21] + (x1824._fst[5] * x1824._snd[1]);
          x1837[22] = x1837[22] + (x1824._fst[5] * x1824._snd[2]);
          x1837[23] = x1837[23] + (x1824._fst[5] * x1824._snd[3]);
          /* mapSeq */
          /* unrolling loop of 4 */
          x1837[24] = x1837[24] + (x1824._fst[6] * x1824._snd[0]);
          x1837[25] = x1837[25] + (x1824._fst[6] * x1824._snd[1]);
          x1837[26] = x1837[26] + (x1824._fst[6] * x1824._snd[2]);
          x1837[27] = x1837[27] + (x1824._fst[6] * x1824._snd[3]);
          /* mapSeq */
          /* unrolling loop of 4 */
          x1837[28] = x1837[28] + (x1824._fst[7] * x1824._snd[0]);
          x1837[29] = x1837[29] + (x1824._fst[7] * x1824._snd[1]);
          x1837[30] = x1837[30] + (x1824._fst[7] * x1824._snd[2]);
          x1837[31] = x1837[31] + (x1824._fst[7] * x1824._snd[3]);
        }

        {
          struct Record_8_float_4_float x1824;
          /* mapSeq */
          /* unrolling loop of 8 */
          x1824._fst[0] = x1874[0]._fst[192 + (8 * get_local_id(1))];
          x1824._fst[1] = x1874[0]._fst[193 + (8 * get_local_id(1))];
          x1824._fst[2] = x1874[0]._fst[194 + (8 * get_local_id(1))];
          x1824._fst[3] = x1874[0]._fst[195 + (8 * get_local_id(1))];
          x1824._fst[4] = x1874[0]._fst[196 + (8 * get_local_id(1))];
          x1824._fst[5] = x1874[0]._fst[197 + (8 * get_local_id(1))];
          x1824._fst[6] = x1874[0]._fst[198 + (8 * get_local_id(1))];
          x1824._fst[7] = x1874[0]._fst[199 + (8 * get_local_id(1))];
          /* mapSeq */
          /* unrolling loop of 4 */
          x1824._snd[0] = x1874[0]._snd[384 + (4 * get_local_id(0))];
          x1824._snd[1] = x1874[0]._snd[385 + (4 * get_local_id(0))];
          x1824._snd[2] = x1874[0]._snd[386 + (4 * get_local_id(0))];
          x1824._snd[3] = x1874[0]._snd[387 + (4 * get_local_id(0))];
          /* mapSeq */
          /* unrolling loop of 8 */
          /* mapSeq */
          /* unrolling loop of 4 */
          x1837[0] = x1837[0] + (x1824._fst[0] * x1824._snd[0]);
          x1837[1] = x1837[1] + (x1824._fst[0] * x1824._snd[1]);
          x1837[2] = x1837[2] + (x1824._fst[0] * x1824._snd[2]);
          x1837[3] = x1837[3] + (x1824._fst[0] * x1824._snd[3]);
          /* mapSeq */
          /* unrolling loop of 4 */
          x1837[4] = x1837[4] + (x1824._fst[1] * x1824._snd[0]);
          x1837[5] = x1837[5] + (x1824._fst[1] * x1824._snd[1]);
          x1837[6] = x1837[6] + (x1824._fst[1] * x1824._snd[2]);
          x1837[7] = x1837[7] + (x1824._fst[1] * x1824._snd[3]);
          /* mapSeq */
          /* unrolling loop of 4 */
          x1837[8] = x1837[8] + (x1824._fst[2] * x1824._snd[0]);
          x1837[9] = x1837[9] + (x1824._fst[2] * x1824._snd[1]);
          x1837[10] = x1837[10] + (x1824._fst[2] * x1824._snd[2]);
          x1837[11] = x1837[11] + (x1824._fst[2] * x1824._snd[3]);
          /* mapSeq */
          /* unrolling loop of 4 */
          x1837[12] = x1837[12] + (x1824._fst[3] * x1824._snd[0]);
          x1837[13] = x1837[13] + (x1824._fst[3] * x1824._snd[1]);
          x1837[14] = x1837[14] + (x1824._fst[3] * x1824._snd[2]);
          x1837[15] = x1837[15] + (x1824._fst[3] * x1824._snd[3]);
          /* mapSeq */
          /* unrolling loop of 4 */
          x1837[16] = x1837[16] + (x1824._fst[4] * x1824._snd[0]);
          x1837[17] = x1837[17] + (x1824._fst[4] * x1824._snd[1]);
          x1837[18] = x1837[18] + (x1824._fst[4] * x1824._snd[2]);
          x1837[19] = x1837[19] + (x1824._fst[4] * x1824._snd[3]);
          /* mapSeq */
          /* unrolling loop of 4 */
          x1837[20] = x1837[20] + (x1824._fst[5] * x1824._snd[0]);
          x1837[21] = x1837[21] + (x1824._fst[5] * x1824._snd[1]);
          x1837[22] = x1837[22] + (x1824._fst[5] * x1824._snd[2]);
          x1837[23] = x1837[23] + (x1824._fst[5] * x1824._snd[3]);
          /* mapSeq */
          /* unrolling loop of 4 */
          x1837[24] = x1837[24] + (x1824._fst[6] * x1824._snd[0]);
          x1837[25] = x1837[25] + (x1824._fst[6] * x1824._snd[1]);
          x1837[26] = x1837[26] + (x1824._fst[6] * x1824._snd[2]);
          x1837[27] = x1837[27] + (x1824._fst[6] * x1824._snd[3]);
          /* mapSeq */
          /* unrolling loop of 4 */
          x1837[28] = x1837[28] + (x1824._fst[7] * x1824._snd[0]);
          x1837[29] = x1837[29] + (x1824._fst[7] * x1824._snd[1]);
          x1837[30] = x1837[30] + (x1824._fst[7] * x1824._snd[2]);
          x1837[31] = x1837[31] + (x1824._fst[7] * x1824._snd[3]);
        }

        {
          struct Record_8_float_4_float x1824;
          /* mapSeq */
          /* unrolling loop of 8 */
          x1824._fst[0] = x1874[0]._fst[256 + (8 * get_local_id(1))];
          x1824._fst[1] = x1874[0]._fst[257 + (8 * get_local_id(1))];
          x1824._fst[2] = x1874[0]._fst[258 + (8 * get_local_id(1))];
          x1824._fst[3] = x1874[0]._fst[259 + (8 * get_local_id(1))];
          x1824._fst[4] = x1874[0]._fst[260 + (8 * get_local_id(1))];
          x1824._fst[5] = x1874[0]._fst[261 + (8 * get_local_id(1))];
          x1824._fst[6] = x1874[0]._fst[262 + (8 * get_local_id(1))];
          x1824._fst[7] = x1874[0]._fst[263 + (8 * get_local_id(1))];
          /* mapSeq */
          /* unrolling loop of 4 */
          x1824._snd[0] = x1874[0]._snd[512 + (4 * get_local_id(0))];
          x1824._snd[1] = x1874[0]._snd[513 + (4 * get_local_id(0))];
          x1824._snd[2] = x1874[0]._snd[514 + (4 * get_local_id(0))];
          x1824._snd[3] = x1874[0]._snd[515 + (4 * get_local_id(0))];
          /* mapSeq */
          /* unrolling loop of 8 */
          /* mapSeq */
          /* unrolling loop of 4 */
          x1837[0] = x1837[0] + (x1824._fst[0] * x1824._snd[0]);
          x1837[1] = x1837[1] + (x1824._fst[0] * x1824._snd[1]);
          x1837[2] = x1837[2] + (x1824._fst[0] * x1824._snd[2]);
          x1837[3] = x1837[3] + (x1824._fst[0] * x1824._snd[3]);
          /* mapSeq */
          /* unrolling loop of 4 */
          x1837[4] = x1837[4] + (x1824._fst[1] * x1824._snd[0]);
          x1837[5] = x1837[5] + (x1824._fst[1] * x1824._snd[1]);
          x1837[6] = x1837[6] + (x1824._fst[1] * x1824._snd[2]);
          x1837[7] = x1837[7] + (x1824._fst[1] * x1824._snd[3]);
          /* mapSeq */
          /* unrolling loop of 4 */
          x1837[8] = x1837[8] + (x1824._fst[2] * x1824._snd[0]);
          x1837[9] = x1837[9] + (x1824._fst[2] * x1824._snd[1]);
          x1837[10] = x1837[10] + (x1824._fst[2] * x1824._snd[2]);
          x1837[11] = x1837[11] + (x1824._fst[2] * x1824._snd[3]);
          /* mapSeq */
          /* unrolling loop of 4 */
          x1837[12] = x1837[12] + (x1824._fst[3] * x1824._snd[0]);
          x1837[13] = x1837[13] + (x1824._fst[3] * x1824._snd[1]);
          x1837[14] = x1837[14] + (x1824._fst[3] * x1824._snd[2]);
          x1837[15] = x1837[15] + (x1824._fst[3] * x1824._snd[3]);
          /* mapSeq */
          /* unrolling loop of 4 */
          x1837[16] = x1837[16] + (x1824._fst[4] * x1824._snd[0]);
          x1837[17] = x1837[17] + (x1824._fst[4] * x1824._snd[1]);
          x1837[18] = x1837[18] + (x1824._fst[4] * x1824._snd[2]);
          x1837[19] = x1837[19] + (x1824._fst[4] * x1824._snd[3]);
          /* mapSeq */
          /* unrolling loop of 4 */
          x1837[20] = x1837[20] + (x1824._fst[5] * x1824._snd[0]);
          x1837[21] = x1837[21] + (x1824._fst[5] * x1824._snd[1]);
          x1837[22] = x1837[22] + (x1824._fst[5] * x1824._snd[2]);
          x1837[23] = x1837[23] + (x1824._fst[5] * x1824._snd[3]);
          /* mapSeq */
          /* unrolling loop of 4 */
          x1837[24] = x1837[24] + (x1824._fst[6] * x1824._snd[0]);
          x1837[25] = x1837[25] + (x1824._fst[6] * x1824._snd[1]);
          x1837[26] = x1837[26] + (x1824._fst[6] * x1824._snd[2]);
          x1837[27] = x1837[27] + (x1824._fst[6] * x1824._snd[3]);
          /* mapSeq */
          /* unrolling loop of 4 */
          x1837[28] = x1837[28] + (x1824._fst[7] * x1824._snd[0]);
          x1837[29] = x1837[29] + (x1824._fst[7] * x1824._snd[1]);
          x1837[30] = x1837[30] + (x1824._fst[7] * x1824._snd[2]);
          x1837[31] = x1837[31] + (x1824._fst[7] * x1824._snd[3]);
        }

        {
          struct Record_8_float_4_float x1824;
          /* mapSeq */
          /* unrolling loop of 8 */
          x1824._fst[0] = x1874[0]._fst[320 + (8 * get_local_id(1))];
          x1824._fst[1] = x1874[0]._fst[321 + (8 * get_local_id(1))];
          x1824._fst[2] = x1874[0]._fst[322 + (8 * get_local_id(1))];
          x1824._fst[3] = x1874[0]._fst[323 + (8 * get_local_id(1))];
          x1824._fst[4] = x1874[0]._fst[324 + (8 * get_local_id(1))];
          x1824._fst[5] = x1874[0]._fst[325 + (8 * get_local_id(1))];
          x1824._fst[6] = x1874[0]._fst[326 + (8 * get_local_id(1))];
          x1824._fst[7] = x1874[0]._fst[327 + (8 * get_local_id(1))];
          /* mapSeq */
          /* unrolling loop of 4 */
          x1824._snd[0] = x1874[0]._snd[640 + (4 * get_local_id(0))];
          x1824._snd[1] = x1874[0]._snd[641 + (4 * get_local_id(0))];
          x1824._snd[2] = x1874[0]._snd[642 + (4 * get_local_id(0))];
          x1824._snd[3] = x1874[0]._snd[643 + (4 * get_local_id(0))];
          /* mapSeq */
          /* unrolling loop of 8 */
          /* mapSeq */
          /* unrolling loop of 4 */
          x1837[0] = x1837[0] + (x1824._fst[0] * x1824._snd[0]);
          x1837[1] = x1837[1] + (x1824._fst[0] * x1824._snd[1]);
          x1837[2] = x1837[2] + (x1824._fst[0] * x1824._snd[2]);
          x1837[3] = x1837[3] + (x1824._fst[0] * x1824._snd[3]);
          /* mapSeq */
          /* unrolling loop of 4 */
          x1837[4] = x1837[4] + (x1824._fst[1] * x1824._snd[0]);
          x1837[5] = x1837[5] + (x1824._fst[1] * x1824._snd[1]);
          x1837[6] = x1837[6] + (x1824._fst[1] * x1824._snd[2]);
          x1837[7] = x1837[7] + (x1824._fst[1] * x1824._snd[3]);
          /* mapSeq */
          /* unrolling loop of 4 */
          x1837[8] = x1837[8] + (x1824._fst[2] * x1824._snd[0]);
          x1837[9] = x1837[9] + (x1824._fst[2] * x1824._snd[1]);
          x1837[10] = x1837[10] + (x1824._fst[2] * x1824._snd[2]);
          x1837[11] = x1837[11] + (x1824._fst[2] * x1824._snd[3]);
          /* mapSeq */
          /* unrolling loop of 4 */
          x1837[12] = x1837[12] + (x1824._fst[3] * x1824._snd[0]);
          x1837[13] = x1837[13] + (x1824._fst[3] * x1824._snd[1]);
          x1837[14] = x1837[14] + (x1824._fst[3] * x1824._snd[2]);
          x1837[15] = x1837[15] + (x1824._fst[3] * x1824._snd[3]);
          /* mapSeq */
          /* unrolling loop of 4 */
          x1837[16] = x1837[16] + (x1824._fst[4] * x1824._snd[0]);
          x1837[17] = x1837[17] + (x1824._fst[4] * x1824._snd[1]);
          x1837[18] = x1837[18] + (x1824._fst[4] * x1824._snd[2]);
          x1837[19] = x1837[19] + (x1824._fst[4] * x1824._snd[3]);
          /* mapSeq */
          /* unrolling loop of 4 */
          x1837[20] = x1837[20] + (x1824._fst[5] * x1824._snd[0]);
          x1837[21] = x1837[21] + (x1824._fst[5] * x1824._snd[1]);
          x1837[22] = x1837[22] + (x1824._fst[5] * x1824._snd[2]);
          x1837[23] = x1837[23] + (x1824._fst[5] * x1824._snd[3]);
          /* mapSeq */
          /* unrolling loop of 4 */
          x1837[24] = x1837[24] + (x1824._fst[6] * x1824._snd[0]);
          x1837[25] = x1837[25] + (x1824._fst[6] * x1824._snd[1]);
          x1837[26] = x1837[26] + (x1824._fst[6] * x1824._snd[2]);
          x1837[27] = x1837[27] + (x1824._fst[6] * x1824._snd[3]);
          /* mapSeq */
          /* unrolling loop of 4 */
          x1837[28] = x1837[28] + (x1824._fst[7] * x1824._snd[0]);
          x1837[29] = x1837[29] + (x1824._fst[7] * x1824._snd[1]);
          x1837[30] = x1837[30] + (x1824._fst[7] * x1824._snd[2]);
          x1837[31] = x1837[31] + (x1824._fst[7] * x1824._snd[3]);
        }

        {
          struct Record_8_float_4_float x1824;
          /* mapSeq */
          /* unrolling loop of 8 */
          x1824._fst[0] = x1874[0]._fst[384 + (8 * get_local_id(1))];
          x1824._fst[1] = x1874[0]._fst[385 + (8 * get_local_id(1))];
          x1824._fst[2] = x1874[0]._fst[386 + (8 * get_local_id(1))];
          x1824._fst[3] = x1874[0]._fst[387 + (8 * get_local_id(1))];
          x1824._fst[4] = x1874[0]._fst[388 + (8 * get_local_id(1))];
          x1824._fst[5] = x1874[0]._fst[389 + (8 * get_local_id(1))];
          x1824._fst[6] = x1874[0]._fst[390 + (8 * get_local_id(1))];
          x1824._fst[7] = x1874[0]._fst[391 + (8 * get_local_id(1))];
          /* mapSeq */
          /* unrolling loop of 4 */
          x1824._snd[0] = x1874[0]._snd[768 + (4 * get_local_id(0))];
          x1824._snd[1] = x1874[0]._snd[769 + (4 * get_local_id(0))];
          x1824._snd[2] = x1874[0]._snd[770 + (4 * get_local_id(0))];
          x1824._snd[3] = x1874[0]._snd[771 + (4 * get_local_id(0))];
          /* mapSeq */
          /* unrolling loop of 8 */
          /* mapSeq */
          /* unrolling loop of 4 */
          x1837[0] = x1837[0] + (x1824._fst[0] * x1824._snd[0]);
          x1837[1] = x1837[1] + (x1824._fst[0] * x1824._snd[1]);
          x1837[2] = x1837[2] + (x1824._fst[0] * x1824._snd[2]);
          x1837[3] = x1837[3] + (x1824._fst[0] * x1824._snd[3]);
          /* mapSeq */
          /* unrolling loop of 4 */
          x1837[4] = x1837[4] + (x1824._fst[1] * x1824._snd[0]);
          x1837[5] = x1837[5] + (x1824._fst[1] * x1824._snd[1]);
          x1837[6] = x1837[6] + (x1824._fst[1] * x1824._snd[2]);
          x1837[7] = x1837[7] + (x1824._fst[1] * x1824._snd[3]);
          /* mapSeq */
          /* unrolling loop of 4 */
          x1837[8] = x1837[8] + (x1824._fst[2] * x1824._snd[0]);
          x1837[9] = x1837[9] + (x1824._fst[2] * x1824._snd[1]);
          x1837[10] = x1837[10] + (x1824._fst[2] * x1824._snd[2]);
          x1837[11] = x1837[11] + (x1824._fst[2] * x1824._snd[3]);
          /* mapSeq */
          /* unrolling loop of 4 */
          x1837[12] = x1837[12] + (x1824._fst[3] * x1824._snd[0]);
          x1837[13] = x1837[13] + (x1824._fst[3] * x1824._snd[1]);
          x1837[14] = x1837[14] + (x1824._fst[3] * x1824._snd[2]);
          x1837[15] = x1837[15] + (x1824._fst[3] * x1824._snd[3]);
          /* mapSeq */
          /* unrolling loop of 4 */
          x1837[16] = x1837[16] + (x1824._fst[4] * x1824._snd[0]);
          x1837[17] = x1837[17] + (x1824._fst[4] * x1824._snd[1]);
          x1837[18] = x1837[18] + (x1824._fst[4] * x1824._snd[2]);
          x1837[19] = x1837[19] + (x1824._fst[4] * x1824._snd[3]);
          /* mapSeq */
          /* unrolling loop of 4 */
          x1837[20] = x1837[20] + (x1824._fst[5] * x1824._snd[0]);
          x1837[21] = x1837[21] + (x1824._fst[5] * x1824._snd[1]);
          x1837[22] = x1837[22] + (x1824._fst[5] * x1824._snd[2]);
          x1837[23] = x1837[23] + (x1824._fst[5] * x1824._snd[3]);
          /* mapSeq */
          /* unrolling loop of 4 */
          x1837[24] = x1837[24] + (x1824._fst[6] * x1824._snd[0]);
          x1837[25] = x1837[25] + (x1824._fst[6] * x1824._snd[1]);
          x1837[26] = x1837[26] + (x1824._fst[6] * x1824._snd[2]);
          x1837[27] = x1837[27] + (x1824._fst[6] * x1824._snd[3]);
          /* mapSeq */
          /* unrolling loop of 4 */
          x1837[28] = x1837[28] + (x1824._fst[7] * x1824._snd[0]);
          x1837[29] = x1837[29] + (x1824._fst[7] * x1824._snd[1]);
          x1837[30] = x1837[30] + (x1824._fst[7] * x1824._snd[2]);
          x1837[31] = x1837[31] + (x1824._fst[7] * x1824._snd[3]);
        }

        {
          struct Record_8_float_4_float x1824;
          /* mapSeq */
          /* unrolling loop of 8 */
          x1824._fst[0] = x1874[0]._fst[448 + (8 * get_local_id(1))];
          x1824._fst[1] = x1874[0]._fst[449 + (8 * get_local_id(1))];
          x1824._fst[2] = x1874[0]._fst[450 + (8 * get_local_id(1))];
          x1824._fst[3] = x1874[0]._fst[451 + (8 * get_local_id(1))];
          x1824._fst[4] = x1874[0]._fst[452 + (8 * get_local_id(1))];
          x1824._fst[5] = x1874[0]._fst[453 + (8 * get_local_id(1))];
          x1824._fst[6] = x1874[0]._fst[454 + (8 * get_local_id(1))];
          x1824._fst[7] = x1874[0]._fst[455 + (8 * get_local_id(1))];
          /* mapSeq */
          /* unrolling loop of 4 */
          x1824._snd[0] = x1874[0]._snd[896 + (4 * get_local_id(0))];
          x1824._snd[1] = x1874[0]._snd[897 + (4 * get_local_id(0))];
          x1824._snd[2] = x1874[0]._snd[898 + (4 * get_local_id(0))];
          x1824._snd[3] = x1874[0]._snd[899 + (4 * get_local_id(0))];
          /* mapSeq */
          /* unrolling loop of 8 */
          /* mapSeq */
          /* unrolling loop of 4 */
          x1837[0] = x1837[0] + (x1824._fst[0] * x1824._snd[0]);
          x1837[1] = x1837[1] + (x1824._fst[0] * x1824._snd[1]);
          x1837[2] = x1837[2] + (x1824._fst[0] * x1824._snd[2]);
          x1837[3] = x1837[3] + (x1824._fst[0] * x1824._snd[3]);
          /* mapSeq */
          /* unrolling loop of 4 */
          x1837[4] = x1837[4] + (x1824._fst[1] * x1824._snd[0]);
          x1837[5] = x1837[5] + (x1824._fst[1] * x1824._snd[1]);
          x1837[6] = x1837[6] + (x1824._fst[1] * x1824._snd[2]);
          x1837[7] = x1837[7] + (x1824._fst[1] * x1824._snd[3]);
          /* mapSeq */
          /* unrolling loop of 4 */
          x1837[8] = x1837[8] + (x1824._fst[2] * x1824._snd[0]);
          x1837[9] = x1837[9] + (x1824._fst[2] * x1824._snd[1]);
          x1837[10] = x1837[10] + (x1824._fst[2] * x1824._snd[2]);
          x1837[11] = x1837[11] + (x1824._fst[2] * x1824._snd[3]);
          /* mapSeq */
          /* unrolling loop of 4 */
          x1837[12] = x1837[12] + (x1824._fst[3] * x1824._snd[0]);
          x1837[13] = x1837[13] + (x1824._fst[3] * x1824._snd[1]);
          x1837[14] = x1837[14] + (x1824._fst[3] * x1824._snd[2]);
          x1837[15] = x1837[15] + (x1824._fst[3] * x1824._snd[3]);
          /* mapSeq */
          /* unrolling loop of 4 */
          x1837[16] = x1837[16] + (x1824._fst[4] * x1824._snd[0]);
          x1837[17] = x1837[17] + (x1824._fst[4] * x1824._snd[1]);
          x1837[18] = x1837[18] + (x1824._fst[4] * x1824._snd[2]);
          x1837[19] = x1837[19] + (x1824._fst[4] * x1824._snd[3]);
          /* mapSeq */
          /* unrolling loop of 4 */
          x1837[20] = x1837[20] + (x1824._fst[5] * x1824._snd[0]);
          x1837[21] = x1837[21] + (x1824._fst[5] * x1824._snd[1]);
          x1837[22] = x1837[22] + (x1824._fst[5] * x1824._snd[2]);
          x1837[23] = x1837[23] + (x1824._fst[5] * x1824._snd[3]);
          /* mapSeq */
          /* unrolling loop of 4 */
          x1837[24] = x1837[24] + (x1824._fst[6] * x1824._snd[0]);
          x1837[25] = x1837[25] + (x1824._fst[6] * x1824._snd[1]);
          x1837[26] = x1837[26] + (x1824._fst[6] * x1824._snd[2]);
          x1837[27] = x1837[27] + (x1824._fst[6] * x1824._snd[3]);
          /* mapSeq */
          /* unrolling loop of 4 */
          x1837[28] = x1837[28] + (x1824._fst[7] * x1824._snd[0]);
          x1837[29] = x1837[29] + (x1824._fst[7] * x1824._snd[1]);
          x1837[30] = x1837[30] + (x1824._fst[7] * x1824._snd[2]);
          x1837[31] = x1837[31] + (x1824._fst[7] * x1824._snd[3]);
        }

        {
          struct Record_8_float_4_float x1824;
          /* mapSeq */
          /* unrolling loop of 8 */
          x1824._fst[0] = x1874[0]._fst[512 + (8 * get_local_id(1))];
          x1824._fst[1] = x1874[0]._fst[513 + (8 * get_local_id(1))];
          x1824._fst[2] = x1874[0]._fst[514 + (8 * get_local_id(1))];
          x1824._fst[3] = x1874[0]._fst[515 + (8 * get_local_id(1))];
          x1824._fst[4] = x1874[0]._fst[516 + (8 * get_local_id(1))];
          x1824._fst[5] = x1874[0]._fst[517 + (8 * get_local_id(1))];
          x1824._fst[6] = x1874[0]._fst[518 + (8 * get_local_id(1))];
          x1824._fst[7] = x1874[0]._fst[519 + (8 * get_local_id(1))];
          /* mapSeq */
          /* unrolling loop of 4 */
          x1824._snd[0] = x1874[0]._snd[1024 + (4 * get_local_id(0))];
          x1824._snd[1] = x1874[0]._snd[1025 + (4 * get_local_id(0))];
          x1824._snd[2] = x1874[0]._snd[1026 + (4 * get_local_id(0))];
          x1824._snd[3] = x1874[0]._snd[1027 + (4 * get_local_id(0))];
          /* mapSeq */
          /* unrolling loop of 8 */
          /* mapSeq */
          /* unrolling loop of 4 */
          x1837[0] = x1837[0] + (x1824._fst[0] * x1824._snd[0]);
          x1837[1] = x1837[1] + (x1824._fst[0] * x1824._snd[1]);
          x1837[2] = x1837[2] + (x1824._fst[0] * x1824._snd[2]);
          x1837[3] = x1837[3] + (x1824._fst[0] * x1824._snd[3]);
          /* mapSeq */
          /* unrolling loop of 4 */
          x1837[4] = x1837[4] + (x1824._fst[1] * x1824._snd[0]);
          x1837[5] = x1837[5] + (x1824._fst[1] * x1824._snd[1]);
          x1837[6] = x1837[6] + (x1824._fst[1] * x1824._snd[2]);
          x1837[7] = x1837[7] + (x1824._fst[1] * x1824._snd[3]);
          /* mapSeq */
          /* unrolling loop of 4 */
          x1837[8] = x1837[8] + (x1824._fst[2] * x1824._snd[0]);
          x1837[9] = x1837[9] + (x1824._fst[2] * x1824._snd[1]);
          x1837[10] = x1837[10] + (x1824._fst[2] * x1824._snd[2]);
          x1837[11] = x1837[11] + (x1824._fst[2] * x1824._snd[3]);
          /* mapSeq */
          /* unrolling loop of 4 */
          x1837[12] = x1837[12] + (x1824._fst[3] * x1824._snd[0]);
          x1837[13] = x1837[13] + (x1824._fst[3] * x1824._snd[1]);
          x1837[14] = x1837[14] + (x1824._fst[3] * x1824._snd[2]);
          x1837[15] = x1837[15] + (x1824._fst[3] * x1824._snd[3]);
          /* mapSeq */
          /* unrolling loop of 4 */
          x1837[16] = x1837[16] + (x1824._fst[4] * x1824._snd[0]);
          x1837[17] = x1837[17] + (x1824._fst[4] * x1824._snd[1]);
          x1837[18] = x1837[18] + (x1824._fst[4] * x1824._snd[2]);
          x1837[19] = x1837[19] + (x1824._fst[4] * x1824._snd[3]);
          /* mapSeq */
          /* unrolling loop of 4 */
          x1837[20] = x1837[20] + (x1824._fst[5] * x1824._snd[0]);
          x1837[21] = x1837[21] + (x1824._fst[5] * x1824._snd[1]);
          x1837[22] = x1837[22] + (x1824._fst[5] * x1824._snd[2]);
          x1837[23] = x1837[23] + (x1824._fst[5] * x1824._snd[3]);
          /* mapSeq */
          /* unrolling loop of 4 */
          x1837[24] = x1837[24] + (x1824._fst[6] * x1824._snd[0]);
          x1837[25] = x1837[25] + (x1824._fst[6] * x1824._snd[1]);
          x1837[26] = x1837[26] + (x1824._fst[6] * x1824._snd[2]);
          x1837[27] = x1837[27] + (x1824._fst[6] * x1824._snd[3]);
          /* mapSeq */
          /* unrolling loop of 4 */
          x1837[28] = x1837[28] + (x1824._fst[7] * x1824._snd[0]);
          x1837[29] = x1837[29] + (x1824._fst[7] * x1824._snd[1]);
          x1837[30] = x1837[30] + (x1824._fst[7] * x1824._snd[2]);
          x1837[31] = x1837[31] + (x1824._fst[7] * x1824._snd[3]);
        }

        {
          struct Record_8_float_4_float x1824;
          /* mapSeq */
          /* unrolling loop of 8 */
          x1824._fst[0] = x1874[0]._fst[576 + (8 * get_local_id(1))];
          x1824._fst[1] = x1874[0]._fst[577 + (8 * get_local_id(1))];
          x1824._fst[2] = x1874[0]._fst[578 + (8 * get_local_id(1))];
          x1824._fst[3] = x1874[0]._fst[579 + (8 * get_local_id(1))];
          x1824._fst[4] = x1874[0]._fst[580 + (8 * get_local_id(1))];
          x1824._fst[5] = x1874[0]._fst[581 + (8 * get_local_id(1))];
          x1824._fst[6] = x1874[0]._fst[582 + (8 * get_local_id(1))];
          x1824._fst[7] = x1874[0]._fst[583 + (8 * get_local_id(1))];
          /* mapSeq */
          /* unrolling loop of 4 */
          x1824._snd[0] = x1874[0]._snd[1152 + (4 * get_local_id(0))];
          x1824._snd[1] = x1874[0]._snd[1153 + (4 * get_local_id(0))];
          x1824._snd[2] = x1874[0]._snd[1154 + (4 * get_local_id(0))];
          x1824._snd[3] = x1874[0]._snd[1155 + (4 * get_local_id(0))];
          /* mapSeq */
          /* unrolling loop of 8 */
          /* mapSeq */
          /* unrolling loop of 4 */
          x1837[0] = x1837[0] + (x1824._fst[0] * x1824._snd[0]);
          x1837[1] = x1837[1] + (x1824._fst[0] * x1824._snd[1]);
          x1837[2] = x1837[2] + (x1824._fst[0] * x1824._snd[2]);
          x1837[3] = x1837[3] + (x1824._fst[0] * x1824._snd[3]);
          /* mapSeq */
          /* unrolling loop of 4 */
          x1837[4] = x1837[4] + (x1824._fst[1] * x1824._snd[0]);
          x1837[5] = x1837[5] + (x1824._fst[1] * x1824._snd[1]);
          x1837[6] = x1837[6] + (x1824._fst[1] * x1824._snd[2]);
          x1837[7] = x1837[7] + (x1824._fst[1] * x1824._snd[3]);
          /* mapSeq */
          /* unrolling loop of 4 */
          x1837[8] = x1837[8] + (x1824._fst[2] * x1824._snd[0]);
          x1837[9] = x1837[9] + (x1824._fst[2] * x1824._snd[1]);
          x1837[10] = x1837[10] + (x1824._fst[2] * x1824._snd[2]);
          x1837[11] = x1837[11] + (x1824._fst[2] * x1824._snd[3]);
          /* mapSeq */
          /* unrolling loop of 4 */
          x1837[12] = x1837[12] + (x1824._fst[3] * x1824._snd[0]);
          x1837[13] = x1837[13] + (x1824._fst[3] * x1824._snd[1]);
          x1837[14] = x1837[14] + (x1824._fst[3] * x1824._snd[2]);
          x1837[15] = x1837[15] + (x1824._fst[3] * x1824._snd[3]);
          /* mapSeq */
          /* unrolling loop of 4 */
          x1837[16] = x1837[16] + (x1824._fst[4] * x1824._snd[0]);
          x1837[17] = x1837[17] + (x1824._fst[4] * x1824._snd[1]);
          x1837[18] = x1837[18] + (x1824._fst[4] * x1824._snd[2]);
          x1837[19] = x1837[19] + (x1824._fst[4] * x1824._snd[3]);
          /* mapSeq */
          /* unrolling loop of 4 */
          x1837[20] = x1837[20] + (x1824._fst[5] * x1824._snd[0]);
          x1837[21] = x1837[21] + (x1824._fst[5] * x1824._snd[1]);
          x1837[22] = x1837[22] + (x1824._fst[5] * x1824._snd[2]);
          x1837[23] = x1837[23] + (x1824._fst[5] * x1824._snd[3]);
          /* mapSeq */
          /* unrolling loop of 4 */
          x1837[24] = x1837[24] + (x1824._fst[6] * x1824._snd[0]);
          x1837[25] = x1837[25] + (x1824._fst[6] * x1824._snd[1]);
          x1837[26] = x1837[26] + (x1824._fst[6] * x1824._snd[2]);
          x1837[27] = x1837[27] + (x1824._fst[6] * x1824._snd[3]);
          /* mapSeq */
          /* unrolling loop of 4 */
          x1837[28] = x1837[28] + (x1824._fst[7] * x1824._snd[0]);
          x1837[29] = x1837[29] + (x1824._fst[7] * x1824._snd[1]);
          x1837[30] = x1837[30] + (x1824._fst[7] * x1824._snd[2]);
          x1837[31] = x1837[31] + (x1824._fst[7] * x1824._snd[3]);
        }

        {
          struct Record_8_float_4_float x1824;
          /* mapSeq */
          /* unrolling loop of 8 */
          x1824._fst[0] = x1874[0]._fst[640 + (8 * get_local_id(1))];
          x1824._fst[1] = x1874[0]._fst[641 + (8 * get_local_id(1))];
          x1824._fst[2] = x1874[0]._fst[642 + (8 * get_local_id(1))];
          x1824._fst[3] = x1874[0]._fst[643 + (8 * get_local_id(1))];
          x1824._fst[4] = x1874[0]._fst[644 + (8 * get_local_id(1))];
          x1824._fst[5] = x1874[0]._fst[645 + (8 * get_local_id(1))];
          x1824._fst[6] = x1874[0]._fst[646 + (8 * get_local_id(1))];
          x1824._fst[7] = x1874[0]._fst[647 + (8 * get_local_id(1))];
          /* mapSeq */
          /* unrolling loop of 4 */
          x1824._snd[0] = x1874[0]._snd[1280 + (4 * get_local_id(0))];
          x1824._snd[1] = x1874[0]._snd[1281 + (4 * get_local_id(0))];
          x1824._snd[2] = x1874[0]._snd[1282 + (4 * get_local_id(0))];
          x1824._snd[3] = x1874[0]._snd[1283 + (4 * get_local_id(0))];
          /* mapSeq */
          /* unrolling loop of 8 */
          /* mapSeq */
          /* unrolling loop of 4 */
          x1837[0] = x1837[0] + (x1824._fst[0] * x1824._snd[0]);
          x1837[1] = x1837[1] + (x1824._fst[0] * x1824._snd[1]);
          x1837[2] = x1837[2] + (x1824._fst[0] * x1824._snd[2]);
          x1837[3] = x1837[3] + (x1824._fst[0] * x1824._snd[3]);
          /* mapSeq */
          /* unrolling loop of 4 */
          x1837[4] = x1837[4] + (x1824._fst[1] * x1824._snd[0]);
          x1837[5] = x1837[5] + (x1824._fst[1] * x1824._snd[1]);
          x1837[6] = x1837[6] + (x1824._fst[1] * x1824._snd[2]);
          x1837[7] = x1837[7] + (x1824._fst[1] * x1824._snd[3]);
          /* mapSeq */
          /* unrolling loop of 4 */
          x1837[8] = x1837[8] + (x1824._fst[2] * x1824._snd[0]);
          x1837[9] = x1837[9] + (x1824._fst[2] * x1824._snd[1]);
          x1837[10] = x1837[10] + (x1824._fst[2] * x1824._snd[2]);
          x1837[11] = x1837[11] + (x1824._fst[2] * x1824._snd[3]);
          /* mapSeq */
          /* unrolling loop of 4 */
          x1837[12] = x1837[12] + (x1824._fst[3] * x1824._snd[0]);
          x1837[13] = x1837[13] + (x1824._fst[3] * x1824._snd[1]);
          x1837[14] = x1837[14] + (x1824._fst[3] * x1824._snd[2]);
          x1837[15] = x1837[15] + (x1824._fst[3] * x1824._snd[3]);
          /* mapSeq */
          /* unrolling loop of 4 */
          x1837[16] = x1837[16] + (x1824._fst[4] * x1824._snd[0]);
          x1837[17] = x1837[17] + (x1824._fst[4] * x1824._snd[1]);
          x1837[18] = x1837[18] + (x1824._fst[4] * x1824._snd[2]);
          x1837[19] = x1837[19] + (x1824._fst[4] * x1824._snd[3]);
          /* mapSeq */
          /* unrolling loop of 4 */
          x1837[20] = x1837[20] + (x1824._fst[5] * x1824._snd[0]);
          x1837[21] = x1837[21] + (x1824._fst[5] * x1824._snd[1]);
          x1837[22] = x1837[22] + (x1824._fst[5] * x1824._snd[2]);
          x1837[23] = x1837[23] + (x1824._fst[5] * x1824._snd[3]);
          /* mapSeq */
          /* unrolling loop of 4 */
          x1837[24] = x1837[24] + (x1824._fst[6] * x1824._snd[0]);
          x1837[25] = x1837[25] + (x1824._fst[6] * x1824._snd[1]);
          x1837[26] = x1837[26] + (x1824._fst[6] * x1824._snd[2]);
          x1837[27] = x1837[27] + (x1824._fst[6] * x1824._snd[3]);
          /* mapSeq */
          /* unrolling loop of 4 */
          x1837[28] = x1837[28] + (x1824._fst[7] * x1824._snd[0]);
          x1837[29] = x1837[29] + (x1824._fst[7] * x1824._snd[1]);
          x1837[30] = x1837[30] + (x1824._fst[7] * x1824._snd[2]);
          x1837[31] = x1837[31] + (x1824._fst[7] * x1824._snd[3]);
        }

        {
          struct Record_8_float_4_float x1824;
          /* mapSeq */
          /* unrolling loop of 8 */
          x1824._fst[0] = x1874[0]._fst[704 + (8 * get_local_id(1))];
          x1824._fst[1] = x1874[0]._fst[705 + (8 * get_local_id(1))];
          x1824._fst[2] = x1874[0]._fst[706 + (8 * get_local_id(1))];
          x1824._fst[3] = x1874[0]._fst[707 + (8 * get_local_id(1))];
          x1824._fst[4] = x1874[0]._fst[708 + (8 * get_local_id(1))];
          x1824._fst[5] = x1874[0]._fst[709 + (8 * get_local_id(1))];
          x1824._fst[6] = x1874[0]._fst[710 + (8 * get_local_id(1))];
          x1824._fst[7] = x1874[0]._fst[711 + (8 * get_local_id(1))];
          /* mapSeq */
          /* unrolling loop of 4 */
          x1824._snd[0] = x1874[0]._snd[1408 + (4 * get_local_id(0))];
          x1824._snd[1] = x1874[0]._snd[1409 + (4 * get_local_id(0))];
          x1824._snd[2] = x1874[0]._snd[1410 + (4 * get_local_id(0))];
          x1824._snd[3] = x1874[0]._snd[1411 + (4 * get_local_id(0))];
          /* mapSeq */
          /* unrolling loop of 8 */
          /* mapSeq */
          /* unrolling loop of 4 */
          x1837[0] = x1837[0] + (x1824._fst[0] * x1824._snd[0]);
          x1837[1] = x1837[1] + (x1824._fst[0] * x1824._snd[1]);
          x1837[2] = x1837[2] + (x1824._fst[0] * x1824._snd[2]);
          x1837[3] = x1837[3] + (x1824._fst[0] * x1824._snd[3]);
          /* mapSeq */
          /* unrolling loop of 4 */
          x1837[4] = x1837[4] + (x1824._fst[1] * x1824._snd[0]);
          x1837[5] = x1837[5] + (x1824._fst[1] * x1824._snd[1]);
          x1837[6] = x1837[6] + (x1824._fst[1] * x1824._snd[2]);
          x1837[7] = x1837[7] + (x1824._fst[1] * x1824._snd[3]);
          /* mapSeq */
          /* unrolling loop of 4 */
          x1837[8] = x1837[8] + (x1824._fst[2] * x1824._snd[0]);
          x1837[9] = x1837[9] + (x1824._fst[2] * x1824._snd[1]);
          x1837[10] = x1837[10] + (x1824._fst[2] * x1824._snd[2]);
          x1837[11] = x1837[11] + (x1824._fst[2] * x1824._snd[3]);
          /* mapSeq */
          /* unrolling loop of 4 */
          x1837[12] = x1837[12] + (x1824._fst[3] * x1824._snd[0]);
          x1837[13] = x1837[13] + (x1824._fst[3] * x1824._snd[1]);
          x1837[14] = x1837[14] + (x1824._fst[3] * x1824._snd[2]);
          x1837[15] = x1837[15] + (x1824._fst[3] * x1824._snd[3]);
          /* mapSeq */
          /* unrolling loop of 4 */
          x1837[16] = x1837[16] + (x1824._fst[4] * x1824._snd[0]);
          x1837[17] = x1837[17] + (x1824._fst[4] * x1824._snd[1]);
          x1837[18] = x1837[18] + (x1824._fst[4] * x1824._snd[2]);
          x1837[19] = x1837[19] + (x1824._fst[4] * x1824._snd[3]);
          /* mapSeq */
          /* unrolling loop of 4 */
          x1837[20] = x1837[20] + (x1824._fst[5] * x1824._snd[0]);
          x1837[21] = x1837[21] + (x1824._fst[5] * x1824._snd[1]);
          x1837[22] = x1837[22] + (x1824._fst[5] * x1824._snd[2]);
          x1837[23] = x1837[23] + (x1824._fst[5] * x1824._snd[3]);
          /* mapSeq */
          /* unrolling loop of 4 */
          x1837[24] = x1837[24] + (x1824._fst[6] * x1824._snd[0]);
          x1837[25] = x1837[25] + (x1824._fst[6] * x1824._snd[1]);
          x1837[26] = x1837[26] + (x1824._fst[6] * x1824._snd[2]);
          x1837[27] = x1837[27] + (x1824._fst[6] * x1824._snd[3]);
          /* mapSeq */
          /* unrolling loop of 4 */
          x1837[28] = x1837[28] + (x1824._fst[7] * x1824._snd[0]);
          x1837[29] = x1837[29] + (x1824._fst[7] * x1824._snd[1]);
          x1837[30] = x1837[30] + (x1824._fst[7] * x1824._snd[2]);
          x1837[31] = x1837[31] + (x1824._fst[7] * x1824._snd[3]);
        }

        {
          struct Record_8_float_4_float x1824;
          /* mapSeq */
          /* unrolling loop of 8 */
          x1824._fst[0] = x1874[0]._fst[768 + (8 * get_local_id(1))];
          x1824._fst[1] = x1874[0]._fst[769 + (8 * get_local_id(1))];
          x1824._fst[2] = x1874[0]._fst[770 + (8 * get_local_id(1))];
          x1824._fst[3] = x1874[0]._fst[771 + (8 * get_local_id(1))];
          x1824._fst[4] = x1874[0]._fst[772 + (8 * get_local_id(1))];
          x1824._fst[5] = x1874[0]._fst[773 + (8 * get_local_id(1))];
          x1824._fst[6] = x1874[0]._fst[774 + (8 * get_local_id(1))];
          x1824._fst[7] = x1874[0]._fst[775 + (8 * get_local_id(1))];
          /* mapSeq */
          /* unrolling loop of 4 */
          x1824._snd[0] = x1874[0]._snd[1536 + (4 * get_local_id(0))];
          x1824._snd[1] = x1874[0]._snd[1537 + (4 * get_local_id(0))];
          x1824._snd[2] = x1874[0]._snd[1538 + (4 * get_local_id(0))];
          x1824._snd[3] = x1874[0]._snd[1539 + (4 * get_local_id(0))];
          /* mapSeq */
          /* unrolling loop of 8 */
          /* mapSeq */
          /* unrolling loop of 4 */
          x1837[0] = x1837[0] + (x1824._fst[0] * x1824._snd[0]);
          x1837[1] = x1837[1] + (x1824._fst[0] * x1824._snd[1]);
          x1837[2] = x1837[2] + (x1824._fst[0] * x1824._snd[2]);
          x1837[3] = x1837[3] + (x1824._fst[0] * x1824._snd[3]);
          /* mapSeq */
          /* unrolling loop of 4 */
          x1837[4] = x1837[4] + (x1824._fst[1] * x1824._snd[0]);
          x1837[5] = x1837[5] + (x1824._fst[1] * x1824._snd[1]);
          x1837[6] = x1837[6] + (x1824._fst[1] * x1824._snd[2]);
          x1837[7] = x1837[7] + (x1824._fst[1] * x1824._snd[3]);
          /* mapSeq */
          /* unrolling loop of 4 */
          x1837[8] = x1837[8] + (x1824._fst[2] * x1824._snd[0]);
          x1837[9] = x1837[9] + (x1824._fst[2] * x1824._snd[1]);
          x1837[10] = x1837[10] + (x1824._fst[2] * x1824._snd[2]);
          x1837[11] = x1837[11] + (x1824._fst[2] * x1824._snd[3]);
          /* mapSeq */
          /* unrolling loop of 4 */
          x1837[12] = x1837[12] + (x1824._fst[3] * x1824._snd[0]);
          x1837[13] = x1837[13] + (x1824._fst[3] * x1824._snd[1]);
          x1837[14] = x1837[14] + (x1824._fst[3] * x1824._snd[2]);
          x1837[15] = x1837[15] + (x1824._fst[3] * x1824._snd[3]);
          /* mapSeq */
          /* unrolling loop of 4 */
          x1837[16] = x1837[16] + (x1824._fst[4] * x1824._snd[0]);
          x1837[17] = x1837[17] + (x1824._fst[4] * x1824._snd[1]);
          x1837[18] = x1837[18] + (x1824._fst[4] * x1824._snd[2]);
          x1837[19] = x1837[19] + (x1824._fst[4] * x1824._snd[3]);
          /* mapSeq */
          /* unrolling loop of 4 */
          x1837[20] = x1837[20] + (x1824._fst[5] * x1824._snd[0]);
          x1837[21] = x1837[21] + (x1824._fst[5] * x1824._snd[1]);
          x1837[22] = x1837[22] + (x1824._fst[5] * x1824._snd[2]);
          x1837[23] = x1837[23] + (x1824._fst[5] * x1824._snd[3]);
          /* mapSeq */
          /* unrolling loop of 4 */
          x1837[24] = x1837[24] + (x1824._fst[6] * x1824._snd[0]);
          x1837[25] = x1837[25] + (x1824._fst[6] * x1824._snd[1]);
          x1837[26] = x1837[26] + (x1824._fst[6] * x1824._snd[2]);
          x1837[27] = x1837[27] + (x1824._fst[6] * x1824._snd[3]);
          /* mapSeq */
          /* unrolling loop of 4 */
          x1837[28] = x1837[28] + (x1824._fst[7] * x1824._snd[0]);
          x1837[29] = x1837[29] + (x1824._fst[7] * x1824._snd[1]);
          x1837[30] = x1837[30] + (x1824._fst[7] * x1824._snd[2]);
          x1837[31] = x1837[31] + (x1824._fst[7] * x1824._snd[3]);
        }

        {
          struct Record_8_float_4_float x1824;
          /* mapSeq */
          /* unrolling loop of 8 */
          x1824._fst[0] = x1874[0]._fst[832 + (8 * get_local_id(1))];
          x1824._fst[1] = x1874[0]._fst[833 + (8 * get_local_id(1))];
          x1824._fst[2] = x1874[0]._fst[834 + (8 * get_local_id(1))];
          x1824._fst[3] = x1874[0]._fst[835 + (8 * get_local_id(1))];
          x1824._fst[4] = x1874[0]._fst[836 + (8 * get_local_id(1))];
          x1824._fst[5] = x1874[0]._fst[837 + (8 * get_local_id(1))];
          x1824._fst[6] = x1874[0]._fst[838 + (8 * get_local_id(1))];
          x1824._fst[7] = x1874[0]._fst[839 + (8 * get_local_id(1))];
          /* mapSeq */
          /* unrolling loop of 4 */
          x1824._snd[0] = x1874[0]._snd[1664 + (4 * get_local_id(0))];
          x1824._snd[1] = x1874[0]._snd[1665 + (4 * get_local_id(0))];
          x1824._snd[2] = x1874[0]._snd[1666 + (4 * get_local_id(0))];
          x1824._snd[3] = x1874[0]._snd[1667 + (4 * get_local_id(0))];
          /* mapSeq */
          /* unrolling loop of 8 */
          /* mapSeq */
          /* unrolling loop of 4 */
          x1837[0] = x1837[0] + (x1824._fst[0] * x1824._snd[0]);
          x1837[1] = x1837[1] + (x1824._fst[0] * x1824._snd[1]);
          x1837[2] = x1837[2] + (x1824._fst[0] * x1824._snd[2]);
          x1837[3] = x1837[3] + (x1824._fst[0] * x1824._snd[3]);
          /* mapSeq */
          /* unrolling loop of 4 */
          x1837[4] = x1837[4] + (x1824._fst[1] * x1824._snd[0]);
          x1837[5] = x1837[5] + (x1824._fst[1] * x1824._snd[1]);
          x1837[6] = x1837[6] + (x1824._fst[1] * x1824._snd[2]);
          x1837[7] = x1837[7] + (x1824._fst[1] * x1824._snd[3]);
          /* mapSeq */
          /* unrolling loop of 4 */
          x1837[8] = x1837[8] + (x1824._fst[2] * x1824._snd[0]);
          x1837[9] = x1837[9] + (x1824._fst[2] * x1824._snd[1]);
          x1837[10] = x1837[10] + (x1824._fst[2] * x1824._snd[2]);
          x1837[11] = x1837[11] + (x1824._fst[2] * x1824._snd[3]);
          /* mapSeq */
          /* unrolling loop of 4 */
          x1837[12] = x1837[12] + (x1824._fst[3] * x1824._snd[0]);
          x1837[13] = x1837[13] + (x1824._fst[3] * x1824._snd[1]);
          x1837[14] = x1837[14] + (x1824._fst[3] * x1824._snd[2]);
          x1837[15] = x1837[15] + (x1824._fst[3] * x1824._snd[3]);
          /* mapSeq */
          /* unrolling loop of 4 */
          x1837[16] = x1837[16] + (x1824._fst[4] * x1824._snd[0]);
          x1837[17] = x1837[17] + (x1824._fst[4] * x1824._snd[1]);
          x1837[18] = x1837[18] + (x1824._fst[4] * x1824._snd[2]);
          x1837[19] = x1837[19] + (x1824._fst[4] * x1824._snd[3]);
          /* mapSeq */
          /* unrolling loop of 4 */
          x1837[20] = x1837[20] + (x1824._fst[5] * x1824._snd[0]);
          x1837[21] = x1837[21] + (x1824._fst[5] * x1824._snd[1]);
          x1837[22] = x1837[22] + (x1824._fst[5] * x1824._snd[2]);
          x1837[23] = x1837[23] + (x1824._fst[5] * x1824._snd[3]);
          /* mapSeq */
          /* unrolling loop of 4 */
          x1837[24] = x1837[24] + (x1824._fst[6] * x1824._snd[0]);
          x1837[25] = x1837[25] + (x1824._fst[6] * x1824._snd[1]);
          x1837[26] = x1837[26] + (x1824._fst[6] * x1824._snd[2]);
          x1837[27] = x1837[27] + (x1824._fst[6] * x1824._snd[3]);
          /* mapSeq */
          /* unrolling loop of 4 */
          x1837[28] = x1837[28] + (x1824._fst[7] * x1824._snd[0]);
          x1837[29] = x1837[29] + (x1824._fst[7] * x1824._snd[1]);
          x1837[30] = x1837[30] + (x1824._fst[7] * x1824._snd[2]);
          x1837[31] = x1837[31] + (x1824._fst[7] * x1824._snd[3]);
        }

        {
          struct Record_8_float_4_float x1824;
          /* mapSeq */
          /* unrolling loop of 8 */
          x1824._fst[0] = x1874[0]._fst[896 + (8 * get_local_id(1))];
          x1824._fst[1] = x1874[0]._fst[897 + (8 * get_local_id(1))];
          x1824._fst[2] = x1874[0]._fst[898 + (8 * get_local_id(1))];
          x1824._fst[3] = x1874[0]._fst[899 + (8 * get_local_id(1))];
          x1824._fst[4] = x1874[0]._fst[900 + (8 * get_local_id(1))];
          x1824._fst[5] = x1874[0]._fst[901 + (8 * get_local_id(1))];
          x1824._fst[6] = x1874[0]._fst[902 + (8 * get_local_id(1))];
          x1824._fst[7] = x1874[0]._fst[903 + (8 * get_local_id(1))];
          /* mapSeq */
          /* unrolling loop of 4 */
          x1824._snd[0] = x1874[0]._snd[1792 + (4 * get_local_id(0))];
          x1824._snd[1] = x1874[0]._snd[1793 + (4 * get_local_id(0))];
          x1824._snd[2] = x1874[0]._snd[1794 + (4 * get_local_id(0))];
          x1824._snd[3] = x1874[0]._snd[1795 + (4 * get_local_id(0))];
          /* mapSeq */
          /* unrolling loop of 8 */
          /* mapSeq */
          /* unrolling loop of 4 */
          x1837[0] = x1837[0] + (x1824._fst[0] * x1824._snd[0]);
          x1837[1] = x1837[1] + (x1824._fst[0] * x1824._snd[1]);
          x1837[2] = x1837[2] + (x1824._fst[0] * x1824._snd[2]);
          x1837[3] = x1837[3] + (x1824._fst[0] * x1824._snd[3]);
          /* mapSeq */
          /* unrolling loop of 4 */
          x1837[4] = x1837[4] + (x1824._fst[1] * x1824._snd[0]);
          x1837[5] = x1837[5] + (x1824._fst[1] * x1824._snd[1]);
          x1837[6] = x1837[6] + (x1824._fst[1] * x1824._snd[2]);
          x1837[7] = x1837[7] + (x1824._fst[1] * x1824._snd[3]);
          /* mapSeq */
          /* unrolling loop of 4 */
          x1837[8] = x1837[8] + (x1824._fst[2] * x1824._snd[0]);
          x1837[9] = x1837[9] + (x1824._fst[2] * x1824._snd[1]);
          x1837[10] = x1837[10] + (x1824._fst[2] * x1824._snd[2]);
          x1837[11] = x1837[11] + (x1824._fst[2] * x1824._snd[3]);
          /* mapSeq */
          /* unrolling loop of 4 */
          x1837[12] = x1837[12] + (x1824._fst[3] * x1824._snd[0]);
          x1837[13] = x1837[13] + (x1824._fst[3] * x1824._snd[1]);
          x1837[14] = x1837[14] + (x1824._fst[3] * x1824._snd[2]);
          x1837[15] = x1837[15] + (x1824._fst[3] * x1824._snd[3]);
          /* mapSeq */
          /* unrolling loop of 4 */
          x1837[16] = x1837[16] + (x1824._fst[4] * x1824._snd[0]);
          x1837[17] = x1837[17] + (x1824._fst[4] * x1824._snd[1]);
          x1837[18] = x1837[18] + (x1824._fst[4] * x1824._snd[2]);
          x1837[19] = x1837[19] + (x1824._fst[4] * x1824._snd[3]);
          /* mapSeq */
          /* unrolling loop of 4 */
          x1837[20] = x1837[20] + (x1824._fst[5] * x1824._snd[0]);
          x1837[21] = x1837[21] + (x1824._fst[5] * x1824._snd[1]);
          x1837[22] = x1837[22] + (x1824._fst[5] * x1824._snd[2]);
          x1837[23] = x1837[23] + (x1824._fst[5] * x1824._snd[3]);
          /* mapSeq */
          /* unrolling loop of 4 */
          x1837[24] = x1837[24] + (x1824._fst[6] * x1824._snd[0]);
          x1837[25] = x1837[25] + (x1824._fst[6] * x1824._snd[1]);
          x1837[26] = x1837[26] + (x1824._fst[6] * x1824._snd[2]);
          x1837[27] = x1837[27] + (x1824._fst[6] * x1824._snd[3]);
          /* mapSeq */
          /* unrolling loop of 4 */
          x1837[28] = x1837[28] + (x1824._fst[7] * x1824._snd[0]);
          x1837[29] = x1837[29] + (x1824._fst[7] * x1824._snd[1]);
          x1837[30] = x1837[30] + (x1824._fst[7] * x1824._snd[2]);
          x1837[31] = x1837[31] + (x1824._fst[7] * x1824._snd[3]);
        }

        {
          struct Record_8_float_4_float x1824;
          /* mapSeq */
          /* unrolling loop of 8 */
          x1824._fst[0] = x1874[0]._fst[960 + (8 * get_local_id(1))];
          x1824._fst[1] = x1874[0]._fst[961 + (8 * get_local_id(1))];
          x1824._fst[2] = x1874[0]._fst[962 + (8 * get_local_id(1))];
          x1824._fst[3] = x1874[0]._fst[963 + (8 * get_local_id(1))];
          x1824._fst[4] = x1874[0]._fst[964 + (8 * get_local_id(1))];
          x1824._fst[5] = x1874[0]._fst[965 + (8 * get_local_id(1))];
          x1824._fst[6] = x1874[0]._fst[966 + (8 * get_local_id(1))];
          x1824._fst[7] = x1874[0]._fst[967 + (8 * get_local_id(1))];
          /* mapSeq */
          /* unrolling loop of 4 */
          x1824._snd[0] = x1874[0]._snd[1920 + (4 * get_local_id(0))];
          x1824._snd[1] = x1874[0]._snd[1921 + (4 * get_local_id(0))];
          x1824._snd[2] = x1874[0]._snd[1922 + (4 * get_local_id(0))];
          x1824._snd[3] = x1874[0]._snd[1923 + (4 * get_local_id(0))];
          /* mapSeq */
          /* unrolling loop of 8 */
          /* mapSeq */
          /* unrolling loop of 4 */
          x1837[0] = x1837[0] + (x1824._fst[0] * x1824._snd[0]);
          x1837[1] = x1837[1] + (x1824._fst[0] * x1824._snd[1]);
          x1837[2] = x1837[2] + (x1824._fst[0] * x1824._snd[2]);
          x1837[3] = x1837[3] + (x1824._fst[0] * x1824._snd[3]);
          /* mapSeq */
          /* unrolling loop of 4 */
          x1837[4] = x1837[4] + (x1824._fst[1] * x1824._snd[0]);
          x1837[5] = x1837[5] + (x1824._fst[1] * x1824._snd[1]);
          x1837[6] = x1837[6] + (x1824._fst[1] * x1824._snd[2]);
          x1837[7] = x1837[7] + (x1824._fst[1] * x1824._snd[3]);
          /* mapSeq */
          /* unrolling loop of 4 */
          x1837[8] = x1837[8] + (x1824._fst[2] * x1824._snd[0]);
          x1837[9] = x1837[9] + (x1824._fst[2] * x1824._snd[1]);
          x1837[10] = x1837[10] + (x1824._fst[2] * x1824._snd[2]);
          x1837[11] = x1837[11] + (x1824._fst[2] * x1824._snd[3]);
          /* mapSeq */
          /* unrolling loop of 4 */
          x1837[12] = x1837[12] + (x1824._fst[3] * x1824._snd[0]);
          x1837[13] = x1837[13] + (x1824._fst[3] * x1824._snd[1]);
          x1837[14] = x1837[14] + (x1824._fst[3] * x1824._snd[2]);
          x1837[15] = x1837[15] + (x1824._fst[3] * x1824._snd[3]);
          /* mapSeq */
          /* unrolling loop of 4 */
          x1837[16] = x1837[16] + (x1824._fst[4] * x1824._snd[0]);
          x1837[17] = x1837[17] + (x1824._fst[4] * x1824._snd[1]);
          x1837[18] = x1837[18] + (x1824._fst[4] * x1824._snd[2]);
          x1837[19] = x1837[19] + (x1824._fst[4] * x1824._snd[3]);
          /* mapSeq */
          /* unrolling loop of 4 */
          x1837[20] = x1837[20] + (x1824._fst[5] * x1824._snd[0]);
          x1837[21] = x1837[21] + (x1824._fst[5] * x1824._snd[1]);
          x1837[22] = x1837[22] + (x1824._fst[5] * x1824._snd[2]);
          x1837[23] = x1837[23] + (x1824._fst[5] * x1824._snd[3]);
          /* mapSeq */
          /* unrolling loop of 4 */
          x1837[24] = x1837[24] + (x1824._fst[6] * x1824._snd[0]);
          x1837[25] = x1837[25] + (x1824._fst[6] * x1824._snd[1]);
          x1837[26] = x1837[26] + (x1824._fst[6] * x1824._snd[2]);
          x1837[27] = x1837[27] + (x1824._fst[6] * x1824._snd[3]);
          /* mapSeq */
          /* unrolling loop of 4 */
          x1837[28] = x1837[28] + (x1824._fst[7] * x1824._snd[0]);
          x1837[29] = x1837[29] + (x1824._fst[7] * x1824._snd[1]);
          x1837[30] = x1837[30] + (x1824._fst[7] * x1824._snd[2]);
          x1837[31] = x1837[31] + (x1824._fst[7] * x1824._snd[3]);
        }

        /* mapSeq */
        /* unrolling loop of 8 */
        /* mapSeq */
        /* unrolling loop of 4 */
        x1903[0] = x1837[0];
        x1903[1] = x1837[1];
        x1903[2] = x1837[2];
        x1903[3] = x1837[3];
        /* mapSeq */
        /* unrolling loop of 4 */
        x1903[4] = x1837[4];
        x1903[5] = x1837[5];
        x1903[6] = x1837[6];
        x1903[7] = x1837[7];
        /* mapSeq */
        /* unrolling loop of 4 */
        x1903[8] = x1837[8];
        x1903[9] = x1837[9];
        x1903[10] = x1837[10];
        x1903[11] = x1837[11];
        /* mapSeq */
        /* unrolling loop of 4 */
        x1903[12] = x1837[12];
        x1903[13] = x1837[13];
        x1903[14] = x1837[14];
        x1903[15] = x1837[15];
        /* mapSeq */
        /* unrolling loop of 4 */
        x1903[16] = x1837[16];
        x1903[17] = x1837[17];
        x1903[18] = x1837[18];
        x1903[19] = x1837[19];
        /* mapSeq */
        /* unrolling loop of 4 */
        x1903[20] = x1837[20];
        x1903[21] = x1837[21];
        x1903[22] = x1837[22];
        x1903[23] = x1837[23];
        /* mapSeq */
        /* unrolling loop of 4 */
        x1903[24] = x1837[24];
        x1903[25] = x1837[25];
        x1903[26] = x1837[26];
        x1903[27] = x1837[27];
        /* mapSeq */
        /* unrolling loop of 4 */
        x1903[28] = x1837[28];
        x1903[29] = x1837[29];
        x1903[30] = x1837[30];
        x1903[31] = x1837[31];
      }

      barrier(CLK_LOCAL_MEM_FENCE);
    }

    /* mapLocal */
    /* unrolling loop of 1 */
    /* mapLocal */
    /* unrolling loop of 1 */
    /* mapSeq */
    /* unrolling loop of 8 */
    /* mapSeq */
    /* unrolling loop of 1 */
    vstore4((float4)(x1903[0], x1903[1], x1903[2], x1903[3]), 0, &output[(((4 * get_local_id(0)) + ((8 * n869) * get_local_id(1))) + ((64 * n869) * wg_id_1970)) + (128 * wg_id_1971)]);
    /* mapSeq */
    /* unrolling loop of 1 */
    vstore4((float4)(x1903[4], x1903[5], x1903[6], x1903[7]), 0, &output[(((n869 + (4 * get_local_id(0))) + ((8 * n869) * get_local_id(1))) + ((64 * n869) * wg_id_1970)) + (128 * wg_id_1971)]);
    /* mapSeq */
    /* unrolling loop of 1 */
    vstore4((float4)(x1903[8], x1903[9], x1903[10], x1903[11]), 0, &output[((((2 * n869) + (4 * get_local_id(0))) + ((8 * n869) * get_local_id(1))) + ((64 * n869) * wg_id_1970)) + (128 * wg_id_1971)]);
    /* mapSeq */
    /* unrolling loop of 1 */
    vstore4((float4)(x1903[12], x1903[13], x1903[14], x1903[15]), 0, &output[((((3 * n869) + (4 * get_local_id(0))) + ((8 * n869) * get_local_id(1))) + ((64 * n869) * wg_id_1970)) + (128 * wg_id_1971)]);
    /* mapSeq */
    /* unrolling loop of 1 */
    vstore4((float4)(x1903[16], x1903[17], x1903[18], x1903[19]), 0, &output[((((4 * n869) + (4 * get_local_id(0))) + ((8 * n869) * get_local_id(1))) + ((64 * n869) * wg_id_1970)) + (128 * wg_id_1971)]);
    /* mapSeq */
    /* unrolling loop of 1 */
    vstore4((float4)(x1903[20], x1903[21], x1903[22], x1903[23]), 0, &output[((((4 * get_local_id(0)) + (5 * n869)) + ((8 * n869) * get_local_id(1))) + ((64 * n869) * wg_id_1970)) + (128 * wg_id_1971)]);
    /* mapSeq */
    /* unrolling loop of 1 */
    vstore4((float4)(x1903[24], x1903[25], x1903[26], x1903[27]), 0, &output[((((4 * get_local_id(0)) + (6 * n869)) + ((8 * n869) * get_local_id(1))) + ((64 * n869) * wg_id_1970)) + (128 * wg_id_1971)]);
    /* mapSeq */
    /* unrolling loop of 1 */
    vstore4((float4)(x1903[28], x1903[29], x1903[30], x1903[31]), 0, &output[((((4 * get_local_id(0)) + (7 * n869)) + ((8 * n869) * get_local_id(1))) + ((64 * n869) * wg_id_1970)) + (128 * wg_id_1971)]);
  }

}