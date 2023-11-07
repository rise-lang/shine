const char k0_source[] =
""
"struct Record_8_float_4_float {"
"  float _fst[8];"
"  float _snd[4];"
"};"
""
"struct Record_16_64_float_16_128_float {"
"  float _fst[1024];"
"  float _snd[2048];"
"};"
""
""
"__kernel __attribute__ ((reqd_work_group_size(32, 8, 1)))"
"void k0(global float* restrict output, int n205, int n204, int n206, const global float* restrict e207, const global float* restrict e208, local struct Record_16_64_float_16_128_float* restrict x1577){"
"  /* Start of moved local vars */"
"  /* End of moved local vars */"
"  /* mapWorkGroup */"
"  for (int wg_id_1641 = get_group_id(1); wg_id_1641 < (n204 / 64); wg_id_1641 = 16 + wg_id_1641) {"
"    /* mapWorkGroup */"
"    for (int wg_id_1642 = get_group_id(0); wg_id_1642 < (n205 / 128); wg_id_1642 = 8 + wg_id_1642) {"
"      /* oclReduceSeq */"
"      {"
"        float x1428[1024];"
"        /* mapLocal */"
"        /* unrolling loop of 1 */"
"        /* mapLocal */"
"        /* unrolling loop of 1 */"
"        /* mapSeq */"
"        /* unrolling loop of 8 */"
"        /* mapSeq */"
"        /* unrolling loop of 4 */"
"        x1428[32 * get_local_id(0)] = 0.0f;"
"        x1428[1 + (32 * get_local_id(0))] = 0.0f;"
"        x1428[2 + (32 * get_local_id(0))] = 0.0f;"
"        x1428[3 + (32 * get_local_id(0))] = 0.0f;"
"        /* mapSeq */"
"        /* unrolling loop of 4 */"
"        x1428[4 + (32 * get_local_id(0))] = 0.0f;"
"        x1428[5 + (32 * get_local_id(0))] = 0.0f;"
"        x1428[6 + (32 * get_local_id(0))] = 0.0f;"
"        x1428[7 + (32 * get_local_id(0))] = 0.0f;"
"        /* mapSeq */"
"        /* unrolling loop of 4 */"
"        x1428[8 + (32 * get_local_id(0))] = 0.0f;"
"        x1428[9 + (32 * get_local_id(0))] = 0.0f;"
"        x1428[10 + (32 * get_local_id(0))] = 0.0f;"
"        x1428[11 + (32 * get_local_id(0))] = 0.0f;"
"        /* mapSeq */"
"        /* unrolling loop of 4 */"
"        x1428[12 + (32 * get_local_id(0))] = 0.0f;"
"        x1428[13 + (32 * get_local_id(0))] = 0.0f;"
"        x1428[14 + (32 * get_local_id(0))] = 0.0f;"
"        x1428[15 + (32 * get_local_id(0))] = 0.0f;"
"        /* mapSeq */"
"        /* unrolling loop of 4 */"
"        x1428[16 + (32 * get_local_id(0))] = 0.0f;"
"        x1428[17 + (32 * get_local_id(0))] = 0.0f;"
"        x1428[18 + (32 * get_local_id(0))] = 0.0f;"
"        x1428[19 + (32 * get_local_id(0))] = 0.0f;"
"        /* mapSeq */"
"        /* unrolling loop of 4 */"
"        x1428[20 + (32 * get_local_id(0))] = 0.0f;"
"        x1428[21 + (32 * get_local_id(0))] = 0.0f;"
"        x1428[22 + (32 * get_local_id(0))] = 0.0f;"
"        x1428[23 + (32 * get_local_id(0))] = 0.0f;"
"        /* mapSeq */"
"        /* unrolling loop of 4 */"
"        x1428[24 + (32 * get_local_id(0))] = 0.0f;"
"        x1428[25 + (32 * get_local_id(0))] = 0.0f;"
"        x1428[26 + (32 * get_local_id(0))] = 0.0f;"
"        x1428[27 + (32 * get_local_id(0))] = 0.0f;"
"        /* mapSeq */"
"        /* unrolling loop of 4 */"
"        x1428[28 + (32 * get_local_id(0))] = 0.0f;"
"        x1428[29 + (32 * get_local_id(0))] = 0.0f;"
"        x1428[30 + (32 * get_local_id(0))] = 0.0f;"
"        x1428[31 + (32 * get_local_id(0))] = 0.0f;"
"        for (int i_1643 = 0; i_1643 < (n206 / 16); i_1643 = 1 + i_1643) {"
"          /* mapLocal */"
"          /* iteration count is exactly 1, no loop emitted */"
"          int l_id_1644 = get_local_id(1);"
"          /* mapLocal */"
"          /* iteration count is exactly 1, no loop emitted */"
"          int l_id_1645 = get_local_id(0);"
"          vstore4(vload4(0, &e207[(((((4 * l_id_1645) % 64) + ((2 * l_id_1644) * n204)) + ((16 * i_1643) * n204)) + (64 * wg_id_1641)) + (n204 * (l_id_1645 / 16))]), 0, &x1577[0]._fst[(4 * l_id_1645) + (128 * l_id_1644)]);"
"          /* mapLocal */"
"          for (int l_id_1646 = get_local_id(1); l_id_1646 < 16; l_id_1646 = 8 + l_id_1646) {"
"            /* mapLocal */"
"            /* iteration count is exactly 1, no loop emitted */"
"            int l_id_1647 = get_local_id(0);"
"            vstore4(vload4(0, &e208[(((4 * l_id_1647) + ((16 * i_1643) * n205)) + (128 * wg_id_1642)) + (l_id_1646 * n205)]), 0, &x1577[0]._snd[(4 * l_id_1647) + (128 * l_id_1646)]);"
"          }"
"          "
"          barrier(CLK_LOCAL_MEM_FENCE);"
"          /* mapLocal */"
"          /* unrolling loop of 1 */"
"          /* mapLocal */"
"          /* unrolling loop of 1 */"
"          /* oclReduceSeq */"
"          {"
"            float x1481[32];"
"            /* mapSeq */"
"            /* unrolling loop of 8 */"
"            /* mapSeq */"
"            /* unrolling loop of 4 */"
"            x1481[0] = x1428[32 * get_local_id(0)];"
"            x1481[1] = x1428[1 + (32 * get_local_id(0))];"
"            x1481[2] = x1428[2 + (32 * get_local_id(0))];"
"            x1481[3] = x1428[3 + (32 * get_local_id(0))];"
"            /* mapSeq */"
"            /* unrolling loop of 4 */"
"            x1481[4] = x1428[4 + (32 * get_local_id(0))];"
"            x1481[5] = x1428[5 + (32 * get_local_id(0))];"
"            x1481[6] = x1428[6 + (32 * get_local_id(0))];"
"            x1481[7] = x1428[7 + (32 * get_local_id(0))];"
"            /* mapSeq */"
"            /* unrolling loop of 4 */"
"            x1481[8] = x1428[8 + (32 * get_local_id(0))];"
"            x1481[9] = x1428[9 + (32 * get_local_id(0))];"
"            x1481[10] = x1428[10 + (32 * get_local_id(0))];"
"            x1481[11] = x1428[11 + (32 * get_local_id(0))];"
"            /* mapSeq */"
"            /* unrolling loop of 4 */"
"            x1481[12] = x1428[12 + (32 * get_local_id(0))];"
"            x1481[13] = x1428[13 + (32 * get_local_id(0))];"
"            x1481[14] = x1428[14 + (32 * get_local_id(0))];"
"            x1481[15] = x1428[15 + (32 * get_local_id(0))];"
"            /* mapSeq */"
"            /* unrolling loop of 4 */"
"            x1481[16] = x1428[16 + (32 * get_local_id(0))];"
"            x1481[17] = x1428[17 + (32 * get_local_id(0))];"
"            x1481[18] = x1428[18 + (32 * get_local_id(0))];"
"            x1481[19] = x1428[19 + (32 * get_local_id(0))];"
"            /* mapSeq */"
"            /* unrolling loop of 4 */"
"            x1481[20] = x1428[20 + (32 * get_local_id(0))];"
"            x1481[21] = x1428[21 + (32 * get_local_id(0))];"
"            x1481[22] = x1428[22 + (32 * get_local_id(0))];"
"            x1481[23] = x1428[23 + (32 * get_local_id(0))];"
"            /* mapSeq */"
"            /* unrolling loop of 4 */"
"            x1481[24] = x1428[24 + (32 * get_local_id(0))];"
"            x1481[25] = x1428[25 + (32 * get_local_id(0))];"
"            x1481[26] = x1428[26 + (32 * get_local_id(0))];"
"            x1481[27] = x1428[27 + (32 * get_local_id(0))];"
"            /* mapSeq */"
"            /* unrolling loop of 4 */"
"            x1481[28] = x1428[28 + (32 * get_local_id(0))];"
"            x1481[29] = x1428[29 + (32 * get_local_id(0))];"
"            x1481[30] = x1428[30 + (32 * get_local_id(0))];"
"            x1481[31] = x1428[31 + (32 * get_local_id(0))];"
"            for (int i_1648 = 0; i_1648 < 16; i_1648 = 1 + i_1648) {"
"              {"
"                struct Record_8_float_4_float x1531;"
"                /* mapSeq */"
"                /* unrolling loop of 8 */"
"                x1531._fst[0] = x1577[0]._fst[(8 * get_local_id(1)) + (64 * i_1648)];"
"                x1531._fst[1] = x1577[0]._fst[(1 + (8 * get_local_id(1))) + (64 * i_1648)];"
"                x1531._fst[2] = x1577[0]._fst[(2 + (8 * get_local_id(1))) + (64 * i_1648)];"
"                x1531._fst[3] = x1577[0]._fst[(3 + (8 * get_local_id(1))) + (64 * i_1648)];"
"                x1531._fst[4] = x1577[0]._fst[(4 + (8 * get_local_id(1))) + (64 * i_1648)];"
"                x1531._fst[5] = x1577[0]._fst[(5 + (8 * get_local_id(1))) + (64 * i_1648)];"
"                x1531._fst[6] = x1577[0]._fst[(6 + (8 * get_local_id(1))) + (64 * i_1648)];"
"                x1531._fst[7] = x1577[0]._fst[(7 + (8 * get_local_id(1))) + (64 * i_1648)];"
"                /* mapSeq */"
"                /* unrolling loop of 4 */"
"                x1531._snd[0] = x1577[0]._snd[(4 * get_local_id(0)) + (128 * i_1648)];"
"                x1531._snd[1] = x1577[0]._snd[(1 + (4 * get_local_id(0))) + (128 * i_1648)];"
"                x1531._snd[2] = x1577[0]._snd[(2 + (4 * get_local_id(0))) + (128 * i_1648)];"
"                x1531._snd[3] = x1577[0]._snd[(3 + (4 * get_local_id(0))) + (128 * i_1648)];"
"                /* mapSeq */"
"                /* unrolling loop of 8 */"
"                /* mapSeq */"
"                /* unrolling loop of 4 */"
"                x1481[0] = x1481[0] + (x1531._fst[0] * x1531._snd[0]);"
"                x1481[1] = x1481[1] + (x1531._fst[0] * x1531._snd[1]);"
"                x1481[2] = x1481[2] + (x1531._fst[0] * x1531._snd[2]);"
"                x1481[3] = x1481[3] + (x1531._fst[0] * x1531._snd[3]);"
"                /* mapSeq */"
"                /* unrolling loop of 4 */"
"                x1481[4] = x1481[4] + (x1531._fst[1] * x1531._snd[0]);"
"                x1481[5] = x1481[5] + (x1531._fst[1] * x1531._snd[1]);"
"                x1481[6] = x1481[6] + (x1531._fst[1] * x1531._snd[2]);"
"                x1481[7] = x1481[7] + (x1531._fst[1] * x1531._snd[3]);"
"                /* mapSeq */"
"                /* unrolling loop of 4 */"
"                x1481[8] = x1481[8] + (x1531._fst[2] * x1531._snd[0]);"
"                x1481[9] = x1481[9] + (x1531._fst[2] * x1531._snd[1]);"
"                x1481[10] = x1481[10] + (x1531._fst[2] * x1531._snd[2]);"
"                x1481[11] = x1481[11] + (x1531._fst[2] * x1531._snd[3]);"
"                /* mapSeq */"
"                /* unrolling loop of 4 */"
"                x1481[12] = x1481[12] + (x1531._fst[3] * x1531._snd[0]);"
"                x1481[13] = x1481[13] + (x1531._fst[3] * x1531._snd[1]);"
"                x1481[14] = x1481[14] + (x1531._fst[3] * x1531._snd[2]);"
"                x1481[15] = x1481[15] + (x1531._fst[3] * x1531._snd[3]);"
"                /* mapSeq */"
"                /* unrolling loop of 4 */"
"                x1481[16] = x1481[16] + (x1531._fst[4] * x1531._snd[0]);"
"                x1481[17] = x1481[17] + (x1531._fst[4] * x1531._snd[1]);"
"                x1481[18] = x1481[18] + (x1531._fst[4] * x1531._snd[2]);"
"                x1481[19] = x1481[19] + (x1531._fst[4] * x1531._snd[3]);"
"                /* mapSeq */"
"                /* unrolling loop of 4 */"
"                x1481[20] = x1481[20] + (x1531._fst[5] * x1531._snd[0]);"
"                x1481[21] = x1481[21] + (x1531._fst[5] * x1531._snd[1]);"
"                x1481[22] = x1481[22] + (x1531._fst[5] * x1531._snd[2]);"
"                x1481[23] = x1481[23] + (x1531._fst[5] * x1531._snd[3]);"
"                /* mapSeq */"
"                /* unrolling loop of 4 */"
"                x1481[24] = x1481[24] + (x1531._fst[6] * x1531._snd[0]);"
"                x1481[25] = x1481[25] + (x1531._fst[6] * x1531._snd[1]);"
"                x1481[26] = x1481[26] + (x1531._fst[6] * x1531._snd[2]);"
"                x1481[27] = x1481[27] + (x1531._fst[6] * x1531._snd[3]);"
"                /* mapSeq */"
"                /* unrolling loop of 4 */"
"                x1481[28] = x1481[28] + (x1531._fst[7] * x1531._snd[0]);"
"                x1481[29] = x1481[29] + (x1531._fst[7] * x1531._snd[1]);"
"                x1481[30] = x1481[30] + (x1531._fst[7] * x1531._snd[2]);"
"                x1481[31] = x1481[31] + (x1531._fst[7] * x1531._snd[3]);"
"              }"
"              "
"            }"
"            "
"            /* mapSeq */"
"            /* unrolling loop of 8 */"
"            /* mapSeq */"
"            /* unrolling loop of 4 */"
"            x1428[32 * get_local_id(0)] = x1481[0];"
"            x1428[1 + (32 * get_local_id(0))] = x1481[1];"
"            x1428[2 + (32 * get_local_id(0))] = x1481[2];"
"            x1428[3 + (32 * get_local_id(0))] = x1481[3];"
"            /* mapSeq */"
"            /* unrolling loop of 4 */"
"            x1428[4 + (32 * get_local_id(0))] = x1481[4];"
"            x1428[5 + (32 * get_local_id(0))] = x1481[5];"
"            x1428[6 + (32 * get_local_id(0))] = x1481[6];"
"            x1428[7 + (32 * get_local_id(0))] = x1481[7];"
"            /* mapSeq */"
"            /* unrolling loop of 4 */"
"            x1428[8 + (32 * get_local_id(0))] = x1481[8];"
"            x1428[9 + (32 * get_local_id(0))] = x1481[9];"
"            x1428[10 + (32 * get_local_id(0))] = x1481[10];"
"            x1428[11 + (32 * get_local_id(0))] = x1481[11];"
"            /* mapSeq */"
"            /* unrolling loop of 4 */"
"            x1428[12 + (32 * get_local_id(0))] = x1481[12];"
"            x1428[13 + (32 * get_local_id(0))] = x1481[13];"
"            x1428[14 + (32 * get_local_id(0))] = x1481[14];"
"            x1428[15 + (32 * get_local_id(0))] = x1481[15];"
"            /* mapSeq */"
"            /* unrolling loop of 4 */"
"            x1428[16 + (32 * get_local_id(0))] = x1481[16];"
"            x1428[17 + (32 * get_local_id(0))] = x1481[17];"
"            x1428[18 + (32 * get_local_id(0))] = x1481[18];"
"            x1428[19 + (32 * get_local_id(0))] = x1481[19];"
"            /* mapSeq */"
"            /* unrolling loop of 4 */"
"            x1428[20 + (32 * get_local_id(0))] = x1481[20];"
"            x1428[21 + (32 * get_local_id(0))] = x1481[21];"
"            x1428[22 + (32 * get_local_id(0))] = x1481[22];"
"            x1428[23 + (32 * get_local_id(0))] = x1481[23];"
"            /* mapSeq */"
"            /* unrolling loop of 4 */"
"            x1428[24 + (32 * get_local_id(0))] = x1481[24];"
"            x1428[25 + (32 * get_local_id(0))] = x1481[25];"
"            x1428[26 + (32 * get_local_id(0))] = x1481[26];"
"            x1428[27 + (32 * get_local_id(0))] = x1481[27];"
"            /* mapSeq */"
"            /* unrolling loop of 4 */"
"            x1428[28 + (32 * get_local_id(0))] = x1481[28];"
"            x1428[29 + (32 * get_local_id(0))] = x1481[29];"
"            x1428[30 + (32 * get_local_id(0))] = x1481[30];"
"            x1428[31 + (32 * get_local_id(0))] = x1481[31];"
"          }"
"          "
"          barrier(CLK_LOCAL_MEM_FENCE);"
"        }"
"        "
"        /* mapLocal */"
"        /* unrolling loop of 1 */"
"        /* mapLocal */"
"        /* unrolling loop of 1 */"
"        /* mapSeq */"
"        /* unrolling loop of 8 */"
"        /* mapSeq */"
"        /* unrolling loop of 1 */"
"        vstore4((float4)(x1428[32 * get_local_id(0)], x1428[1 + (32 * get_local_id(0))], x1428[2 + (32 * get_local_id(0))], x1428[3 + (32 * get_local_id(0))]), 0, &output[(((4 * get_local_id(0)) + ((8 * n205) * get_local_id(1))) + ((64 * n205) * wg_id_1641)) + (128 * wg_id_1642)]);"
"        /* mapSeq */"
"        /* unrolling loop of 1 */"
"        vstore4((float4)(x1428[4 + (32 * get_local_id(0))], x1428[5 + (32 * get_local_id(0))], x1428[6 + (32 * get_local_id(0))], x1428[7 + (32 * get_local_id(0))]), 0, &output[(((n205 + (4 * get_local_id(0))) + ((8 * n205) * get_local_id(1))) + ((64 * n205) * wg_id_1641)) + (128 * wg_id_1642)]);"
"        /* mapSeq */"
"        /* unrolling loop of 1 */"
"        vstore4((float4)(x1428[8 + (32 * get_local_id(0))], x1428[9 + (32 * get_local_id(0))], x1428[10 + (32 * get_local_id(0))], x1428[11 + (32 * get_local_id(0))]), 0, &output[((((2 * n205) + (4 * get_local_id(0))) + ((8 * n205) * get_local_id(1))) + ((64 * n205) * wg_id_1641)) + (128 * wg_id_1642)]);"
"        /* mapSeq */"
"        /* unrolling loop of 1 */"
"        vstore4((float4)(x1428[12 + (32 * get_local_id(0))], x1428[13 + (32 * get_local_id(0))], x1428[14 + (32 * get_local_id(0))], x1428[15 + (32 * get_local_id(0))]), 0, &output[((((3 * n205) + (4 * get_local_id(0))) + ((8 * n205) * get_local_id(1))) + ((64 * n205) * wg_id_1641)) + (128 * wg_id_1642)]);"
"        /* mapSeq */"
"        /* unrolling loop of 1 */"
"        vstore4((float4)(x1428[16 + (32 * get_local_id(0))], x1428[17 + (32 * get_local_id(0))], x1428[18 + (32 * get_local_id(0))], x1428[19 + (32 * get_local_id(0))]), 0, &output[((((4 * n205) + (4 * get_local_id(0))) + ((8 * n205) * get_local_id(1))) + ((64 * n205) * wg_id_1641)) + (128 * wg_id_1642)]);"
"        /* mapSeq */"
"        /* unrolling loop of 1 */"
"        vstore4((float4)(x1428[20 + (32 * get_local_id(0))], x1428[21 + (32 * get_local_id(0))], x1428[22 + (32 * get_local_id(0))], x1428[23 + (32 * get_local_id(0))]), 0, &output[((((4 * get_local_id(0)) + (5 * n205)) + ((8 * n205) * get_local_id(1))) + ((64 * n205) * wg_id_1641)) + (128 * wg_id_1642)]);"
"        /* mapSeq */"
"        /* unrolling loop of 1 */"
"        vstore4((float4)(x1428[24 + (32 * get_local_id(0))], x1428[25 + (32 * get_local_id(0))], x1428[26 + (32 * get_local_id(0))], x1428[27 + (32 * get_local_id(0))]), 0, &output[((((4 * get_local_id(0)) + (6 * n205)) + ((8 * n205) * get_local_id(1))) + ((64 * n205) * wg_id_1641)) + (128 * wg_id_1642)]);"
"        /* mapSeq */"
"        /* unrolling loop of 1 */"
"        vstore4((float4)(x1428[28 + (32 * get_local_id(0))], x1428[29 + (32 * get_local_id(0))], x1428[30 + (32 * get_local_id(0))], x1428[31 + (32 * get_local_id(0))]), 0, &output[((((4 * get_local_id(0)) + (7 * n205)) + ((8 * n205) * get_local_id(1))) + ((64 * n205) * wg_id_1641)) + (128 * wg_id_1642)]);"
"      }"
"      "
"    }"
"    "
"  }"
"  "
"}"
"";

#define loadKernel(ctx, id)\
  loadKernelFromSource(ctx, #id, id##_source, sizeof(id##_source) - 1)

#include "ocl/ocl.h"
struct fun_t {
  Kernel k0;
};

typedef struct fun_t fun_t;

void fun_init(Context ctx, fun_t* self){
  (*self).k0 = loadKernel(ctx, k0);
}

void fun_destroy(Context ctx, fun_t* self){
  destroyKernel(ctx, (*self).k0);
}

void fun_run(Context ctx, fun_t* self, Buffer moutput, int n204, int n205, int n206, Buffer me207, Buffer me208){
  {
    DeviceBuffer b0 = deviceBufferSync(ctx, moutput, n204 * (n205 * sizeof(float)), DEVICE_WRITE);
    DeviceBuffer b4 = deviceBufferSync(ctx, me207, n206 * (n204 * sizeof(float)), DEVICE_READ);
    DeviceBuffer b5 = deviceBufferSync(ctx, me208, n206 * (n205 * sizeof(float)), DEVICE_READ);
    const size_t global_size[3] = (const size_t[3]){256, 128, 1};
    const size_t local_size[3] = (const size_t[3]){32, 8, 1};
    const KernelArg args[7] = (const KernelArg[7]){KARG(b0), KARG(n205), KARG(n204), KARG(n206), KARG(b4), KARG(b5), LARG((1 <= (n206 / 16) ? 1 : (n206 / 16)) * ((16 * (64 * sizeof(float))) + (16 * (128 * sizeof(float)))))};
    launchKernel(ctx, (*self).k0, global_size, local_size, 7, args);
  }

}

void fun_init_run(Context ctx, Buffer moutput, int n204, int n205, int n206, Buffer me207, Buffer me208){
  fun_t fun;
  fun_init(ctx, &fun);
  fun_run(ctx, &fun, moutput, n204, n205, n206, me207, me208);
  fun_destroy(ctx, &fun);
}




int main(int argc, char** argv) {
  Context ctx = createDefaultContext();
  fun_t fun;
  fun_init(ctx, &fun);


const int N = 1024;
const int M = 1024;
const int O = 1024;

srand(time(NULL));

Buffer inputA = createBuffer(ctx, N * M * sizeof(float), HOST_WRITE | DEVICE_READ);
Buffer inputB = createBuffer(ctx, M * O * sizeof(float), HOST_WRITE | DEVICE_READ);
Buffer outputC = createBuffer(ctx, N * O *  sizeof(float), HOST_READ | DEVICE_WRITE);

float* inA = hostBufferSync(ctx, inputA, N * M * sizeof(float), HOST_WRITE);
for (int i = 0; i < N * M ; i++) {
  //inA[i] = (float)(rand());
  inA[i] = (float)(i+1);
}

float* inB = hostBufferSync(ctx, inputB, M * O * sizeof(float), HOST_WRITE);
for (int i = 0; i < M * O; i++) {
  //inB[i] = (float)(rand());
  inB[i] = (float)(i+1);
}



  int iterations = atoi(argv[1]);
  for (int sample = 0; sample < iterations; sample++) {

fun_run(ctx, &fun, outputC, M, N, O, inputA, inputB);

  }

// TODO: could check output here

destroyBuffer(ctx, inputA);
destroyBuffer(ctx, inputB);
destroyBuffer(ctx, outputC);

  fun_destroy(ctx, &fun);
  destroyContext(ctx);
  return EXIT_SUCCESS;
}

