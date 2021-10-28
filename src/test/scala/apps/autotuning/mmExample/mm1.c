const char k0_source[] =
""
"struct Record_2_float_8_float {"
"  float _fst[2];"
"  float _snd[8];"
"};"
""
"struct Record_2_512_float_2_16_float {"
"  float _fst[1024];"
"  float _snd[32];"
"};"
""
""
"__kernel __attribute__ ((reqd_work_group_size(1, 256, 1)))"
"void k0(global float* restrict output, int n205, int n204, int n206, const global float* restrict e207, const global float* restrict e208, local struct Record_2_512_float_2_16_float* restrict x1550){"
"  /* Start of moved local vars */"
"  /* End of moved local vars */"
"  /* mapWorkGroup */"
"  for (int wg_id_1614 = get_group_id(1); wg_id_1614 < (n204 / 512); wg_id_1614 = 4 + wg_id_1614) {"
"    /* mapWorkGroup */"
"    for (int wg_id_1615 = get_group_id(0); wg_id_1615 < (n205 / 16); wg_id_1615 = 128 + wg_id_1615) {"
"      /* oclReduceSeq */"
"      {"
"        float x1401[32];"
"        /* mapLocal */"
"        /* unrolling loop of 1 */"
"        /* mapLocal */"
"        /* unrolling loop of 2 */"
"        /* mapSeq */"
"        /* unrolling loop of 2 */"
"        /* mapSeq */"
"        /* unrolling loop of 8 */"
"        x1401[16 * get_local_id(0)] = 0.0f;"
"        x1401[1 + (16 * get_local_id(0))] = 0.0f;"
"        x1401[2 + (16 * get_local_id(0))] = 0.0f;"
"        x1401[3 + (16 * get_local_id(0))] = 0.0f;"
"        x1401[4 + (16 * get_local_id(0))] = 0.0f;"
"        x1401[5 + (16 * get_local_id(0))] = 0.0f;"
"        x1401[6 + (16 * get_local_id(0))] = 0.0f;"
"        x1401[7 + (16 * get_local_id(0))] = 0.0f;"
"        /* mapSeq */"
"        /* unrolling loop of 8 */"
"        x1401[8 + (16 * get_local_id(0))] = 0.0f;"
"        x1401[9 + (16 * get_local_id(0))] = 0.0f;"
"        x1401[10 + (16 * get_local_id(0))] = 0.0f;"
"        x1401[11 + (16 * get_local_id(0))] = 0.0f;"
"        x1401[12 + (16 * get_local_id(0))] = 0.0f;"
"        x1401[13 + (16 * get_local_id(0))] = 0.0f;"
"        x1401[14 + (16 * get_local_id(0))] = 0.0f;"
"        x1401[15 + (16 * get_local_id(0))] = 0.0f;"
"        /* mapSeq */"
"        /* unrolling loop of 2 */"
"        /* mapSeq */"
"        /* unrolling loop of 8 */"
"        x1401[16 + (16 * get_local_id(0))] = 0.0f;"
"        x1401[17 + (16 * get_local_id(0))] = 0.0f;"
"        x1401[18 + (16 * get_local_id(0))] = 0.0f;"
"        x1401[19 + (16 * get_local_id(0))] = 0.0f;"
"        x1401[20 + (16 * get_local_id(0))] = 0.0f;"
"        x1401[21 + (16 * get_local_id(0))] = 0.0f;"
"        x1401[22 + (16 * get_local_id(0))] = 0.0f;"
"        x1401[23 + (16 * get_local_id(0))] = 0.0f;"
"        /* mapSeq */"
"        /* unrolling loop of 8 */"
"        x1401[24 + (16 * get_local_id(0))] = 0.0f;"
"        x1401[25 + (16 * get_local_id(0))] = 0.0f;"
"        x1401[26 + (16 * get_local_id(0))] = 0.0f;"
"        x1401[27 + (16 * get_local_id(0))] = 0.0f;"
"        x1401[28 + (16 * get_local_id(0))] = 0.0f;"
"        x1401[29 + (16 * get_local_id(0))] = 0.0f;"
"        x1401[30 + (16 * get_local_id(0))] = 0.0f;"
"        x1401[31 + (16 * get_local_id(0))] = 0.0f;"
"        for (int i_1616 = 0; i_1616 < (n206 / 2); i_1616 = 1 + i_1616) {"
"          /* mapLocal */"
"          for (int l_id_1617 = get_local_id(1); l_id_1617 < 128; l_id_1617 = 256 + l_id_1617) {"
"            /* mapLocal */"
"            for (int l_id_1618 = get_local_id(0); l_id_1618 < 2; l_id_1618 = 1 + l_id_1618) {"
"              vstore4(vload4(0, &e207[(((((4 * l_id_1618) + (8 * l_id_1617)) % 512) + ((2 * i_1616) * n204)) + (512 * wg_id_1614)) + (n204 * ((l_id_1618 + (2 * l_id_1617)) / 128))]), 0, &x1550[0]._fst[(4 * l_id_1618) + (8 * l_id_1617)]);"
"            }"
"            "
"          }"
"          "
"          /* mapLocal */"
"          for (int l_id_1619 = get_local_id(1); l_id_1619 < 2; l_id_1619 = 256 + l_id_1619) {"
"            /* mapLocal */"
"            for (int l_id_1620 = get_local_id(0); l_id_1620 < 4; l_id_1620 = 1 + l_id_1620) {"
"              vstore4(vload4(0, &e208[((((2 * i_1616) * n205) + (4 * l_id_1620)) + (16 * wg_id_1615)) + (l_id_1619 * n205)]), 0, &x1550[0]._snd[(4 * l_id_1620) + (16 * l_id_1619)]);"
"            }"
"            "
"          }"
"          "
"          barrier(CLK_LOCAL_MEM_FENCE);"
"          /* mapLocal */"
"          /* unrolling loop of 1 */"
"          /* mapLocal */"
"          /* unrolling loop of 2 */"
"          /* oclReduceSeq */"
"          {"
"            float x1454[16];"
"            /* mapSeq */"
"            /* unrolling loop of 2 */"
"            /* mapSeq */"
"            /* unrolling loop of 8 */"
"            x1454[0] = x1401[16 * get_local_id(0)];"
"            x1454[1] = x1401[1 + (16 * get_local_id(0))];"
"            x1454[2] = x1401[2 + (16 * get_local_id(0))];"
"            x1454[3] = x1401[3 + (16 * get_local_id(0))];"
"            x1454[4] = x1401[4 + (16 * get_local_id(0))];"
"            x1454[5] = x1401[5 + (16 * get_local_id(0))];"
"            x1454[6] = x1401[6 + (16 * get_local_id(0))];"
"            x1454[7] = x1401[7 + (16 * get_local_id(0))];"
"            /* mapSeq */"
"            /* unrolling loop of 8 */"
"            x1454[8] = x1401[8 + (16 * get_local_id(0))];"
"            x1454[9] = x1401[9 + (16 * get_local_id(0))];"
"            x1454[10] = x1401[10 + (16 * get_local_id(0))];"
"            x1454[11] = x1401[11 + (16 * get_local_id(0))];"
"            x1454[12] = x1401[12 + (16 * get_local_id(0))];"
"            x1454[13] = x1401[13 + (16 * get_local_id(0))];"
"            x1454[14] = x1401[14 + (16 * get_local_id(0))];"
"            x1454[15] = x1401[15 + (16 * get_local_id(0))];"
"            for (int i_1621 = 0; i_1621 < 2; i_1621 = 1 + i_1621) {"
"              {"
"                struct Record_2_float_8_float x1504;"
"                /* mapSeq */"
"                /* unrolling loop of 2 */"
"                x1504._fst[0] = x1550[0]._fst[(2 * get_local_id(1)) + (512 * i_1621)];"
"                x1504._fst[1] = x1550[0]._fst[(1 + (2 * get_local_id(1))) + (512 * i_1621)];"
"                /* mapSeq */"
"                /* unrolling loop of 8 */"
"                x1504._snd[0] = x1550[0]._snd[(8 * get_local_id(0)) + (16 * i_1621)];"
"                x1504._snd[1] = x1550[0]._snd[(1 + (8 * get_local_id(0))) + (16 * i_1621)];"
"                x1504._snd[2] = x1550[0]._snd[(2 + (8 * get_local_id(0))) + (16 * i_1621)];"
"                x1504._snd[3] = x1550[0]._snd[(3 + (8 * get_local_id(0))) + (16 * i_1621)];"
"                x1504._snd[4] = x1550[0]._snd[(4 + (8 * get_local_id(0))) + (16 * i_1621)];"
"                x1504._snd[5] = x1550[0]._snd[(5 + (8 * get_local_id(0))) + (16 * i_1621)];"
"                x1504._snd[6] = x1550[0]._snd[(6 + (8 * get_local_id(0))) + (16 * i_1621)];"
"                x1504._snd[7] = x1550[0]._snd[(7 + (8 * get_local_id(0))) + (16 * i_1621)];"
"                /* mapSeq */"
"                /* unrolling loop of 2 */"
"                /* mapSeq */"
"                /* unrolling loop of 8 */"
"                x1454[0] = x1454[0] + (x1504._fst[0] * x1504._snd[0]);"
"                x1454[1] = x1454[1] + (x1504._fst[0] * x1504._snd[1]);"
"                x1454[2] = x1454[2] + (x1504._fst[0] * x1504._snd[2]);"
"                x1454[3] = x1454[3] + (x1504._fst[0] * x1504._snd[3]);"
"                x1454[4] = x1454[4] + (x1504._fst[0] * x1504._snd[4]);"
"                x1454[5] = x1454[5] + (x1504._fst[0] * x1504._snd[5]);"
"                x1454[6] = x1454[6] + (x1504._fst[0] * x1504._snd[6]);"
"                x1454[7] = x1454[7] + (x1504._fst[0] * x1504._snd[7]);"
"                /* mapSeq */"
"                /* unrolling loop of 8 */"
"                x1454[8] = x1454[8] + (x1504._fst[1] * x1504._snd[0]);"
"                x1454[9] = x1454[9] + (x1504._fst[1] * x1504._snd[1]);"
"                x1454[10] = x1454[10] + (x1504._fst[1] * x1504._snd[2]);"
"                x1454[11] = x1454[11] + (x1504._fst[1] * x1504._snd[3]);"
"                x1454[12] = x1454[12] + (x1504._fst[1] * x1504._snd[4]);"
"                x1454[13] = x1454[13] + (x1504._fst[1] * x1504._snd[5]);"
"                x1454[14] = x1454[14] + (x1504._fst[1] * x1504._snd[6]);"
"                x1454[15] = x1454[15] + (x1504._fst[1] * x1504._snd[7]);"
"              }"
"              "
"            }"
"            "
"            /* mapSeq */"
"            /* unrolling loop of 2 */"
"            /* mapSeq */"
"            /* unrolling loop of 8 */"
"            x1401[16 * get_local_id(0)] = x1454[0];"
"            x1401[1 + (16 * get_local_id(0))] = x1454[1];"
"            x1401[2 + (16 * get_local_id(0))] = x1454[2];"
"            x1401[3 + (16 * get_local_id(0))] = x1454[3];"
"            x1401[4 + (16 * get_local_id(0))] = x1454[4];"
"            x1401[5 + (16 * get_local_id(0))] = x1454[5];"
"            x1401[6 + (16 * get_local_id(0))] = x1454[6];"
"            x1401[7 + (16 * get_local_id(0))] = x1454[7];"
"            /* mapSeq */"
"            /* unrolling loop of 8 */"
"            x1401[8 + (16 * get_local_id(0))] = x1454[8];"
"            x1401[9 + (16 * get_local_id(0))] = x1454[9];"
"            x1401[10 + (16 * get_local_id(0))] = x1454[10];"
"            x1401[11 + (16 * get_local_id(0))] = x1454[11];"
"            x1401[12 + (16 * get_local_id(0))] = x1454[12];"
"            x1401[13 + (16 * get_local_id(0))] = x1454[13];"
"            x1401[14 + (16 * get_local_id(0))] = x1454[14];"
"            x1401[15 + (16 * get_local_id(0))] = x1454[15];"
"          }"
"          "
"          /* oclReduceSeq */"
"          {"
"            float x1454[16];"
"            /* mapSeq */"
"            /* unrolling loop of 2 */"
"            /* mapSeq */"
"            /* unrolling loop of 8 */"
"            x1454[0] = x1401[16 + (16 * get_local_id(0))];"
"            x1454[1] = x1401[17 + (16 * get_local_id(0))];"
"            x1454[2] = x1401[18 + (16 * get_local_id(0))];"
"            x1454[3] = x1401[19 + (16 * get_local_id(0))];"
"            x1454[4] = x1401[20 + (16 * get_local_id(0))];"
"            x1454[5] = x1401[21 + (16 * get_local_id(0))];"
"            x1454[6] = x1401[22 + (16 * get_local_id(0))];"
"            x1454[7] = x1401[23 + (16 * get_local_id(0))];"
"            /* mapSeq */"
"            /* unrolling loop of 8 */"
"            x1454[8] = x1401[24 + (16 * get_local_id(0))];"
"            x1454[9] = x1401[25 + (16 * get_local_id(0))];"
"            x1454[10] = x1401[26 + (16 * get_local_id(0))];"
"            x1454[11] = x1401[27 + (16 * get_local_id(0))];"
"            x1454[12] = x1401[28 + (16 * get_local_id(0))];"
"            x1454[13] = x1401[29 + (16 * get_local_id(0))];"
"            x1454[14] = x1401[30 + (16 * get_local_id(0))];"
"            x1454[15] = x1401[31 + (16 * get_local_id(0))];"
"            for (int i_1622 = 0; i_1622 < 2; i_1622 = 1 + i_1622) {"
"              {"
"                struct Record_2_float_8_float x1504;"
"                /* mapSeq */"
"                /* unrolling loop of 2 */"
"                x1504._fst[0] = x1550[0]._fst[(2 * get_local_id(1)) + (512 * i_1622)];"
"                x1504._fst[1] = x1550[0]._fst[(1 + (2 * get_local_id(1))) + (512 * i_1622)];"
"                /* mapSeq */"
"                /* unrolling loop of 8 */"
"                x1504._snd[0] = x1550[0]._snd[(8 + (8 * get_local_id(0))) + (16 * i_1622)];"
"                x1504._snd[1] = x1550[0]._snd[(9 + (8 * get_local_id(0))) + (16 * i_1622)];"
"                x1504._snd[2] = x1550[0]._snd[(10 + (8 * get_local_id(0))) + (16 * i_1622)];"
"                x1504._snd[3] = x1550[0]._snd[(11 + (8 * get_local_id(0))) + (16 * i_1622)];"
"                x1504._snd[4] = x1550[0]._snd[(12 + (8 * get_local_id(0))) + (16 * i_1622)];"
"                x1504._snd[5] = x1550[0]._snd[(13 + (8 * get_local_id(0))) + (16 * i_1622)];"
"                x1504._snd[6] = x1550[0]._snd[(14 + (8 * get_local_id(0))) + (16 * i_1622)];"
"                x1504._snd[7] = x1550[0]._snd[(15 + (8 * get_local_id(0))) + (16 * i_1622)];"
"                /* mapSeq */"
"                /* unrolling loop of 2 */"
"                /* mapSeq */"
"                /* unrolling loop of 8 */"
"                x1454[0] = x1454[0] + (x1504._fst[0] * x1504._snd[0]);"
"                x1454[1] = x1454[1] + (x1504._fst[0] * x1504._snd[1]);"
"                x1454[2] = x1454[2] + (x1504._fst[0] * x1504._snd[2]);"
"                x1454[3] = x1454[3] + (x1504._fst[0] * x1504._snd[3]);"
"                x1454[4] = x1454[4] + (x1504._fst[0] * x1504._snd[4]);"
"                x1454[5] = x1454[5] + (x1504._fst[0] * x1504._snd[5]);"
"                x1454[6] = x1454[6] + (x1504._fst[0] * x1504._snd[6]);"
"                x1454[7] = x1454[7] + (x1504._fst[0] * x1504._snd[7]);"
"                /* mapSeq */"
"                /* unrolling loop of 8 */"
"                x1454[8] = x1454[8] + (x1504._fst[1] * x1504._snd[0]);"
"                x1454[9] = x1454[9] + (x1504._fst[1] * x1504._snd[1]);"
"                x1454[10] = x1454[10] + (x1504._fst[1] * x1504._snd[2]);"
"                x1454[11] = x1454[11] + (x1504._fst[1] * x1504._snd[3]);"
"                x1454[12] = x1454[12] + (x1504._fst[1] * x1504._snd[4]);"
"                x1454[13] = x1454[13] + (x1504._fst[1] * x1504._snd[5]);"
"                x1454[14] = x1454[14] + (x1504._fst[1] * x1504._snd[6]);"
"                x1454[15] = x1454[15] + (x1504._fst[1] * x1504._snd[7]);"
"              }"
"              "
"            }"
"            "
"            /* mapSeq */"
"            /* unrolling loop of 2 */"
"            /* mapSeq */"
"            /* unrolling loop of 8 */"
"            x1401[16 + (16 * get_local_id(0))] = x1454[0];"
"            x1401[17 + (16 * get_local_id(0))] = x1454[1];"
"            x1401[18 + (16 * get_local_id(0))] = x1454[2];"
"            x1401[19 + (16 * get_local_id(0))] = x1454[3];"
"            x1401[20 + (16 * get_local_id(0))] = x1454[4];"
"            x1401[21 + (16 * get_local_id(0))] = x1454[5];"
"            x1401[22 + (16 * get_local_id(0))] = x1454[6];"
"            x1401[23 + (16 * get_local_id(0))] = x1454[7];"
"            /* mapSeq */"
"            /* unrolling loop of 8 */"
"            x1401[24 + (16 * get_local_id(0))] = x1454[8];"
"            x1401[25 + (16 * get_local_id(0))] = x1454[9];"
"            x1401[26 + (16 * get_local_id(0))] = x1454[10];"
"            x1401[27 + (16 * get_local_id(0))] = x1454[11];"
"            x1401[28 + (16 * get_local_id(0))] = x1454[12];"
"            x1401[29 + (16 * get_local_id(0))] = x1454[13];"
"            x1401[30 + (16 * get_local_id(0))] = x1454[14];"
"            x1401[31 + (16 * get_local_id(0))] = x1454[15];"
"          }"
"          "
"          barrier(CLK_LOCAL_MEM_FENCE);"
"        }"
"        "
"        /* mapLocal */"
"        /* unrolling loop of 1 */"
"        /* mapLocal */"
"        /* unrolling loop of 2 */"
"        /* mapSeq */"
"        /* unrolling loop of 2 */"
"        /* mapSeq */"
"        /* unrolling loop of 2 */"
"        vstore4((float4)(x1401[16 * get_local_id(0)], x1401[1 + (16 * get_local_id(0))], x1401[2 + (16 * get_local_id(0))], x1401[3 + (16 * get_local_id(0))]), 0, &output[((((2 * n205) * get_local_id(1)) + (8 * get_local_id(0))) + (16 * wg_id_1615)) + ((512 * n205) * wg_id_1614)]);"
"        vstore4((float4)(x1401[4 + (16 * get_local_id(0))], x1401[5 + (16 * get_local_id(0))], x1401[6 + (16 * get_local_id(0))], x1401[7 + (16 * get_local_id(0))]), 0, &output[(((4 + ((2 * n205) * get_local_id(1))) + (8 * get_local_id(0))) + (16 * wg_id_1615)) + ((512 * n205) * wg_id_1614)]);"
"        /* mapSeq */"
"        /* unrolling loop of 2 */"
"        vstore4((float4)(x1401[8 + (16 * get_local_id(0))], x1401[9 + (16 * get_local_id(0))], x1401[10 + (16 * get_local_id(0))], x1401[11 + (16 * get_local_id(0))]), 0, &output[(((n205 + ((2 * n205) * get_local_id(1))) + (8 * get_local_id(0))) + (16 * wg_id_1615)) + ((512 * n205) * wg_id_1614)]);"
"        vstore4((float4)(x1401[12 + (16 * get_local_id(0))], x1401[13 + (16 * get_local_id(0))], x1401[14 + (16 * get_local_id(0))], x1401[15 + (16 * get_local_id(0))]), 0, &output[((((4 + n205) + ((2 * n205) * get_local_id(1))) + (8 * get_local_id(0))) + (16 * wg_id_1615)) + ((512 * n205) * wg_id_1614)]);"
"        /* mapSeq */"
"        /* unrolling loop of 2 */"
"        /* mapSeq */"
"        /* unrolling loop of 2 */"
"        vstore4((float4)(x1401[16 + (16 * get_local_id(0))], x1401[17 + (16 * get_local_id(0))], x1401[18 + (16 * get_local_id(0))], x1401[19 + (16 * get_local_id(0))]), 0, &output[(((8 + ((2 * n205) * get_local_id(1))) + (8 * get_local_id(0))) + (16 * wg_id_1615)) + ((512 * n205) * wg_id_1614)]);"
"        vstore4((float4)(x1401[20 + (16 * get_local_id(0))], x1401[21 + (16 * get_local_id(0))], x1401[22 + (16 * get_local_id(0))], x1401[23 + (16 * get_local_id(0))]), 0, &output[(((12 + ((2 * n205) * get_local_id(1))) + (8 * get_local_id(0))) + (16 * wg_id_1615)) + ((512 * n205) * wg_id_1614)]);"
"        /* mapSeq */"
"        /* unrolling loop of 2 */"
"        vstore4((float4)(x1401[24 + (16 * get_local_id(0))], x1401[25 + (16 * get_local_id(0))], x1401[26 + (16 * get_local_id(0))], x1401[27 + (16 * get_local_id(0))]), 0, &output[((((8 + n205) + ((2 * n205) * get_local_id(1))) + (8 * get_local_id(0))) + (16 * wg_id_1615)) + ((512 * n205) * wg_id_1614)]);"
"        vstore4((float4)(x1401[28 + (16 * get_local_id(0))], x1401[29 + (16 * get_local_id(0))], x1401[30 + (16 * get_local_id(0))], x1401[31 + (16 * get_local_id(0))]), 0, &output[((((12 + n205) + ((2 * n205) * get_local_id(1))) + (8 * get_local_id(0))) + (16 * wg_id_1615)) + ((512 * n205) * wg_id_1614)]);"
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
        const size_t global_size[3] = (const size_t[3]){128, 1024, 1};
        const size_t local_size[3] = (const size_t[3]){1, 256, 1};
        const KernelArg args[7] = (const KernelArg[7]){KARG(b0), KARG(n205), KARG(n204), KARG(n206), KARG(b4), KARG(b5), LARG((1 <= (n206 / 2) ? 1 : (n206 / 2)) * ((2 * (512 * sizeof(float))) + (2 * (16 * sizeof(float)))))};
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
        inA[i] = (float)(rand());
    }

    float* inB = hostBufferSync(ctx, inputB, M * O * sizeof(float), HOST_WRITE);
    for (int i = 0; i < M * O; i++) {
        inB[i] = (float)(rand());
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

