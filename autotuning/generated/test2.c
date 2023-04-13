const char k0_source[] =
""
"struct Record_float_int {"
"  float _fst;"
"  int _snd;"
"};"
""
"struct Record_float__float_int_ {"
"  float _fst;"
"  struct Record_float_int _snd;"
"};"
""
"int idxF(int i, int j, int k, int m, int n, int o){"
"  int count = 6;"
"  if (i == (m - 1) || i == 0){ count--; }"
"  if (j == (n - 1) || j == 0){ count--; }"
"  if (k == (o - 1) || k == 0){ count--; }"
"  return count;"
"}"
"float getCF(int neigh, float cfB, float cfI){ if (neigh < 6) { return cfB; } else { return cfI; } }"
""
"__kernel __attribute__ ((reqd_work_group_size(1, 1, 1)))"
"void k0(global float* restrict output, int n37, int n35, int n36, const global float* restrict e39, const global float* restrict e38){"
"  /* Start of moved local vars */"
"  /* End of moved local vars */"
"  /* mapGlobal */"
"  for (int gl_id_1761 = get_global_id(0); gl_id_1761 < n37; gl_id_1761 = 1024 + gl_id_1761) {"
"    /* mapGlobal */"
"    for (int gl_id_1762 = get_global_id(1); gl_id_1762 < n36; gl_id_1762 = 1 + gl_id_1762) {"
"      {"
"        struct Record_float__float_int_ x1634[27];"
"        /* unrolling loop of 2 */"
"        /* mapSeq */"
"        /* unrolling loop of 3 */"
"        /* mapSeq */"
"        /* unrolling loop of 3 */"
"        x1634[0]._fst = e38[(gl_id_1761 + (2 * gl_id_1762)) + (gl_id_1762 * n37)];"
"        x1634[0]._snd._fst = e39[(gl_id_1761 + (2 * gl_id_1762)) + (gl_id_1762 * n37)];"
"        x1634[0]._snd._snd = idxF((int)gl_id_1761, (int)gl_id_1762, (int)0, (int)(2 + n37), (int)(2 + n36), (int)(2 + n35));"
"        x1634[1]._fst = e38[((1 + gl_id_1761) + (2 * gl_id_1762)) + (gl_id_1762 * n37)];"
"        x1634[1]._snd._fst = e39[((1 + gl_id_1761) + (2 * gl_id_1762)) + (gl_id_1762 * n37)];"
"        x1634[1]._snd._snd = idxF((int)(1 + gl_id_1761), (int)gl_id_1762, (int)0, (int)(2 + n37), (int)(2 + n36), (int)(2 + n35));"
"        x1634[2]._fst = e38[((2 + gl_id_1761) + (2 * gl_id_1762)) + (gl_id_1762 * n37)];"
"        x1634[2]._snd._fst = e39[((2 + gl_id_1761) + (2 * gl_id_1762)) + (gl_id_1762 * n37)];"
"        x1634[2]._snd._snd = idxF((int)(2 + gl_id_1761), (int)gl_id_1762, (int)0, (int)(2 + n37), (int)(2 + n36), (int)(2 + n35));"
"        /* mapSeq */"
"        /* unrolling loop of 3 */"
"        x1634[3]._fst = e38[(((2 + gl_id_1761) + n37) + (2 * gl_id_1762)) + (gl_id_1762 * n37)];"
"        x1634[3]._snd._fst = e39[(((2 + gl_id_1761) + n37) + (2 * gl_id_1762)) + (gl_id_1762 * n37)];"
"        x1634[3]._snd._snd = idxF((int)gl_id_1761, (int)(1 + gl_id_1762), (int)0, (int)(2 + n37), (int)(2 + n36), (int)(2 + n35));"
"        x1634[4]._fst = e38[(((3 + gl_id_1761) + n37) + (2 * gl_id_1762)) + (gl_id_1762 * n37)];"
"        x1634[4]._snd._fst = e39[(((3 + gl_id_1761) + n37) + (2 * gl_id_1762)) + (gl_id_1762 * n37)];"
"        x1634[4]._snd._snd = idxF((int)(1 + gl_id_1761), (int)(1 + gl_id_1762), (int)0, (int)(2 + n37), (int)(2 + n36), (int)(2 + n35));"
"        x1634[5]._fst = e38[(((4 + gl_id_1761) + n37) + (2 * gl_id_1762)) + (gl_id_1762 * n37)];"
"        x1634[5]._snd._fst = e39[(((4 + gl_id_1761) + n37) + (2 * gl_id_1762)) + (gl_id_1762 * n37)];"
"        x1634[5]._snd._snd = idxF((int)(2 + gl_id_1761), (int)(1 + gl_id_1762), (int)0, (int)(2 + n37), (int)(2 + n36), (int)(2 + n35));"
"        /* mapSeq */"
"        /* unrolling loop of 3 */"
"        x1634[6]._fst = e38[(((4 + gl_id_1761) + (2 * gl_id_1762)) + (2 * n37)) + (gl_id_1762 * n37)];"
"        x1634[6]._snd._fst = e39[(((4 + gl_id_1761) + (2 * gl_id_1762)) + (2 * n37)) + (gl_id_1762 * n37)];"
"        x1634[6]._snd._snd = idxF((int)gl_id_1761, (int)(2 + gl_id_1762), (int)0, (int)(2 + n37), (int)(2 + n36), (int)(2 + n35));"
"        x1634[7]._fst = e38[(((5 + gl_id_1761) + (2 * gl_id_1762)) + (2 * n37)) + (gl_id_1762 * n37)];"
"        x1634[7]._snd._fst = e39[(((5 + gl_id_1761) + (2 * gl_id_1762)) + (2 * n37)) + (gl_id_1762 * n37)];"
"        x1634[7]._snd._snd = idxF((int)(1 + gl_id_1761), (int)(2 + gl_id_1762), (int)0, (int)(2 + n37), (int)(2 + n36), (int)(2 + n35));"
"        x1634[8]._fst = e38[(((6 + gl_id_1761) + (2 * gl_id_1762)) + (2 * n37)) + (gl_id_1762 * n37)];"
"        x1634[8]._snd._fst = e39[(((6 + gl_id_1761) + (2 * gl_id_1762)) + (2 * n37)) + (gl_id_1762 * n37)];"
"        x1634[8]._snd._snd = idxF((int)(2 + gl_id_1761), (int)(2 + gl_id_1762), (int)0, (int)(2 + n37), (int)(2 + n36), (int)(2 + n35));"
"        /* mapSeq */"
"        /* unrolling loop of 3 */"
"        /* mapSeq */"
"        /* unrolling loop of 3 */"
"        x1634[9]._fst = e38[(((((4 + gl_id_1761) + (2 * gl_id_1762)) + (2 * n36)) + (2 * n37)) + (gl_id_1762 * n37)) + (n36 * n37)];"
"        x1634[9]._snd._fst = e39[(((((4 + gl_id_1761) + (2 * gl_id_1762)) + (2 * n36)) + (2 * n37)) + (gl_id_1762 * n37)) + (n36 * n37)];"
"        x1634[9]._snd._snd = idxF((int)gl_id_1761, (int)gl_id_1762, (int)1, (int)(2 + n37), (int)(2 + n36), (int)(2 + n35));"
"        x1634[10]._fst = e38[(((((5 + gl_id_1761) + (2 * gl_id_1762)) + (2 * n36)) + (2 * n37)) + (gl_id_1762 * n37)) + (n36 * n37)];"
"        x1634[10]._snd._fst = e39[(((((5 + gl_id_1761) + (2 * gl_id_1762)) + (2 * n36)) + (2 * n37)) + (gl_id_1762 * n37)) + (n36 * n37)];"
"        x1634[10]._snd._snd = idxF((int)(1 + gl_id_1761), (int)gl_id_1762, (int)1, (int)(2 + n37), (int)(2 + n36), (int)(2 + n35));"
"        x1634[11]._fst = e38[(((((6 + gl_id_1761) + (2 * gl_id_1762)) + (2 * n36)) + (2 * n37)) + (gl_id_1762 * n37)) + (n36 * n37)];"
"        x1634[11]._snd._fst = e39[(((((6 + gl_id_1761) + (2 * gl_id_1762)) + (2 * n36)) + (2 * n37)) + (gl_id_1762 * n37)) + (n36 * n37)];"
"        x1634[11]._snd._snd = idxF((int)(2 + gl_id_1761), (int)gl_id_1762, (int)1, (int)(2 + n37), (int)(2 + n36), (int)(2 + n35));"
"        /* mapSeq */"
"        /* unrolling loop of 3 */"
"        x1634[12]._fst = e38[(((((6 + gl_id_1761) + (2 * gl_id_1762)) + (2 * n36)) + (3 * n37)) + (gl_id_1762 * n37)) + (n36 * n37)];"
"        x1634[12]._snd._fst = e39[(((((6 + gl_id_1761) + (2 * gl_id_1762)) + (2 * n36)) + (3 * n37)) + (gl_id_1762 * n37)) + (n36 * n37)];"
"        x1634[12]._snd._snd = idxF((int)gl_id_1761, (int)(1 + gl_id_1762), (int)1, (int)(2 + n37), (int)(2 + n36), (int)(2 + n35));"
"        x1634[13]._fst = e38[(((((7 + gl_id_1761) + (2 * gl_id_1762)) + (2 * n36)) + (3 * n37)) + (gl_id_1762 * n37)) + (n36 * n37)];"
"        x1634[13]._snd._fst = e39[(((((7 + gl_id_1761) + (2 * gl_id_1762)) + (2 * n36)) + (3 * n37)) + (gl_id_1762 * n37)) + (n36 * n37)];"
"        x1634[13]._snd._snd = idxF((int)(1 + gl_id_1761), (int)(1 + gl_id_1762), (int)1, (int)(2 + n37), (int)(2 + n36), (int)(2 + n35));"
"        x1634[14]._fst = e38[(((((8 + gl_id_1761) + (2 * gl_id_1762)) + (2 * n36)) + (3 * n37)) + (gl_id_1762 * n37)) + (n36 * n37)];"
"        x1634[14]._snd._fst = e39[(((((8 + gl_id_1761) + (2 * gl_id_1762)) + (2 * n36)) + (3 * n37)) + (gl_id_1762 * n37)) + (n36 * n37)];"
"        x1634[14]._snd._snd = idxF((int)(2 + gl_id_1761), (int)(1 + gl_id_1762), (int)1, (int)(2 + n37), (int)(2 + n36), (int)(2 + n35));"
"        /* mapSeq */"
"        /* unrolling loop of 3 */"
"        x1634[15]._fst = e38[(((((8 + gl_id_1761) + (2 * gl_id_1762)) + (2 * n36)) + (4 * n37)) + (gl_id_1762 * n37)) + (n36 * n37)];"
"        x1634[15]._snd._fst = e39[(((((8 + gl_id_1761) + (2 * gl_id_1762)) + (2 * n36)) + (4 * n37)) + (gl_id_1762 * n37)) + (n36 * n37)];"
"        x1634[15]._snd._snd = idxF((int)gl_id_1761, (int)(2 + gl_id_1762), (int)1, (int)(2 + n37), (int)(2 + n36), (int)(2 + n35));"
"        x1634[16]._fst = e38[(((((9 + gl_id_1761) + (2 * gl_id_1762)) + (2 * n36)) + (4 * n37)) + (gl_id_1762 * n37)) + (n36 * n37)];"
"        x1634[16]._snd._fst = e39[(((((9 + gl_id_1761) + (2 * gl_id_1762)) + (2 * n36)) + (4 * n37)) + (gl_id_1762 * n37)) + (n36 * n37)];"
"        x1634[16]._snd._snd = idxF((int)(1 + gl_id_1761), (int)(2 + gl_id_1762), (int)1, (int)(2 + n37), (int)(2 + n36), (int)(2 + n35));"
"        x1634[17]._fst = e38[(((((10 + gl_id_1761) + (2 * gl_id_1762)) + (2 * n36)) + (4 * n37)) + (gl_id_1762 * n37)) + (n36 * n37)];"
"        x1634[17]._snd._fst = e39[(((((10 + gl_id_1761) + (2 * gl_id_1762)) + (2 * n36)) + (4 * n37)) + (gl_id_1762 * n37)) + (n36 * n37)];"
"        x1634[17]._snd._snd = idxF((int)(2 + gl_id_1761), (int)(2 + gl_id_1762), (int)1, (int)(2 + n37), (int)(2 + n36), (int)(2 + n35));"
"        /* iterateStream */"
"        for (int i_1763 = 0; i_1763 < n35; i_1763 = 1 + i_1763) {"
"          /* mapSeq */"
"          /* unrolling loop of 3 */"
"          /* mapSeq */"
"          /* unrolling loop of 3 */"
"          x1634[18]._fst = e38[(((((((((8 + gl_id_1761) + (2 * gl_id_1762)) + ((2 * i_1763) * n36)) + ((2 * i_1763) * n37)) + ((2 * n36) * n37)) + (4 * i_1763)) + (4 * n36)) + (4 * n37)) + (gl_id_1762 * n37)) + ((i_1763 * n36) * n37)];"
"          x1634[18]._snd._fst = e39[(((((((((8 + gl_id_1761) + (2 * gl_id_1762)) + ((2 * i_1763) * n36)) + ((2 * i_1763) * n37)) + ((2 * n36) * n37)) + (4 * i_1763)) + (4 * n36)) + (4 * n37)) + (gl_id_1762 * n37)) + ((i_1763 * n36) * n37)];"
"          x1634[18]._snd._snd = idxF((int)gl_id_1761, (int)gl_id_1762, (int)(2 + i_1763), (int)(2 + n37), (int)(2 + n36), (int)(2 + n35));"
"          x1634[19]._fst = e38[(((((((((9 + gl_id_1761) + (2 * gl_id_1762)) + ((2 * i_1763) * n36)) + ((2 * i_1763) * n37)) + ((2 * n36) * n37)) + (4 * i_1763)) + (4 * n36)) + (4 * n37)) + (gl_id_1762 * n37)) + ((i_1763 * n36) * n37)];"
"          x1634[19]._snd._fst = e39[(((((((((9 + gl_id_1761) + (2 * gl_id_1762)) + ((2 * i_1763) * n36)) + ((2 * i_1763) * n37)) + ((2 * n36) * n37)) + (4 * i_1763)) + (4 * n36)) + (4 * n37)) + (gl_id_1762 * n37)) + ((i_1763 * n36) * n37)];"
"          x1634[19]._snd._snd = idxF((int)(1 + gl_id_1761), (int)gl_id_1762, (int)(2 + i_1763), (int)(2 + n37), (int)(2 + n36), (int)(2 + n35));"
"          x1634[20]._fst = e38[(((((((((10 + gl_id_1761) + (2 * gl_id_1762)) + ((2 * i_1763) * n36)) + ((2 * i_1763) * n37)) + ((2 * n36) * n37)) + (4 * i_1763)) + (4 * n36)) + (4 * n37)) + (gl_id_1762 * n37)) + ((i_1763 * n36) * n37)];"
"          x1634[20]._snd._fst = e39[(((((((((10 + gl_id_1761) + (2 * gl_id_1762)) + ((2 * i_1763) * n36)) + ((2 * i_1763) * n37)) + ((2 * n36) * n37)) + (4 * i_1763)) + (4 * n36)) + (4 * n37)) + (gl_id_1762 * n37)) + ((i_1763 * n36) * n37)];"
"          x1634[20]._snd._snd = idxF((int)(2 + gl_id_1761), (int)gl_id_1762, (int)(2 + i_1763), (int)(2 + n37), (int)(2 + n36), (int)(2 + n35));"
"          /* mapSeq */"
"          /* unrolling loop of 3 */"
"          x1634[21]._fst = e38[(((((((((10 + gl_id_1761) + (2 * gl_id_1762)) + ((2 * i_1763) * n36)) + ((2 * i_1763) * n37)) + ((2 * n36) * n37)) + (4 * i_1763)) + (4 * n36)) + (5 * n37)) + (gl_id_1762 * n37)) + ((i_1763 * n36) * n37)];"
"          x1634[21]._snd._fst = e39[(((((((((10 + gl_id_1761) + (2 * gl_id_1762)) + ((2 * i_1763) * n36)) + ((2 * i_1763) * n37)) + ((2 * n36) * n37)) + (4 * i_1763)) + (4 * n36)) + (5 * n37)) + (gl_id_1762 * n37)) + ((i_1763 * n36) * n37)];"
"          x1634[21]._snd._snd = idxF((int)gl_id_1761, (int)(1 + gl_id_1762), (int)(2 + i_1763), (int)(2 + n37), (int)(2 + n36), (int)(2 + n35));"
"          x1634[22]._fst = e38[(((((((((11 + gl_id_1761) + (2 * gl_id_1762)) + ((2 * i_1763) * n36)) + ((2 * i_1763) * n37)) + ((2 * n36) * n37)) + (4 * i_1763)) + (4 * n36)) + (5 * n37)) + (gl_id_1762 * n37)) + ((i_1763 * n36) * n37)];"
"          x1634[22]._snd._fst = e39[(((((((((11 + gl_id_1761) + (2 * gl_id_1762)) + ((2 * i_1763) * n36)) + ((2 * i_1763) * n37)) + ((2 * n36) * n37)) + (4 * i_1763)) + (4 * n36)) + (5 * n37)) + (gl_id_1762 * n37)) + ((i_1763 * n36) * n37)];"
"          x1634[22]._snd._snd = idxF((int)(1 + gl_id_1761), (int)(1 + gl_id_1762), (int)(2 + i_1763), (int)(2 + n37), (int)(2 + n36), (int)(2 + n35));"
"          x1634[23]._fst = e38[(((((((((12 + gl_id_1761) + (2 * gl_id_1762)) + ((2 * i_1763) * n36)) + ((2 * i_1763) * n37)) + ((2 * n36) * n37)) + (4 * i_1763)) + (4 * n36)) + (5 * n37)) + (gl_id_1762 * n37)) + ((i_1763 * n36) * n37)];"
"          x1634[23]._snd._fst = e39[(((((((((12 + gl_id_1761) + (2 * gl_id_1762)) + ((2 * i_1763) * n36)) + ((2 * i_1763) * n37)) + ((2 * n36) * n37)) + (4 * i_1763)) + (4 * n36)) + (5 * n37)) + (gl_id_1762 * n37)) + ((i_1763 * n36) * n37)];"
"          x1634[23]._snd._snd = idxF((int)(2 + gl_id_1761), (int)(1 + gl_id_1762), (int)(2 + i_1763), (int)(2 + n37), (int)(2 + n36), (int)(2 + n35));"
"          /* mapSeq */"
"          /* unrolling loop of 3 */"
"          x1634[24]._fst = e38[(((((((((12 + gl_id_1761) + (2 * gl_id_1762)) + ((2 * i_1763) * n36)) + ((2 * i_1763) * n37)) + ((2 * n36) * n37)) + (4 * i_1763)) + (4 * n36)) + (6 * n37)) + (gl_id_1762 * n37)) + ((i_1763 * n36) * n37)];"
"          x1634[24]._snd._fst = e39[(((((((((12 + gl_id_1761) + (2 * gl_id_1762)) + ((2 * i_1763) * n36)) + ((2 * i_1763) * n37)) + ((2 * n36) * n37)) + (4 * i_1763)) + (4 * n36)) + (6 * n37)) + (gl_id_1762 * n37)) + ((i_1763 * n36) * n37)];"
"          x1634[24]._snd._snd = idxF((int)gl_id_1761, (int)(2 + gl_id_1762), (int)(2 + i_1763), (int)(2 + n37), (int)(2 + n36), (int)(2 + n35));"
"          x1634[25]._fst = e38[(((((((((13 + gl_id_1761) + (2 * gl_id_1762)) + ((2 * i_1763) * n36)) + ((2 * i_1763) * n37)) + ((2 * n36) * n37)) + (4 * i_1763)) + (4 * n36)) + (6 * n37)) + (gl_id_1762 * n37)) + ((i_1763 * n36) * n37)];"
"          x1634[25]._snd._fst = e39[(((((((((13 + gl_id_1761) + (2 * gl_id_1762)) + ((2 * i_1763) * n36)) + ((2 * i_1763) * n37)) + ((2 * n36) * n37)) + (4 * i_1763)) + (4 * n36)) + (6 * n37)) + (gl_id_1762 * n37)) + ((i_1763 * n36) * n37)];"
"          x1634[25]._snd._snd = idxF((int)(1 + gl_id_1761), (int)(2 + gl_id_1762), (int)(2 + i_1763), (int)(2 + n37), (int)(2 + n36), (int)(2 + n35));"
"          x1634[26]._fst = e38[(((((((((14 + gl_id_1761) + (2 * gl_id_1762)) + ((2 * i_1763) * n36)) + ((2 * i_1763) * n37)) + ((2 * n36) * n37)) + (4 * i_1763)) + (4 * n36)) + (6 * n37)) + (gl_id_1762 * n37)) + ((i_1763 * n36) * n37)];"
"          x1634[26]._snd._fst = e39[(((((((((14 + gl_id_1761) + (2 * gl_id_1762)) + ((2 * i_1763) * n36)) + ((2 * i_1763) * n37)) + ((2 * n36) * n37)) + (4 * i_1763)) + (4 * n36)) + (6 * n37)) + (gl_id_1762 * n37)) + ((i_1763 * n36) * n37)];"
"          x1634[26]._snd._snd = idxF((int)(2 + gl_id_1761), (int)(2 + gl_id_1762), (int)(2 + i_1763), (int)(2 + n37), (int)(2 + n36), (int)(2 + n35));"
"          {"
"            float x1521;"
"            x1521 = ((((x1634[4]._snd._fst + x1634[10]._snd._fst) + x1634[12]._snd._fst) + x1634[14]._snd._fst) + x1634[16]._snd._fst) + x1634[22]._snd._fst;"
"            {"
"              float x1493;"
"              x1493 = getCF(x1634[13]._snd._snd, 0.9971132f, 1.0f);"
"              {"
"                float x1471;"
"                x1471 = getCF(x1634[13]._snd._snd, 0.9971216f, 1.0f);"
"                output[(gl_id_1761 + (gl_id_1762 * n37)) + ((i_1763 * n36) * n37)] = ((x1634[13]._snd._fst * (2.0f - (((float)x1634[13]._snd._snd) * 0.3333333f))) + ((x1521 * 0.3333333f) - (x1634[13]._fst * x1493))) * x1471;"
"              }"
"              "
"            }"
"            "
"          }"
"          "
"          /* mapSeq */"
"          /* unrolling loop of 2 */"
"          /* mapSeq */"
"          /* unrolling loop of 3 */"
"          /* mapSeq */"
"          /* unrolling loop of 3 */"
"          x1634[0]._fst = x1634[9]._fst;"
"          x1634[0]._snd._fst = x1634[9]._snd._fst;"
"          x1634[0]._snd._snd = x1634[9]._snd._snd;"
"          x1634[1]._fst = x1634[10]._fst;"
"          x1634[1]._snd._fst = x1634[10]._snd._fst;"
"          x1634[1]._snd._snd = x1634[10]._snd._snd;"
"          x1634[2]._fst = x1634[11]._fst;"
"          x1634[2]._snd._fst = x1634[11]._snd._fst;"
"          x1634[2]._snd._snd = x1634[11]._snd._snd;"
"          /* mapSeq */"
"          /* unrolling loop of 3 */"
"          x1634[3]._fst = x1634[12]._fst;"
"          x1634[3]._snd._fst = x1634[12]._snd._fst;"
"          x1634[3]._snd._snd = x1634[12]._snd._snd;"
"          x1634[4]._fst = x1634[13]._fst;"
"          x1634[4]._snd._fst = x1634[13]._snd._fst;"
"          x1634[4]._snd._snd = x1634[13]._snd._snd;"
"          x1634[5]._fst = x1634[14]._fst;"
"          x1634[5]._snd._fst = x1634[14]._snd._fst;"
"          x1634[5]._snd._snd = x1634[14]._snd._snd;"
"          /* mapSeq */"
"          /* unrolling loop of 3 */"
"          x1634[6]._fst = x1634[15]._fst;"
"          x1634[6]._snd._fst = x1634[15]._snd._fst;"
"          x1634[6]._snd._snd = x1634[15]._snd._snd;"
"          x1634[7]._fst = x1634[16]._fst;"
"          x1634[7]._snd._fst = x1634[16]._snd._fst;"
"          x1634[7]._snd._snd = x1634[16]._snd._snd;"
"          x1634[8]._fst = x1634[17]._fst;"
"          x1634[8]._snd._fst = x1634[17]._snd._fst;"
"          x1634[8]._snd._snd = x1634[17]._snd._snd;"
"          /* mapSeq */"
"          /* unrolling loop of 3 */"
"          /* mapSeq */"
"          /* unrolling loop of 3 */"
"          x1634[9]._fst = x1634[18]._fst;"
"          x1634[9]._snd._fst = x1634[18]._snd._fst;"
"          x1634[9]._snd._snd = x1634[18]._snd._snd;"
"          x1634[10]._fst = x1634[19]._fst;"
"          x1634[10]._snd._fst = x1634[19]._snd._fst;"
"          x1634[10]._snd._snd = x1634[19]._snd._snd;"
"          x1634[11]._fst = x1634[20]._fst;"
"          x1634[11]._snd._fst = x1634[20]._snd._fst;"
"          x1634[11]._snd._snd = x1634[20]._snd._snd;"
"          /* mapSeq */"
"          /* unrolling loop of 3 */"
"          x1634[12]._fst = x1634[21]._fst;"
"          x1634[12]._snd._fst = x1634[21]._snd._fst;"
"          x1634[12]._snd._snd = x1634[21]._snd._snd;"
"          x1634[13]._fst = x1634[22]._fst;"
"          x1634[13]._snd._fst = x1634[22]._snd._fst;"
"          x1634[13]._snd._snd = x1634[22]._snd._snd;"
"          x1634[14]._fst = x1634[23]._fst;"
"          x1634[14]._snd._fst = x1634[23]._snd._fst;"
"          x1634[14]._snd._snd = x1634[23]._snd._snd;"
"          /* mapSeq */"
"          /* unrolling loop of 3 */"
"          x1634[15]._fst = x1634[24]._fst;"
"          x1634[15]._snd._fst = x1634[24]._snd._fst;"
"          x1634[15]._snd._snd = x1634[24]._snd._snd;"
"          x1634[16]._fst = x1634[25]._fst;"
"          x1634[16]._snd._fst = x1634[25]._snd._fst;"
"          x1634[16]._snd._snd = x1634[25]._snd._snd;"
"          x1634[17]._fst = x1634[26]._fst;"
"          x1634[17]._snd._fst = x1634[26]._snd._fst;"
"          x1634[17]._snd._snd = x1634[26]._snd._snd;"
"        }"
"        "
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

void fun_run(Context ctx, fun_t* self, Buffer moutput, int n35, int n36, int n37, Buffer me38, Buffer me39){
  {
    DeviceBuffer b0 = deviceBufferSync(ctx, moutput, n35 * (n36 * (n37 * sizeof(float))), DEVICE_WRITE);
    DeviceBuffer b4 = deviceBufferSync(ctx, me39, (2 + n35) * ((2 + n36) * ((2 + n37) * sizeof(float))), DEVICE_READ);
    DeviceBuffer b5 = deviceBufferSync(ctx, me38, (2 + n35) * ((2 + n36) * ((2 + n37) * sizeof(float))), DEVICE_READ);
    const size_t global_size[3] = (const size_t[3]){1024, 1, 1};
    const size_t local_size[3] = (const size_t[3]){1, 1, 1};
    const KernelArg args[6] = (const KernelArg[6]){KARG(b0), KARG(n37), KARG(n35), KARG(n36), KARG(b4), KARG(b5)};
    launchKernel(ctx, (*self).k0, global_size, local_size, 6, args);
  }

}

void fun_init_run(Context ctx, Buffer moutput, int n35, int n36, int n37, Buffer me38, Buffer me39){
  fun_t fun;
  fun_init(ctx, &fun);
  fun_run(ctx, &fun, moutput, n35, n36, n37, me38, me39);
  fun_destroy(ctx, &fun);
}




int main(int argc, char** argv) {
  Context ctx = createDefaultContext();
  fun_t fun;
  fun_init(ctx, &fun);


const int O = 1024;
const int N = 1024;
const int M = 1024;

srand(time(NULL));

Buffer mat1 = createBuffer(ctx, (O+2) * (N+2) * (M+2) * sizeof(float), HOST_READ | HOST_WRITE | DEVICE_READ);
Buffer mat2 = createBuffer(ctx, (O+2) * (N+2) * (M+2) * sizeof(float), HOST_READ | HOST_WRITE | DEVICE_READ);
Buffer output = createBuffer(ctx, O * N * M * sizeof(float), HOST_READ | HOST_WRITE | DEVICE_WRITE);

float* m1 = hostBufferSync(ctx, mat1, (O+2) * (N+2) * (M+2) * sizeof(float), HOST_WRITE);
for (int i = 0; i < (O+2) * (N+2) * (M+2); i++) {
  m1[i] = (float)(i%100);
}
float* m2 = hostBufferSync(ctx, mat2, (O+2) * (N+2) * (M+2) * sizeof(float), HOST_WRITE);
for (int i = 0; i < (O+2) * (N+2) * (M+2); i++) {
  m2[i] = (float)(i%100);
}

// synchronize before entering timed section
deviceBufferSync(ctx, mat1, (O+2) * (N+2) * (M+2) * sizeof(float), DEVICE_READ);
deviceBufferSync(ctx, mat2, (O+2) * (N+2) * (M+2) * sizeof(float), DEVICE_READ);
waitFinished(ctx);


  int iterations = atoi(argv[1]);
  for (int sample = 0; sample < iterations; sample++) {

fun_run(ctx, &fun, output, O, N, M, mat1, mat2);
waitFinished(ctx);

  }

// TODO: could check output here
// use given gold expression?

destroyBuffer(ctx, mat1);
destroyBuffer(ctx, mat2);
destroyBuffer(ctx, output);

  fun_destroy(ctx, &fun);
  destroyContext(ctx);
  return EXIT_SUCCESS;
}