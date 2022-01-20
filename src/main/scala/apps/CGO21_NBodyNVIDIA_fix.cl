
struct Record_float4_float4 {
  float4 _fst;
  float4 _snd;
};

float4 calcAcc(float4 p1, float4 p2, float deltaT, float espSqr, float4 acc){
  float4 r;
  r.xyz = p2.xyz - p1.xyz;
  float distSqr = r.x*r.x + r.y*r.y + r.z*r.z;
  float invDist = 1.0f / sqrt(distSqr + espSqr);
  float invDistCube = invDist * invDist * invDist;
  float s = invDistCube * p2.w;
  float4 res;
  res.xyz = acc.xyz + s * r.xyz;
  return res;
}

struct Record_float4_float4 update(float4 pos, float4 vel, float deltaT, float4 acceleration){
  float4 newPos;
  newPos.xyz = pos.xyz + vel.xyz * deltaT + 0.5f * acceleration.xyz * deltaT * deltaT;
  newPos.w = pos.w;
  float4 newVel;
  newVel.xyz = vel.xyz + acceleration.xyz * deltaT;
  newVel.w = vel.w;
  return (struct Record_float4_float4){ newPos, newVel };
}

__kernel __attribute__ ((reqd_work_group_size(256, 1, 1)))
void KERNEL(global struct Record_float4_float4* restrict output, int n154, const global float4* restrict e155, const global float4* restrict e156, float e157, float e158, local float4* restrict x845){
  /* Start of moved local vars */
  /* End of moved local vars */
  /* mapWorkGroup */
  /* iteration count is exactly 1, no loop emitted */
  int wg_id_893 = get_group_id(1);
  /* mapWorkGroup */
  /* iteration count is exactly 1, no loop emitted */
  int wg_id_894 = get_group_id(0);
  /* oclReduceSeq */
  {
    float4 x859[1];
    /* mapLocal */
    /* unrolling loop of 1 */
    /* mapLocal */
    /* unrolling loop of 1 */
    x859[0] = (float4)(0.0f);
    for (int i_895 = 0; i_895 < (n154 / 256); i_895 = 1 + i_895) {
      /* mapLocal */
      /* iteration count is exactly 1, no loop emitted */
      int l_id_896 = get_local_id(1);
      /* mapLocal */
      /* iteration count is exactly 1, no loop emitted */
      int l_id_897 = get_local_id(0);
      x845[l_id_897 + (256 * l_id_896)] = e155[(l_id_897 + (256 * i_895)) + (256 * l_id_896)];
      barrier(CLK_LOCAL_MEM_FENCE);
      /* mapLocal */
      /* unrolling loop of 1 */
      {
        float4 x830[1];
        /* mapLocal */
        /* unrolling loop of 1 */
        x830[0] = e155[((256 * wg_id_894) + (n154 * wg_id_893)) + get_local_id(0)];
        /* mapLocal */
        /* unrolling loop of 1 */
        /* oclReduceSeq */
        {
          float4 x816;
          x816 = x859[0];
          /* unrolling loop of 256 */
          x816 = calcAcc(x830[0], x845[256 * get_local_id(1)], e158, e157, x816);
          x816 = calcAcc(x830[0], x845[1 + (256 * get_local_id(1))], e158, e157, x816);
          x816 = calcAcc(x830[0], x845[2 + (256 * get_local_id(1))], e158, e157, x816);
          x816 = calcAcc(x830[0], x845[3 + (256 * get_local_id(1))], e158, e157, x816);
          x816 = calcAcc(x830[0], x845[4 + (256 * get_local_id(1))], e158, e157, x816);
          x816 = calcAcc(x830[0], x845[5 + (256 * get_local_id(1))], e158, e157, x816);
          x816 = calcAcc(x830[0], x845[6 + (256 * get_local_id(1))], e158, e157, x816);
          x816 = calcAcc(x830[0], x845[7 + (256 * get_local_id(1))], e158, e157, x816);
          x816 = calcAcc(x830[0], x845[8 + (256 * get_local_id(1))], e158, e157, x816);
          x816 = calcAcc(x830[0], x845[9 + (256 * get_local_id(1))], e158, e157, x816);
          x816 = calcAcc(x830[0], x845[10 + (256 * get_local_id(1))], e158, e157, x816);
          x816 = calcAcc(x830[0], x845[11 + (256 * get_local_id(1))], e158, e157, x816);
          x816 = calcAcc(x830[0], x845[12 + (256 * get_local_id(1))], e158, e157, x816);
          x816 = calcAcc(x830[0], x845[13 + (256 * get_local_id(1))], e158, e157, x816);
          x816 = calcAcc(x830[0], x845[14 + (256 * get_local_id(1))], e158, e157, x816);
          x816 = calcAcc(x830[0], x845[15 + (256 * get_local_id(1))], e158, e157, x816);
          x816 = calcAcc(x830[0], x845[16 + (256 * get_local_id(1))], e158, e157, x816);
          x816 = calcAcc(x830[0], x845[17 + (256 * get_local_id(1))], e158, e157, x816);
          x816 = calcAcc(x830[0], x845[18 + (256 * get_local_id(1))], e158, e157, x816);
          x816 = calcAcc(x830[0], x845[19 + (256 * get_local_id(1))], e158, e157, x816);
          x816 = calcAcc(x830[0], x845[20 + (256 * get_local_id(1))], e158, e157, x816);
          x816 = calcAcc(x830[0], x845[21 + (256 * get_local_id(1))], e158, e157, x816);
          x816 = calcAcc(x830[0], x845[22 + (256 * get_local_id(1))], e158, e157, x816);
          x816 = calcAcc(x830[0], x845[23 + (256 * get_local_id(1))], e158, e157, x816);
          x816 = calcAcc(x830[0], x845[24 + (256 * get_local_id(1))], e158, e157, x816);
          x816 = calcAcc(x830[0], x845[25 + (256 * get_local_id(1))], e158, e157, x816);
          x816 = calcAcc(x830[0], x845[26 + (256 * get_local_id(1))], e158, e157, x816);
          x816 = calcAcc(x830[0], x845[27 + (256 * get_local_id(1))], e158, e157, x816);
          x816 = calcAcc(x830[0], x845[28 + (256 * get_local_id(1))], e158, e157, x816);
          x816 = calcAcc(x830[0], x845[29 + (256 * get_local_id(1))], e158, e157, x816);
          x816 = calcAcc(x830[0], x845[30 + (256 * get_local_id(1))], e158, e157, x816);
          x816 = calcAcc(x830[0], x845[31 + (256 * get_local_id(1))], e158, e157, x816);
          x816 = calcAcc(x830[0], x845[32 + (256 * get_local_id(1))], e158, e157, x816);
          x816 = calcAcc(x830[0], x845[33 + (256 * get_local_id(1))], e158, e157, x816);
          x816 = calcAcc(x830[0], x845[34 + (256 * get_local_id(1))], e158, e157, x816);
          x816 = calcAcc(x830[0], x845[35 + (256 * get_local_id(1))], e158, e157, x816);
          x816 = calcAcc(x830[0], x845[36 + (256 * get_local_id(1))], e158, e157, x816);
          x816 = calcAcc(x830[0], x845[37 + (256 * get_local_id(1))], e158, e157, x816);
          x816 = calcAcc(x830[0], x845[38 + (256 * get_local_id(1))], e158, e157, x816);
          x816 = calcAcc(x830[0], x845[39 + (256 * get_local_id(1))], e158, e157, x816);
          x816 = calcAcc(x830[0], x845[40 + (256 * get_local_id(1))], e158, e157, x816);
          x816 = calcAcc(x830[0], x845[41 + (256 * get_local_id(1))], e158, e157, x816);
          x816 = calcAcc(x830[0], x845[42 + (256 * get_local_id(1))], e158, e157, x816);
          x816 = calcAcc(x830[0], x845[43 + (256 * get_local_id(1))], e158, e157, x816);
          x816 = calcAcc(x830[0], x845[44 + (256 * get_local_id(1))], e158, e157, x816);
          x816 = calcAcc(x830[0], x845[45 + (256 * get_local_id(1))], e158, e157, x816);
          x816 = calcAcc(x830[0], x845[46 + (256 * get_local_id(1))], e158, e157, x816);
          x816 = calcAcc(x830[0], x845[47 + (256 * get_local_id(1))], e158, e157, x816);
          x816 = calcAcc(x830[0], x845[48 + (256 * get_local_id(1))], e158, e157, x816);
          x816 = calcAcc(x830[0], x845[49 + (256 * get_local_id(1))], e158, e157, x816);
          x816 = calcAcc(x830[0], x845[50 + (256 * get_local_id(1))], e158, e157, x816);
          x816 = calcAcc(x830[0], x845[51 + (256 * get_local_id(1))], e158, e157, x816);
          x816 = calcAcc(x830[0], x845[52 + (256 * get_local_id(1))], e158, e157, x816);
          x816 = calcAcc(x830[0], x845[53 + (256 * get_local_id(1))], e158, e157, x816);
          x816 = calcAcc(x830[0], x845[54 + (256 * get_local_id(1))], e158, e157, x816);
          x816 = calcAcc(x830[0], x845[55 + (256 * get_local_id(1))], e158, e157, x816);
          x816 = calcAcc(x830[0], x845[56 + (256 * get_local_id(1))], e158, e157, x816);
          x816 = calcAcc(x830[0], x845[57 + (256 * get_local_id(1))], e158, e157, x816);
          x816 = calcAcc(x830[0], x845[58 + (256 * get_local_id(1))], e158, e157, x816);
          x816 = calcAcc(x830[0], x845[59 + (256 * get_local_id(1))], e158, e157, x816);
          x816 = calcAcc(x830[0], x845[60 + (256 * get_local_id(1))], e158, e157, x816);
          x816 = calcAcc(x830[0], x845[61 + (256 * get_local_id(1))], e158, e157, x816);
          x816 = calcAcc(x830[0], x845[62 + (256 * get_local_id(1))], e158, e157, x816);
          x816 = calcAcc(x830[0], x845[63 + (256 * get_local_id(1))], e158, e157, x816);
          x816 = calcAcc(x830[0], x845[64 + (256 * get_local_id(1))], e158, e157, x816);
          x816 = calcAcc(x830[0], x845[65 + (256 * get_local_id(1))], e158, e157, x816);
          x816 = calcAcc(x830[0], x845[66 + (256 * get_local_id(1))], e158, e157, x816);
          x816 = calcAcc(x830[0], x845[67 + (256 * get_local_id(1))], e158, e157, x816);
          x816 = calcAcc(x830[0], x845[68 + (256 * get_local_id(1))], e158, e157, x816);
          x816 = calcAcc(x830[0], x845[69 + (256 * get_local_id(1))], e158, e157, x816);
          x816 = calcAcc(x830[0], x845[70 + (256 * get_local_id(1))], e158, e157, x816);
          x816 = calcAcc(x830[0], x845[71 + (256 * get_local_id(1))], e158, e157, x816);
          x816 = calcAcc(x830[0], x845[72 + (256 * get_local_id(1))], e158, e157, x816);
          x816 = calcAcc(x830[0], x845[73 + (256 * get_local_id(1))], e158, e157, x816);
          x816 = calcAcc(x830[0], x845[74 + (256 * get_local_id(1))], e158, e157, x816);
          x816 = calcAcc(x830[0], x845[75 + (256 * get_local_id(1))], e158, e157, x816);
          x816 = calcAcc(x830[0], x845[76 + (256 * get_local_id(1))], e158, e157, x816);
          x816 = calcAcc(x830[0], x845[77 + (256 * get_local_id(1))], e158, e157, x816);
          x816 = calcAcc(x830[0], x845[78 + (256 * get_local_id(1))], e158, e157, x816);
          x816 = calcAcc(x830[0], x845[79 + (256 * get_local_id(1))], e158, e157, x816);
          x816 = calcAcc(x830[0], x845[80 + (256 * get_local_id(1))], e158, e157, x816);
          x816 = calcAcc(x830[0], x845[81 + (256 * get_local_id(1))], e158, e157, x816);
          x816 = calcAcc(x830[0], x845[82 + (256 * get_local_id(1))], e158, e157, x816);
          x816 = calcAcc(x830[0], x845[83 + (256 * get_local_id(1))], e158, e157, x816);
          x816 = calcAcc(x830[0], x845[84 + (256 * get_local_id(1))], e158, e157, x816);
          x816 = calcAcc(x830[0], x845[85 + (256 * get_local_id(1))], e158, e157, x816);
          x816 = calcAcc(x830[0], x845[86 + (256 * get_local_id(1))], e158, e157, x816);
          x816 = calcAcc(x830[0], x845[87 + (256 * get_local_id(1))], e158, e157, x816);
          x816 = calcAcc(x830[0], x845[88 + (256 * get_local_id(1))], e158, e157, x816);
          x816 = calcAcc(x830[0], x845[89 + (256 * get_local_id(1))], e158, e157, x816);
          x816 = calcAcc(x830[0], x845[90 + (256 * get_local_id(1))], e158, e157, x816);
          x816 = calcAcc(x830[0], x845[91 + (256 * get_local_id(1))], e158, e157, x816);
          x816 = calcAcc(x830[0], x845[92 + (256 * get_local_id(1))], e158, e157, x816);
          x816 = calcAcc(x830[0], x845[93 + (256 * get_local_id(1))], e158, e157, x816);
          x816 = calcAcc(x830[0], x845[94 + (256 * get_local_id(1))], e158, e157, x816);
          x816 = calcAcc(x830[0], x845[95 + (256 * get_local_id(1))], e158, e157, x816);
          x816 = calcAcc(x830[0], x845[96 + (256 * get_local_id(1))], e158, e157, x816);
          x816 = calcAcc(x830[0], x845[97 + (256 * get_local_id(1))], e158, e157, x816);
          x816 = calcAcc(x830[0], x845[98 + (256 * get_local_id(1))], e158, e157, x816);
          x816 = calcAcc(x830[0], x845[99 + (256 * get_local_id(1))], e158, e157, x816);
          x816 = calcAcc(x830[0], x845[100 + (256 * get_local_id(1))], e158, e157, x816);
          x816 = calcAcc(x830[0], x845[101 + (256 * get_local_id(1))], e158, e157, x816);
          x816 = calcAcc(x830[0], x845[102 + (256 * get_local_id(1))], e158, e157, x816);
          x816 = calcAcc(x830[0], x845[103 + (256 * get_local_id(1))], e158, e157, x816);
          x816 = calcAcc(x830[0], x845[104 + (256 * get_local_id(1))], e158, e157, x816);
          x816 = calcAcc(x830[0], x845[105 + (256 * get_local_id(1))], e158, e157, x816);
          x816 = calcAcc(x830[0], x845[106 + (256 * get_local_id(1))], e158, e157, x816);
          x816 = calcAcc(x830[0], x845[107 + (256 * get_local_id(1))], e158, e157, x816);
          x816 = calcAcc(x830[0], x845[108 + (256 * get_local_id(1))], e158, e157, x816);
          x816 = calcAcc(x830[0], x845[109 + (256 * get_local_id(1))], e158, e157, x816);
          x816 = calcAcc(x830[0], x845[110 + (256 * get_local_id(1))], e158, e157, x816);
          x816 = calcAcc(x830[0], x845[111 + (256 * get_local_id(1))], e158, e157, x816);
          x816 = calcAcc(x830[0], x845[112 + (256 * get_local_id(1))], e158, e157, x816);
          x816 = calcAcc(x830[0], x845[113 + (256 * get_local_id(1))], e158, e157, x816);
          x816 = calcAcc(x830[0], x845[114 + (256 * get_local_id(1))], e158, e157, x816);
          x816 = calcAcc(x830[0], x845[115 + (256 * get_local_id(1))], e158, e157, x816);
          x816 = calcAcc(x830[0], x845[116 + (256 * get_local_id(1))], e158, e157, x816);
          x816 = calcAcc(x830[0], x845[117 + (256 * get_local_id(1))], e158, e157, x816);
          x816 = calcAcc(x830[0], x845[118 + (256 * get_local_id(1))], e158, e157, x816);
          x816 = calcAcc(x830[0], x845[119 + (256 * get_local_id(1))], e158, e157, x816);
          x816 = calcAcc(x830[0], x845[120 + (256 * get_local_id(1))], e158, e157, x816);
          x816 = calcAcc(x830[0], x845[121 + (256 * get_local_id(1))], e158, e157, x816);
          x816 = calcAcc(x830[0], x845[122 + (256 * get_local_id(1))], e158, e157, x816);
          x816 = calcAcc(x830[0], x845[123 + (256 * get_local_id(1))], e158, e157, x816);
          x816 = calcAcc(x830[0], x845[124 + (256 * get_local_id(1))], e158, e157, x816);
          x816 = calcAcc(x830[0], x845[125 + (256 * get_local_id(1))], e158, e157, x816);
          x816 = calcAcc(x830[0], x845[126 + (256 * get_local_id(1))], e158, e157, x816);
          x816 = calcAcc(x830[0], x845[127 + (256 * get_local_id(1))], e158, e157, x816);
          x816 = calcAcc(x830[0], x845[128 + (256 * get_local_id(1))], e158, e157, x816);
          x816 = calcAcc(x830[0], x845[129 + (256 * get_local_id(1))], e158, e157, x816);
          x816 = calcAcc(x830[0], x845[130 + (256 * get_local_id(1))], e158, e157, x816);
          x816 = calcAcc(x830[0], x845[131 + (256 * get_local_id(1))], e158, e157, x816);
          x816 = calcAcc(x830[0], x845[132 + (256 * get_local_id(1))], e158, e157, x816);
          x816 = calcAcc(x830[0], x845[133 + (256 * get_local_id(1))], e158, e157, x816);
          x816 = calcAcc(x830[0], x845[134 + (256 * get_local_id(1))], e158, e157, x816);
          x816 = calcAcc(x830[0], x845[135 + (256 * get_local_id(1))], e158, e157, x816);
          x816 = calcAcc(x830[0], x845[136 + (256 * get_local_id(1))], e158, e157, x816);
          x816 = calcAcc(x830[0], x845[137 + (256 * get_local_id(1))], e158, e157, x816);
          x816 = calcAcc(x830[0], x845[138 + (256 * get_local_id(1))], e158, e157, x816);
          x816 = calcAcc(x830[0], x845[139 + (256 * get_local_id(1))], e158, e157, x816);
          x816 = calcAcc(x830[0], x845[140 + (256 * get_local_id(1))], e158, e157, x816);
          x816 = calcAcc(x830[0], x845[141 + (256 * get_local_id(1))], e158, e157, x816);
          x816 = calcAcc(x830[0], x845[142 + (256 * get_local_id(1))], e158, e157, x816);
          x816 = calcAcc(x830[0], x845[143 + (256 * get_local_id(1))], e158, e157, x816);
          x816 = calcAcc(x830[0], x845[144 + (256 * get_local_id(1))], e158, e157, x816);
          x816 = calcAcc(x830[0], x845[145 + (256 * get_local_id(1))], e158, e157, x816);
          x816 = calcAcc(x830[0], x845[146 + (256 * get_local_id(1))], e158, e157, x816);
          x816 = calcAcc(x830[0], x845[147 + (256 * get_local_id(1))], e158, e157, x816);
          x816 = calcAcc(x830[0], x845[148 + (256 * get_local_id(1))], e158, e157, x816);
          x816 = calcAcc(x830[0], x845[149 + (256 * get_local_id(1))], e158, e157, x816);
          x816 = calcAcc(x830[0], x845[150 + (256 * get_local_id(1))], e158, e157, x816);
          x816 = calcAcc(x830[0], x845[151 + (256 * get_local_id(1))], e158, e157, x816);
          x816 = calcAcc(x830[0], x845[152 + (256 * get_local_id(1))], e158, e157, x816);
          x816 = calcAcc(x830[0], x845[153 + (256 * get_local_id(1))], e158, e157, x816);
          x816 = calcAcc(x830[0], x845[154 + (256 * get_local_id(1))], e158, e157, x816);
          x816 = calcAcc(x830[0], x845[155 + (256 * get_local_id(1))], e158, e157, x816);
          x816 = calcAcc(x830[0], x845[156 + (256 * get_local_id(1))], e158, e157, x816);
          x816 = calcAcc(x830[0], x845[157 + (256 * get_local_id(1))], e158, e157, x816);
          x816 = calcAcc(x830[0], x845[158 + (256 * get_local_id(1))], e158, e157, x816);
          x816 = calcAcc(x830[0], x845[159 + (256 * get_local_id(1))], e158, e157, x816);
          x816 = calcAcc(x830[0], x845[160 + (256 * get_local_id(1))], e158, e157, x816);
          x816 = calcAcc(x830[0], x845[161 + (256 * get_local_id(1))], e158, e157, x816);
          x816 = calcAcc(x830[0], x845[162 + (256 * get_local_id(1))], e158, e157, x816);
          x816 = calcAcc(x830[0], x845[163 + (256 * get_local_id(1))], e158, e157, x816);
          x816 = calcAcc(x830[0], x845[164 + (256 * get_local_id(1))], e158, e157, x816);
          x816 = calcAcc(x830[0], x845[165 + (256 * get_local_id(1))], e158, e157, x816);
          x816 = calcAcc(x830[0], x845[166 + (256 * get_local_id(1))], e158, e157, x816);
          x816 = calcAcc(x830[0], x845[167 + (256 * get_local_id(1))], e158, e157, x816);
          x816 = calcAcc(x830[0], x845[168 + (256 * get_local_id(1))], e158, e157, x816);
          x816 = calcAcc(x830[0], x845[169 + (256 * get_local_id(1))], e158, e157, x816);
          x816 = calcAcc(x830[0], x845[170 + (256 * get_local_id(1))], e158, e157, x816);
          x816 = calcAcc(x830[0], x845[171 + (256 * get_local_id(1))], e158, e157, x816);
          x816 = calcAcc(x830[0], x845[172 + (256 * get_local_id(1))], e158, e157, x816);
          x816 = calcAcc(x830[0], x845[173 + (256 * get_local_id(1))], e158, e157, x816);
          x816 = calcAcc(x830[0], x845[174 + (256 * get_local_id(1))], e158, e157, x816);
          x816 = calcAcc(x830[0], x845[175 + (256 * get_local_id(1))], e158, e157, x816);
          x816 = calcAcc(x830[0], x845[176 + (256 * get_local_id(1))], e158, e157, x816);
          x816 = calcAcc(x830[0], x845[177 + (256 * get_local_id(1))], e158, e157, x816);
          x816 = calcAcc(x830[0], x845[178 + (256 * get_local_id(1))], e158, e157, x816);
          x816 = calcAcc(x830[0], x845[179 + (256 * get_local_id(1))], e158, e157, x816);
          x816 = calcAcc(x830[0], x845[180 + (256 * get_local_id(1))], e158, e157, x816);
          x816 = calcAcc(x830[0], x845[181 + (256 * get_local_id(1))], e158, e157, x816);
          x816 = calcAcc(x830[0], x845[182 + (256 * get_local_id(1))], e158, e157, x816);
          x816 = calcAcc(x830[0], x845[183 + (256 * get_local_id(1))], e158, e157, x816);
          x816 = calcAcc(x830[0], x845[184 + (256 * get_local_id(1))], e158, e157, x816);
          x816 = calcAcc(x830[0], x845[185 + (256 * get_local_id(1))], e158, e157, x816);
          x816 = calcAcc(x830[0], x845[186 + (256 * get_local_id(1))], e158, e157, x816);
          x816 = calcAcc(x830[0], x845[187 + (256 * get_local_id(1))], e158, e157, x816);
          x816 = calcAcc(x830[0], x845[188 + (256 * get_local_id(1))], e158, e157, x816);
          x816 = calcAcc(x830[0], x845[189 + (256 * get_local_id(1))], e158, e157, x816);
          x816 = calcAcc(x830[0], x845[190 + (256 * get_local_id(1))], e158, e157, x816);
          x816 = calcAcc(x830[0], x845[191 + (256 * get_local_id(1))], e158, e157, x816);
          x816 = calcAcc(x830[0], x845[192 + (256 * get_local_id(1))], e158, e157, x816);
          x816 = calcAcc(x830[0], x845[193 + (256 * get_local_id(1))], e158, e157, x816);
          x816 = calcAcc(x830[0], x845[194 + (256 * get_local_id(1))], e158, e157, x816);
          x816 = calcAcc(x830[0], x845[195 + (256 * get_local_id(1))], e158, e157, x816);
          x816 = calcAcc(x830[0], x845[196 + (256 * get_local_id(1))], e158, e157, x816);
          x816 = calcAcc(x830[0], x845[197 + (256 * get_local_id(1))], e158, e157, x816);
          x816 = calcAcc(x830[0], x845[198 + (256 * get_local_id(1))], e158, e157, x816);
          x816 = calcAcc(x830[0], x845[199 + (256 * get_local_id(1))], e158, e157, x816);
          x816 = calcAcc(x830[0], x845[200 + (256 * get_local_id(1))], e158, e157, x816);
          x816 = calcAcc(x830[0], x845[201 + (256 * get_local_id(1))], e158, e157, x816);
          x816 = calcAcc(x830[0], x845[202 + (256 * get_local_id(1))], e158, e157, x816);
          x816 = calcAcc(x830[0], x845[203 + (256 * get_local_id(1))], e158, e157, x816);
          x816 = calcAcc(x830[0], x845[204 + (256 * get_local_id(1))], e158, e157, x816);
          x816 = calcAcc(x830[0], x845[205 + (256 * get_local_id(1))], e158, e157, x816);
          x816 = calcAcc(x830[0], x845[206 + (256 * get_local_id(1))], e158, e157, x816);
          x816 = calcAcc(x830[0], x845[207 + (256 * get_local_id(1))], e158, e157, x816);
          x816 = calcAcc(x830[0], x845[208 + (256 * get_local_id(1))], e158, e157, x816);
          x816 = calcAcc(x830[0], x845[209 + (256 * get_local_id(1))], e158, e157, x816);
          x816 = calcAcc(x830[0], x845[210 + (256 * get_local_id(1))], e158, e157, x816);
          x816 = calcAcc(x830[0], x845[211 + (256 * get_local_id(1))], e158, e157, x816);
          x816 = calcAcc(x830[0], x845[212 + (256 * get_local_id(1))], e158, e157, x816);
          x816 = calcAcc(x830[0], x845[213 + (256 * get_local_id(1))], e158, e157, x816);
          x816 = calcAcc(x830[0], x845[214 + (256 * get_local_id(1))], e158, e157, x816);
          x816 = calcAcc(x830[0], x845[215 + (256 * get_local_id(1))], e158, e157, x816);
          x816 = calcAcc(x830[0], x845[216 + (256 * get_local_id(1))], e158, e157, x816);
          x816 = calcAcc(x830[0], x845[217 + (256 * get_local_id(1))], e158, e157, x816);
          x816 = calcAcc(x830[0], x845[218 + (256 * get_local_id(1))], e158, e157, x816);
          x816 = calcAcc(x830[0], x845[219 + (256 * get_local_id(1))], e158, e157, x816);
          x816 = calcAcc(x830[0], x845[220 + (256 * get_local_id(1))], e158, e157, x816);
          x816 = calcAcc(x830[0], x845[221 + (256 * get_local_id(1))], e158, e157, x816);
          x816 = calcAcc(x830[0], x845[222 + (256 * get_local_id(1))], e158, e157, x816);
          x816 = calcAcc(x830[0], x845[223 + (256 * get_local_id(1))], e158, e157, x816);
          x816 = calcAcc(x830[0], x845[224 + (256 * get_local_id(1))], e158, e157, x816);
          x816 = calcAcc(x830[0], x845[225 + (256 * get_local_id(1))], e158, e157, x816);
          x816 = calcAcc(x830[0], x845[226 + (256 * get_local_id(1))], e158, e157, x816);
          x816 = calcAcc(x830[0], x845[227 + (256 * get_local_id(1))], e158, e157, x816);
          x816 = calcAcc(x830[0], x845[228 + (256 * get_local_id(1))], e158, e157, x816);
          x816 = calcAcc(x830[0], x845[229 + (256 * get_local_id(1))], e158, e157, x816);
          x816 = calcAcc(x830[0], x845[230 + (256 * get_local_id(1))], e158, e157, x816);
          x816 = calcAcc(x830[0], x845[231 + (256 * get_local_id(1))], e158, e157, x816);
          x816 = calcAcc(x830[0], x845[232 + (256 * get_local_id(1))], e158, e157, x816);
          x816 = calcAcc(x830[0], x845[233 + (256 * get_local_id(1))], e158, e157, x816);
          x816 = calcAcc(x830[0], x845[234 + (256 * get_local_id(1))], e158, e157, x816);
          x816 = calcAcc(x830[0], x845[235 + (256 * get_local_id(1))], e158, e157, x816);
          x816 = calcAcc(x830[0], x845[236 + (256 * get_local_id(1))], e158, e157, x816);
          x816 = calcAcc(x830[0], x845[237 + (256 * get_local_id(1))], e158, e157, x816);
          x816 = calcAcc(x830[0], x845[238 + (256 * get_local_id(1))], e158, e157, x816);
          x816 = calcAcc(x830[0], x845[239 + (256 * get_local_id(1))], e158, e157, x816);
          x816 = calcAcc(x830[0], x845[240 + (256 * get_local_id(1))], e158, e157, x816);
          x816 = calcAcc(x830[0], x845[241 + (256 * get_local_id(1))], e158, e157, x816);
          x816 = calcAcc(x830[0], x845[242 + (256 * get_local_id(1))], e158, e157, x816);
          x816 = calcAcc(x830[0], x845[243 + (256 * get_local_id(1))], e158, e157, x816);
          x816 = calcAcc(x830[0], x845[244 + (256 * get_local_id(1))], e158, e157, x816);
          x816 = calcAcc(x830[0], x845[245 + (256 * get_local_id(1))], e158, e157, x816);
          x816 = calcAcc(x830[0], x845[246 + (256 * get_local_id(1))], e158, e157, x816);
          x816 = calcAcc(x830[0], x845[247 + (256 * get_local_id(1))], e158, e157, x816);
          x816 = calcAcc(x830[0], x845[248 + (256 * get_local_id(1))], e158, e157, x816);
          x816 = calcAcc(x830[0], x845[249 + (256 * get_local_id(1))], e158, e157, x816);
          x816 = calcAcc(x830[0], x845[250 + (256 * get_local_id(1))], e158, e157, x816);
          x816 = calcAcc(x830[0], x845[251 + (256 * get_local_id(1))], e158, e157, x816);
          x816 = calcAcc(x830[0], x845[252 + (256 * get_local_id(1))], e158, e157, x816);
          x816 = calcAcc(x830[0], x845[253 + (256 * get_local_id(1))], e158, e157, x816);
          x816 = calcAcc(x830[0], x845[254 + (256 * get_local_id(1))], e158, e157, x816);
          x816 = calcAcc(x830[0], x845[255 + (256 * get_local_id(1))], e158, e157, x816);
          x859[0] = x816;
        }

      }

      barrier(CLK_LOCAL_MEM_FENCE);
    }

    /* mapLocal */
    /* unrolling loop of 1 */
    {
      float4 x779[1];
      /* mapLocal */
      /* unrolling loop of 1 */
      x779[0] = e155[((256 * wg_id_894) + (n154 * wg_id_893)) + get_local_id(0)];
      /* mapLocal */
      /* unrolling loop of 1 */
      {
        struct Record_float4_float4 x765;
        x765 = update(x779[0], e156[((256 * wg_id_894) + (n154 * wg_id_893)) + get_local_id(0)], e158, x859[0]);
        output[(((256 * wg_id_894) + (256 * get_local_id(1))) + (n154 * wg_id_893)) + get_local_id(0)]._fst = x765._fst;
        output[(((256 * wg_id_894) + (256 * get_local_id(1))) + (n154 * wg_id_893)) + get_local_id(0)]._snd = x765._snd;
      }

    }

  }

}
