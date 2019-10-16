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
float4 id4(float4 x){
  { return x; }
}
float add(float x, float y){
  { return x+y; }
}
float mult(float l, float r){
  { return l * r; }
}
kernel void KERNEL(const global float* restrict v__34, const global float* restrict v__35, global float* v__50, int v_K_2, int v_M_1, int v_N_0){ 
#ifndef WORKGROUP_GUARD
#define WORKGROUP_GUARD
#endif
WORKGROUP_GUARD
{
  /* Static local memory */
  /* Typed Value memory */
  float v__37;
  /* Private Memory */
  float v__38_0;
  float v__38_1;
  float v__38_2;
  float v__38_3;
  float v__38_4;
  float v__38_5;
  float v__38_6;
  float v__38_7;
  float v__38_8;
  float v__38_9;
  float v__38_10;
  float v__38_11;
  float v__38_12;
  float v__38_13;
  float v__38_14;
  float v__38_15;
  float v__38_16;
  float v__38_17;
  float v__38_18;
  float v__38_19;
  float v__38_20;
  float v__38_21;
  float v__38_22;
  float v__38_23;
  float v__38_24;
  float v__38_25;
  float v__38_26;
  float v__38_27;
  float v__38_28;
  float v__38_29;
  float v__38_30;
  float v__38_31;
  
  float v__41_0;
  float v__41_1;
  float v__41_2;
  float v__41_3;
  float v__41_4;
  float v__41_5;
  float v__41_6;
  float v__41_7;
  
  float4 v__42_0;
  
  float v__47_0;
  float v__47_1;
  float v__47_2;
  float v__47_3;
  float v__47_4;
  float v__47_5;
  float v__47_6;
  float v__47_7;
  float v__47_8;
  float v__47_9;
  float v__47_10;
  float v__47_11;
  float v__47_12;
  float v__47_13;
  float v__47_14;
  float v__47_15;
  float v__47_16;
  float v__47_17;
  float v__47_18;
  float v__47_19;
  float v__47_20;
  float v__47_21;
  float v__47_22;
  float v__47_23;
  float v__47_24;
  float v__47_25;
  float v__47_26;
  float v__47_27;
  float v__47_28;
  float v__47_29;
  float v__47_30;
  float v__47_31;
  
  /* iteration count is exactly 1, no loop emitted */
  {
    int v_gl_id_20 = get_global_id(1);
    /* iteration count is exactly 1, no loop emitted */
    {
      int v_gl_id_21 = get_global_id(0);
      float v_tmp_108 = 0.0f;
      v__37 = v_tmp_108;
      /* map_seq */
      /* unroll */
      /* map_seq */
      /* unroll */
      v__38_0 = id(v__37);
      v__38_1 = id(v__37);
      v__38_2 = id(v__37);
      v__38_3 = id(v__37);
      /* end unroll */
      /* end map_seq */
      /* map_seq */
      /* unroll */
      v__38_4 = id(v__37);
      v__38_5 = id(v__37);
      v__38_6 = id(v__37);
      v__38_7 = id(v__37);
      /* end unroll */
      /* end map_seq */
      /* map_seq */
      /* unroll */
      v__38_8 = id(v__37);
      v__38_9 = id(v__37);
      v__38_10 = id(v__37);
      v__38_11 = id(v__37);
      /* end unroll */
      /* end map_seq */
      /* map_seq */
      /* unroll */
      v__38_12 = id(v__37);
      v__38_13 = id(v__37);
      v__38_14 = id(v__37);
      v__38_15 = id(v__37);
      /* end unroll */
      /* end map_seq */
      /* map_seq */
      /* unroll */
      v__38_16 = id(v__37);
      v__38_17 = id(v__37);
      v__38_18 = id(v__37);
      v__38_19 = id(v__37);
      /* end unroll */
      /* end map_seq */
      /* map_seq */
      /* unroll */
      v__38_20 = id(v__37);
      v__38_21 = id(v__37);
      v__38_22 = id(v__37);
      v__38_23 = id(v__37);
      /* end unroll */
      /* end map_seq */
      /* map_seq */
      /* unroll */
      v__38_24 = id(v__37);
      v__38_25 = id(v__37);
      v__38_26 = id(v__37);
      v__38_27 = id(v__37);
      /* end unroll */
      /* end map_seq */
      /* map_seq */
      /* unroll */
      v__38_28 = id(v__37);
      v__38_29 = id(v__37);
      v__38_30 = id(v__37);
      v__38_31 = id(v__37);
      /* end unroll */
      /* end map_seq */
      /* end unroll */
      /* end map_seq */
      /* reduce_seq */
      for (int v_i_24 = 0;v_i_24<v_K_2;v_i_24 = (1 + v_i_24)){
        /* map_seq */
        /* unroll */
        v__41_0 = id(v__34[((8 * v_gl_id_20) + (v_M_1 * v_i_24))]);
        v__41_1 = id(v__34[(1 + (8 * v_gl_id_20) + (v_M_1 * v_i_24))]);
        v__41_2 = id(v__34[(2 + (8 * v_gl_id_20) + (v_M_1 * v_i_24))]);
        v__41_3 = id(v__34[(3 + (8 * v_gl_id_20) + (v_M_1 * v_i_24))]);
        v__41_4 = id(v__34[(4 + (8 * v_gl_id_20) + (v_M_1 * v_i_24))]);
        v__41_5 = id(v__34[(5 + (8 * v_gl_id_20) + (v_M_1 * v_i_24))]);
        v__41_6 = id(v__34[(6 + (8 * v_gl_id_20) + (v_M_1 * v_i_24))]);
        v__41_7 = id(v__34[(7 + (8 * v_gl_id_20) + (v_M_1 * v_i_24))]);
        /* end unroll */
        /* end map_seq */
        /* map_seq */
        /* unroll */
        v__42_0 = id4(vload4((v_gl_id_21 + ((v_N_0 * v_i_24) / 4)),v__35));
        /* end unroll */
        /* end map_seq */
        /* map_seq */
        /* unroll */
        /* map_seq */
        /* unroll */
        v__47_0 = mult(v__41_0, v__42_0.s0);
        v__38_0 = add(v__38_0, v__47_0);
        v__47_1 = mult(v__41_0, v__42_0.s1);
        v__38_1 = add(v__38_1, v__47_1);
        v__47_2 = mult(v__41_0, v__42_0.s2);
        v__38_2 = add(v__38_2, v__47_2);
        v__47_3 = mult(v__41_0, v__42_0.s3);
        v__38_3 = add(v__38_3, v__47_3);
        /* end unroll */
        /* end map_seq */
        /* map_seq */
        /* unroll */
        v__47_4 = mult(v__41_1, v__42_0.s0);
        v__38_4 = add(v__38_4, v__47_4);
        v__47_5 = mult(v__41_1, v__42_0.s1);
        v__38_5 = add(v__38_5, v__47_5);
        v__47_6 = mult(v__41_1, v__42_0.s2);
        v__38_6 = add(v__38_6, v__47_6);
        v__47_7 = mult(v__41_1, v__42_0.s3);
        v__38_7 = add(v__38_7, v__47_7);
        /* end unroll */
        /* end map_seq */
        /* map_seq */
        /* unroll */
        v__47_8 = mult(v__41_2, v__42_0.s0);
        v__38_8 = add(v__38_8, v__47_8);
        v__47_9 = mult(v__41_2, v__42_0.s1);
        v__38_9 = add(v__38_9, v__47_9);
        v__47_10 = mult(v__41_2, v__42_0.s2);
        v__38_10 = add(v__38_10, v__47_10);
        v__47_11 = mult(v__41_2, v__42_0.s3);
        v__38_11 = add(v__38_11, v__47_11);
        /* end unroll */
        /* end map_seq */
        /* map_seq */
        /* unroll */
        v__47_12 = mult(v__41_3, v__42_0.s0);
        v__38_12 = add(v__38_12, v__47_12);
        v__47_13 = mult(v__41_3, v__42_0.s1);
        v__38_13 = add(v__38_13, v__47_13);
        v__47_14 = mult(v__41_3, v__42_0.s2);
        v__38_14 = add(v__38_14, v__47_14);
        v__47_15 = mult(v__41_3, v__42_0.s3);
        v__38_15 = add(v__38_15, v__47_15);
        /* end unroll */
        /* end map_seq */
        /* map_seq */
        /* unroll */
        v__47_16 = mult(v__41_4, v__42_0.s0);
        v__38_16 = add(v__38_16, v__47_16);
        v__47_17 = mult(v__41_4, v__42_0.s1);
        v__38_17 = add(v__38_17, v__47_17);
        v__47_18 = mult(v__41_4, v__42_0.s2);
        v__38_18 = add(v__38_18, v__47_18);
        v__47_19 = mult(v__41_4, v__42_0.s3);
        v__38_19 = add(v__38_19, v__47_19);
        /* end unroll */
        /* end map_seq */
        /* map_seq */
        /* unroll */
        v__47_20 = mult(v__41_5, v__42_0.s0);
        v__38_20 = add(v__38_20, v__47_20);
        v__47_21 = mult(v__41_5, v__42_0.s1);
        v__38_21 = add(v__38_21, v__47_21);
        v__47_22 = mult(v__41_5, v__42_0.s2);
        v__38_22 = add(v__38_22, v__47_22);
        v__47_23 = mult(v__41_5, v__42_0.s3);
        v__38_23 = add(v__38_23, v__47_23);
        /* end unroll */
        /* end map_seq */
        /* map_seq */
        /* unroll */
        v__47_24 = mult(v__41_6, v__42_0.s0);
        v__38_24 = add(v__38_24, v__47_24);
        v__47_25 = mult(v__41_6, v__42_0.s1);
        v__38_25 = add(v__38_25, v__47_25);
        v__47_26 = mult(v__41_6, v__42_0.s2);
        v__38_26 = add(v__38_26, v__47_26);
        v__47_27 = mult(v__41_6, v__42_0.s3);
        v__38_27 = add(v__38_27, v__47_27);
        /* end unroll */
        /* end map_seq */
        /* map_seq */
        /* unroll */
        v__47_28 = mult(v__41_7, v__42_0.s0);
        v__38_28 = add(v__38_28, v__47_28);
        v__47_29 = mult(v__41_7, v__42_0.s1);
        v__38_29 = add(v__38_29, v__47_29);
        v__47_30 = mult(v__41_7, v__42_0.s2);
        v__38_30 = add(v__38_30, v__47_30);
        v__47_31 = mult(v__41_7, v__42_0.s3);
        v__38_31 = add(v__38_31, v__47_31);
        /* end unroll */
        /* end map_seq */
        /* end unroll */
        /* end map_seq */
      }
      /* end reduce_seq */
      /* map_seq */
      /* unroll */
      /* map_seq */
      /* unroll */
      /* map_seq */
      /* unroll */
      vstore4(id4((float4)(v__38_0, v__38_1, v__38_2, v__38_3)),(v_gl_id_21 + (2 * v_N_0 * v_gl_id_20)),v__50);;
      /* end unroll */
      /* end map_seq */
      /* map_seq */
      /* unroll */
      vstore4(id4((float4)(v__38_4, v__38_5, v__38_6, v__38_7)),(v_gl_id_21 + (v_N_0 / 4) + (2 * v_N_0 * v_gl_id_20)),v__50);;
      /* end unroll */
      /* end map_seq */
      /* map_seq */
      /* unroll */
      vstore4(id4((float4)(v__38_8, v__38_9, v__38_10, v__38_11)),(v_gl_id_21 + (v_N_0 / 2) + (2 * v_N_0 * v_gl_id_20)),v__50);;
      /* end unroll */
      /* end map_seq */
      /* map_seq */
      /* unroll */
      vstore4(id4((float4)(v__38_12, v__38_13, v__38_14, v__38_15)),(v_gl_id_21 + ((3 * v_N_0) / 4) + (2 * v_N_0 * v_gl_id_20)),v__50);;
      /* end unroll */
      /* end map_seq */
      /* map_seq */
      /* unroll */
      vstore4(id4((float4)(v__38_16, v__38_17, v__38_18, v__38_19)),(v_N_0 + v_gl_id_21 + (2 * v_N_0 * v_gl_id_20)),v__50);;
      /* end unroll */
      /* end map_seq */
      /* map_seq */
      /* unroll */
      vstore4(id4((float4)(v__38_20, v__38_21, v__38_22, v__38_23)),(v_gl_id_21 + ((5 * v_N_0) / 4) + (2 * v_N_0 * v_gl_id_20)),v__50);;
      /* end unroll */
      /* end map_seq */
      /* map_seq */
      /* unroll */
      vstore4(id4((float4)(v__38_24, v__38_25, v__38_26, v__38_27)),(v_gl_id_21 + ((3 * v_N_0) / 2) + (2 * v_N_0 * v_gl_id_20)),v__50);;
      /* end unroll */
      /* end map_seq */
      /* map_seq */
      /* unroll */
      vstore4(id4((float4)(v__38_28, v__38_29, v__38_30, v__38_31)),(v_gl_id_21 + ((7 * v_N_0) / 4) + (2 * v_N_0 * v_gl_id_20)),v__50);;
      /* end unroll */
      /* end map_seq */
      /* end unroll */
      /* end map_seq */
      /* end unroll */
      /* end map_seq */
    }
  }
}}

