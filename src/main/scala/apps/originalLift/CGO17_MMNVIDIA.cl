#ifndef Tuple_float_float_DEFINED
#define Tuple_float_float_DEFINED
typedef struct {
  float _0;
  float _1;
} Tuple_float_float;
#endif

float idfloat(float x){
  { return x; }
}
float4 idfloat4(float4 x){
  { return x; }
}
float add(float x, float y){
  { return x+y; }
}
float mult(float l, float r){
  { return l * r; }
}
kernel void KERNEL(const global float* restrict v__70, const global float* restrict v__71, global float* v__93, int v_K_4, int v_M_3, int v_N_5){
  /* Static local memory */
  local float v__78[2048] __attribute__ ((aligned(16)));
  local float v__77[1024] __attribute__ ((aligned(16)));
  /* Typed Value memory */
  float v__73;
  /* Private Memory */
  float v__74_0;
  float v__74_1;
  float v__74_2;
  float v__74_3;
  float v__74_4;
  float v__74_5;
  float v__74_6;
  float v__74_7;
  float v__74_8;
  float v__74_9;
  float v__74_10;
  float v__74_11;
  float v__74_12;
  float v__74_13;
  float v__74_14;
  float v__74_15;
  float v__74_16;
  float v__74_17;
  float v__74_18;
  float v__74_19;
  float v__74_20;
  float v__74_21;
  float v__74_22;
  float v__74_23;
  float v__74_24;
  float v__74_25;
  float v__74_26;
  float v__74_27;
  float v__74_28;
  float v__74_29;
  float v__74_30;
  float v__74_31;
  
  float v__84_0;
  float v__84_1;
  float v__84_2;
  float v__84_3;
  float v__84_4;
  float v__84_5;
  float v__84_6;
  float v__84_7;
  
  float v__85_0;
  float v__85_1;
  float v__85_2;
  float v__85_3;
  
  float v__90_0;
  float v__90_1;
  float v__90_2;
  float v__90_3;
  float v__90_4;
  float v__90_5;
  float v__90_6;
  float v__90_7;
  float v__90_8;
  float v__90_9;
  float v__90_10;
  float v__90_11;
  float v__90_12;
  float v__90_13;
  float v__90_14;
  float v__90_15;
  float v__90_16;
  float v__90_17;
  float v__90_18;
  float v__90_19;
  float v__90_20;
  float v__90_21;
  float v__90_22;
  float v__90_23;
  float v__90_24;
  float v__90_25;
  float v__90_26;
  float v__90_27;
  float v__90_28;
  float v__90_29;
  float v__90_30;
  float v__90_31;
  
  /* iteration count is exactly 1, no loop emitted */
  {
    int v_wg_id_40 = get_group_id(1);
    /* iteration count is exactly 1, no loop emitted */
    {
      int v_wg_id_41 = get_group_id(0);
      float v_tmp_209 = 0.0f;
      v__73 = v_tmp_209;
      /* unroll */
      /* unroll */
      /* map_seq */
      /* unroll */
      /* map_seq */
      /* unroll */
      v__74_0 = idfloat(v__73);
      v__74_1 = idfloat(v__73);
      v__74_2 = idfloat(v__73);
      v__74_3 = idfloat(v__73);
      /* end unroll */
      /* end map_seq */
      /* map_seq */
      /* unroll */
      v__74_4 = idfloat(v__73);
      v__74_5 = idfloat(v__73);
      v__74_6 = idfloat(v__73);
      v__74_7 = idfloat(v__73);
      /* end unroll */
      /* end map_seq */
      /* map_seq */
      /* unroll */
      v__74_8 = idfloat(v__73);
      v__74_9 = idfloat(v__73);
      v__74_10 = idfloat(v__73);
      v__74_11 = idfloat(v__73);
      /* end unroll */
      /* end map_seq */
      /* map_seq */
      /* unroll */
      v__74_12 = idfloat(v__73);
      v__74_13 = idfloat(v__73);
      v__74_14 = idfloat(v__73);
      v__74_15 = idfloat(v__73);
      /* end unroll */
      /* end map_seq */
      /* map_seq */
      /* unroll */
      v__74_16 = idfloat(v__73);
      v__74_17 = idfloat(v__73);
      v__74_18 = idfloat(v__73);
      v__74_19 = idfloat(v__73);
      /* end unroll */
      /* end map_seq */
      /* map_seq */
      /* unroll */
      v__74_20 = idfloat(v__73);
      v__74_21 = idfloat(v__73);
      v__74_22 = idfloat(v__73);
      v__74_23 = idfloat(v__73);
      /* end unroll */
      /* end map_seq */
      /* map_seq */
      /* unroll */
      v__74_24 = idfloat(v__73);
      v__74_25 = idfloat(v__73);
      v__74_26 = idfloat(v__73);
      v__74_27 = idfloat(v__73);
      /* end unroll */
      /* end map_seq */
      /* map_seq */
      /* unroll */
      v__74_28 = idfloat(v__73);
      v__74_29 = idfloat(v__73);
      v__74_30 = idfloat(v__73);
      v__74_31 = idfloat(v__73);
      /* end unroll */
      /* end map_seq */
      /* end unroll */
      /* end map_seq */
      /* end unroll */
      /* end unroll */
      /* reduce_seq */
      for (int v_i_46 = 0;v_i_46<(v_K_4 / (16));v_i_46 = (1 + v_i_46)){
        /* iteration count is exactly 1, no loop emitted */
        {
          int v_l_id_51 = get_local_id(1);
          /* iteration count is exactly 1, no loop emitted */
          {
            int v_l_id_52 = get_local_id(0);
            *( ((local float4*)v__77) + (v_l_id_52 + (32 * v_l_id_51))) = idfloat4(*( ((global float4*)v__70) + (((((4 * v_l_id_52) % 64) + (2 * v_M_3 * v_l_id_51) + (v_M_3 * (v_l_id_52 / 16))) / 4) + (4 * v_M_3 * v_i_46) + (16 * ((v_wg_id_40 + (v_M_3 * v_l_id_51 / (32))) % (v_M_3 / (64)))))));
          }
          barrier(CLK_LOCAL_MEM_FENCE);
          
        }
        barrier(CLK_LOCAL_MEM_FENCE);
        
        for (int v_l_id_53 = get_local_id(1);v_l_id_53<16;v_l_id_53 = (8 + v_l_id_53)){
          /* iteration count is exactly 1, no loop emitted */
          {
            int v_l_id_54 = get_local_id(0);
            *( ((local float4*)v__78) + (v_l_id_54 + (32 * v_l_id_53))) = idfloat4(*( ((global float4*)v__71) + (v_l_id_54 + ((v_N_5 * v_l_id_53) / 4) + (4 * v_N_5 * v_i_46) + (32 * v_wg_id_41))));
          }
          barrier(CLK_LOCAL_MEM_FENCE);
          
        }
        barrier(CLK_LOCAL_MEM_FENCE);
        
        /* unroll */
        /* unroll */
        /* reduce_seq */
        for (int v_i_57 = 0;v_i_57<16;v_i_57 = (1 + v_i_57)){
          /* map_seq */
          /* unroll */
          v__84_0 = idfloat(v__77[((8 * get_local_id(1)) + (64 * v_i_57))]);
          v__84_1 = idfloat(v__77[(1 + (8 * get_local_id(1)) + (64 * v_i_57))]);
          v__84_2 = idfloat(v__77[(2 + (8 * get_local_id(1)) + (64 * v_i_57))]);
          v__84_3 = idfloat(v__77[(3 + (8 * get_local_id(1)) + (64 * v_i_57))]);
          v__84_4 = idfloat(v__77[(4 + (8 * get_local_id(1)) + (64 * v_i_57))]);
          v__84_5 = idfloat(v__77[(5 + (8 * get_local_id(1)) + (64 * v_i_57))]);
          v__84_6 = idfloat(v__77[(6 + (8 * get_local_id(1)) + (64 * v_i_57))]);
          v__84_7 = idfloat(v__77[(7 + (8 * get_local_id(1)) + (64 * v_i_57))]);
          /* end unroll */
          /* end map_seq */
          /* map_seq */
          /* unroll */
          v__85_0 = idfloat(v__78[((4 * get_local_id(0)) + (128 * v_i_57))]);
          v__85_1 = idfloat(v__78[(1 + (4 * get_local_id(0)) + (128 * v_i_57))]);
          v__85_2 = idfloat(v__78[(2 + (4 * get_local_id(0)) + (128 * v_i_57))]);
          v__85_3 = idfloat(v__78[(3 + (4 * get_local_id(0)) + (128 * v_i_57))]);
          /* end unroll */
          /* end map_seq */
          /* map_seq */
          /* unroll */
          /* map_seq */
          /* unroll */
          v__90_0 = mult(v__84_0, v__85_0);
          v__74_0 = add(v__74_0, v__90_0);
          v__90_1 = mult(v__84_0, v__85_1);
          v__74_1 = add(v__74_1, v__90_1);
          v__90_2 = mult(v__84_0, v__85_2);
          v__74_2 = add(v__74_2, v__90_2);
          v__90_3 = mult(v__84_0, v__85_3);
          v__74_3 = add(v__74_3, v__90_3);
          /* end unroll */
          /* end map_seq */
          /* map_seq */
          /* unroll */
          v__90_4 = mult(v__84_1, v__85_0);
          v__74_4 = add(v__74_4, v__90_4);
          v__90_5 = mult(v__84_1, v__85_1);
          v__74_5 = add(v__74_5, v__90_5);
          v__90_6 = mult(v__84_1, v__85_2);
          v__74_6 = add(v__74_6, v__90_6);
          v__90_7 = mult(v__84_1, v__85_3);
          v__74_7 = add(v__74_7, v__90_7);
          /* end unroll */
          /* end map_seq */
          /* map_seq */
          /* unroll */
          v__90_8 = mult(v__84_2, v__85_0);
          v__74_8 = add(v__74_8, v__90_8);
          v__90_9 = mult(v__84_2, v__85_1);
          v__74_9 = add(v__74_9, v__90_9);
          v__90_10 = mult(v__84_2, v__85_2);
          v__74_10 = add(v__74_10, v__90_10);
          v__90_11 = mult(v__84_2, v__85_3);
          v__74_11 = add(v__74_11, v__90_11);
          /* end unroll */
          /* end map_seq */
          /* map_seq */
          /* unroll */
          v__90_12 = mult(v__84_3, v__85_0);
          v__74_12 = add(v__74_12, v__90_12);
          v__90_13 = mult(v__84_3, v__85_1);
          v__74_13 = add(v__74_13, v__90_13);
          v__90_14 = mult(v__84_3, v__85_2);
          v__74_14 = add(v__74_14, v__90_14);
          v__90_15 = mult(v__84_3, v__85_3);
          v__74_15 = add(v__74_15, v__90_15);
          /* end unroll */
          /* end map_seq */
          /* map_seq */
          /* unroll */
          v__90_16 = mult(v__84_4, v__85_0);
          v__74_16 = add(v__74_16, v__90_16);
          v__90_17 = mult(v__84_4, v__85_1);
          v__74_17 = add(v__74_17, v__90_17);
          v__90_18 = mult(v__84_4, v__85_2);
          v__74_18 = add(v__74_18, v__90_18);
          v__90_19 = mult(v__84_4, v__85_3);
          v__74_19 = add(v__74_19, v__90_19);
          /* end unroll */
          /* end map_seq */
          /* map_seq */
          /* unroll */
          v__90_20 = mult(v__84_5, v__85_0);
          v__74_20 = add(v__74_20, v__90_20);
          v__90_21 = mult(v__84_5, v__85_1);
          v__74_21 = add(v__74_21, v__90_21);
          v__90_22 = mult(v__84_5, v__85_2);
          v__74_22 = add(v__74_22, v__90_22);
          v__90_23 = mult(v__84_5, v__85_3);
          v__74_23 = add(v__74_23, v__90_23);
          /* end unroll */
          /* end map_seq */
          /* map_seq */
          /* unroll */
          v__90_24 = mult(v__84_6, v__85_0);
          v__74_24 = add(v__74_24, v__90_24);
          v__90_25 = mult(v__84_6, v__85_1);
          v__74_25 = add(v__74_25, v__90_25);
          v__90_26 = mult(v__84_6, v__85_2);
          v__74_26 = add(v__74_26, v__90_26);
          v__90_27 = mult(v__84_6, v__85_3);
          v__74_27 = add(v__74_27, v__90_27);
          /* end unroll */
          /* end map_seq */
          /* map_seq */
          /* unroll */
          v__90_28 = mult(v__84_7, v__85_0);
          v__74_28 = add(v__74_28, v__90_28);
          v__90_29 = mult(v__84_7, v__85_1);
          v__74_29 = add(v__74_29, v__90_29);
          v__90_30 = mult(v__84_7, v__85_2);
          v__74_30 = add(v__74_30, v__90_30);
          v__90_31 = mult(v__84_7, v__85_3);
          v__74_31 = add(v__74_31, v__90_31);
          /* end unroll */
          /* end map_seq */
          /* end unroll */
          /* end map_seq */
        }
        /* end reduce_seq */
        /* map_seq */
        /* unroll */
        /* end unroll */
        /* end map_seq */
        /* end unroll */
        /* end unroll */
        barrier(CLK_LOCAL_MEM_FENCE | CLK_GLOBAL_MEM_FENCE);
        
      }
      /* end reduce_seq */
      /* map_seq */
      /* unroll */
      /* unroll */
      /* unroll */
      /* map_seq */
      /* unroll */
      /* map_seq */
      /* unroll */
      *( ((global float4*)v__93) + ((2 * v_N_5 * get_local_id(1)) + (16 * v_N_5 * v_wg_id_40) + (32 * v_wg_id_41) + get_local_id(0))) = idfloat4((float4)(v__74_0, v__74_1, v__74_2, v__74_3));
      /* end unroll */
      /* end map_seq */
      /* map_seq */
      /* unroll */
      *( ((global float4*)v__93) + ((v_N_5 / 4) + (16 * v_N_5 * v_wg_id_40) + (2 * v_N_5 * get_local_id(1)) + (32 * v_wg_id_41) + get_local_id(0))) = idfloat4((float4)(v__74_4, v__74_5, v__74_6, v__74_7));
      /* end unroll */
      /* end map_seq */
      /* map_seq */
      /* unroll */
      *( ((global float4*)v__93) + ((v_N_5 / 2) + (2 * v_N_5 * get_local_id(1)) + (16 * v_N_5 * v_wg_id_40) + (32 * v_wg_id_41) + get_local_id(0))) = idfloat4((float4)(v__74_8, v__74_9, v__74_10, v__74_11));
      /* end unroll */
      /* end map_seq */
      /* map_seq */
      /* unroll */
      *( ((global float4*)v__93) + (((3 * v_N_5) / 4) + (16 * v_N_5 * v_wg_id_40) + (2 * v_N_5 * get_local_id(1)) + (32 * v_wg_id_41) + get_local_id(0))) = idfloat4((float4)(v__74_12, v__74_13, v__74_14, v__74_15));
      /* end unroll */
      /* end map_seq */
      /* map_seq */
      /* unroll */
      *( ((global float4*)v__93) + (v_N_5 + (16 * v_N_5 * v_wg_id_40) + (2 * v_N_5 * get_local_id(1)) + (32 * v_wg_id_41) + get_local_id(0))) = idfloat4((float4)(v__74_16, v__74_17, v__74_18, v__74_19));
      /* end unroll */
      /* end map_seq */
      /* map_seq */
      /* unroll */
      *( ((global float4*)v__93) + (((5 * v_N_5) / 4) + (16 * v_N_5 * v_wg_id_40) + (2 * v_N_5 * get_local_id(1)) + (32 * v_wg_id_41) + get_local_id(0))) = idfloat4((float4)(v__74_20, v__74_21, v__74_22, v__74_23));
      /* end unroll */
      /* end map_seq */
      /* map_seq */
      /* unroll */
      *( ((global float4*)v__93) + (((3 * v_N_5) / 2) + (2 * v_N_5 * get_local_id(1)) + (16 * v_N_5 * v_wg_id_40) + (32 * v_wg_id_41) + get_local_id(0))) = idfloat4((float4)(v__74_24, v__74_25, v__74_26, v__74_27));
      /* end unroll */
      /* end map_seq */
      /* map_seq */
      /* unroll */
      *( ((global float4*)v__93) + (((7 * v_N_5) / 4) + (16 * v_N_5 * v_wg_id_40) + (2 * v_N_5 * get_local_id(1)) + (32 * v_wg_id_41) + get_local_id(0))) = idfloat4((float4)(v__74_28, v__74_29, v__74_30, v__74_31));
      /* end unroll */
      /* end map_seq */
      /* end unroll */
      /* end map_seq */
      /* end unroll */
      /* end unroll */
      /* end unroll */
      /* end map_seq */
    }
  }
}
