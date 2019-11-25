//This code was generated for the following work-item sizes and input dimensions:
//localSize = 2,2,1
//globalSize = 256,128,1
//
//M = 512
//N = 256
//K = 64
#ifndef Tuple2_float4_float4_DEFINED
#define Tuple2_float4_float4_DEFINED
typedef struct __attribute__((aligned(16))){
    float4 _0;
    float4 _1;
} Tuple2_float4_float4;
#endif

// NOTE: trying to print unprintable type: float
 // NOTE: trying to print unprintable type: float
#ifndef Tuple2_float_float_DEFINED
#define Tuple2_float_float_DEFINED
typedef struct __attribute__((aligned(4))){
    float _0;
    float _1;
} Tuple2_float_float;
#endif

float idfloat(float x){
    {
        { return x; };
    }
}
float add(float x, float y){
    {
        { return x+y; };
    }
}
float mult(float l, float r){
    {
        { return l * r; };
    }
}
kernel void KERNEL(const global float* restrict v__31, const global float* restrict v__32, const global float* restrict v__33, float v__34, float v__35, global float* v__57, global float* v__53, global float* v__55, int v_K_2, int v_M_1, int v_N_0){
    // Static local memory
    // Typed Value memory
    float v__38;
    // Private Memory
    float v_v__39_1_305;
    float v_v__39_2_306;
    float v_v__39_3_307;
    float v_v__39_4_308;
    float v_v__46_1_309;
    float v_v__46_2_310;
    float v_v__46_3_311;
    float v_v__46_4_312;
    // iteration count is exactly 1, no loop emitted
    {
        int v_gl_id_18 = get_global_id(0);
        // iteration count is exactly 1, no loop emitted
        {
            int v_gl_id_19 = get_global_id(1);
            float v_tmp_271 = 0.0f;
            v__38 = v_tmp_271;
            // map_seq
            // unroll
            // map_seq
            // unroll
            v_v__39_1_305 = idfloat(v__38);
            v_v__39_2_306 = idfloat(v__38);
            // end unroll
            // end map_seq
            // map_seq
            // unroll
            v_v__39_3_307 = idfloat(v__38);
            v_v__39_4_308 = idfloat(v__38);
            // end unroll
            // end map_seq
            // end unroll
            // end map_seq
            // reduce_seq
            for (int v_i_22 = 0; (v_i_22 < ((v_K_2)/(4))); v_i_22 = (1 + v_i_22)){
                // map_seq
                // unroll
                // map_seq
                // unroll
                // map_seq
                // unroll
                v_v__46_1_309 = dot(vload4((v_i_22 + ((v_K_2 * v_gl_id_18) / 2)),v__31 + ((2 * v_K_2 * v_gl_id_18) % 4)), vload4((v_i_22 + ((v_K_2 * v_gl_id_19) / 2)),v__32 + ((2 * v_K_2 * v_gl_id_19) % 4)));
                // end unroll
                // end map_seq
                // reduce_seq
                // unroll
                v_v__39_1_305 = add(v_v__39_1_305, v_v__46_1_309);
                // end unroll
                // end reduce_seq
                // map_seq
                // unroll
                // end unroll
                // end map_seq
                // map_seq
                // unroll
                v_v__46_2_310 = dot(vload4((v_i_22 + ((v_K_2 * v_gl_id_18) / 2)),v__31 + ((2 * v_K_2 * v_gl_id_18) % 4)), vload4((v_i_22 + ((v_K_2 + (2 * v_K_2 * v_gl_id_19)) / 4)),v__32 + ((v_K_2 + (2 * v_K_2 * v_gl_id_19)) % 4)));
                // end unroll
                // end map_seq
                // reduce_seq
                // unroll
                v_v__39_2_306 = add(v_v__39_2_306, v_v__46_2_310);
                // end unroll
                // end reduce_seq
                // map_seq
                // unroll
                // end unroll
                // end map_seq
                // end unroll
                // end map_seq
                // map_seq
                // unroll
                // map_seq
                // unroll
                v_v__46_3_311 = dot(vload4((v_i_22 + ((v_K_2 + (2 * v_K_2 * v_gl_id_18)) / 4)),v__31 + ((v_K_2 + (2 * v_K_2 * v_gl_id_18)) % 4)), vload4((v_i_22 + ((v_K_2 * v_gl_id_19) / 2)),v__32 + ((2 * v_K_2 * v_gl_id_19) % 4)));
                // end unroll
                // end map_seq
                // reduce_seq
                // unroll
                v_v__39_3_307 = add(v_v__39_3_307, v_v__46_3_311);
                // end unroll
                // end reduce_seq
                // map_seq
                // unroll
                // end unroll
                // end map_seq
                // map_seq
                // unroll
                v_v__46_4_312 = dot(vload4((v_i_22 + ((v_K_2 + (2 * v_K_2 * v_gl_id_18)) / 4)),v__31 + ((v_K_2 + (2 * v_K_2 * v_gl_id_18)) % 4)), vload4((v_i_22 + ((v_K_2 + (2 * v_K_2 * v_gl_id_19)) / 4)),v__32 + ((v_K_2 + (2 * v_K_2 * v_gl_id_19)) % 4)));
                // end unroll
                // end map_seq
                // reduce_seq
                // unroll
                v_v__39_4_308 = add(v_v__39_4_308, v_v__46_4_312);
                // end unroll
                // end reduce_seq
                // map_seq
                // unroll
                // end unroll
                // end map_seq
                // end unroll
                // end map_seq
                // end unroll
                // end map_seq
            }
            // end reduce_seq
            // map_seq
            // unroll
            // map_seq
            // unroll
            // map_seq
            // unroll
            v__53[((2 * v_N_0 * v_gl_id_18) + (4 * v_gl_id_19))] = mult(v_v__39_1_305, v__34);
            v__55[((2 * v_N_0 * v_gl_id_18) + (4 * v_gl_id_19))] = mult(v__33[((2 * v_N_0 * v_gl_id_18) + (2 * v_gl_id_19))], v__35);
            v__57[((2 * v_N_0 * v_gl_id_18) + (2 * v_gl_id_19))] = add(v__53[((2 * v_N_0 * v_gl_id_18) + (4 * v_gl_id_19))], v__55[((2 * v_N_0 * v_gl_id_18) + (4 * v_gl_id_19))]);
            v__53[(1 + (2 * v_N_0 * v_gl_id_18) + (4 * v_gl_id_19))] = mult(v_v__39_2_306, v__34);
            v__55[(1 + (2 * v_N_0 * v_gl_id_18) + (4 * v_gl_id_19))] = mult(v__33[(1 + (2 * v_N_0 * v_gl_id_18) + (2 * v_gl_id_19))], v__35);
            v__57[(1 + (2 * v_N_0 * v_gl_id_18) + (2 * v_gl_id_19))] = add(v__53[(1 + (2 * v_N_0 * v_gl_id_18) + (4 * v_gl_id_19))], v__55[(1 + (2 * v_N_0 * v_gl_id_18) + (4 * v_gl_id_19))]);
            // end unroll
            // end map_seq
            // map_seq
            // unroll
            v__53[(2 + (2 * v_N_0 * v_gl_id_18) + (4 * v_gl_id_19))] = mult(v_v__39_3_307, v__34);
            v__55[(2 + (2 * v_N_0 * v_gl_id_18) + (4 * v_gl_id_19))] = mult(v__33[(v_N_0 + (2 * v_N_0 * v_gl_id_18) + (2 * v_gl_id_19))], v__35);
            v__57[(v_N_0 + (2 * v_N_0 * v_gl_id_18) + (2 * v_gl_id_19))] = add(v__53[(2 + (2 * v_N_0 * v_gl_id_18) + (4 * v_gl_id_19))], v__55[(2 + (2 * v_N_0 * v_gl_id_18) + (4 * v_gl_id_19))]);
            v__53[(3 + (2 * v_N_0 * v_gl_id_18) + (4 * v_gl_id_19))] = mult(v_v__39_4_308, v__34);
            v__55[(3 + (2 * v_N_0 * v_gl_id_18) + (4 * v_gl_id_19))] = mult(v__33[(1 + v_N_0 + (2 * v_N_0 * v_gl_id_18) + (2 * v_gl_id_19))], v__35);
            v__57[(1 + v_N_0 + (2 * v_N_0 * v_gl_id_18) + (2 * v_gl_id_19))] = add(v__53[(3 + (2 * v_N_0 * v_gl_id_18) + (4 * v_gl_id_19))], v__55[(3 + (2 * v_N_0 * v_gl_id_18) + (4 * v_gl_id_19))]);
            // end unroll
            // end map_seq
            // end unroll
            // end map_seq
            // end unroll
            // end map_seq
        }
    }
}