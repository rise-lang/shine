#ifndef Tuple2_float_float_DEFINED
#define Tuple2_float_float_DEFINED
typedef struct __attribute__((aligned(4))){
    float _0;
    float _1;
} Tuple2_float_float;
#endif

float distance_(Tuple2_float_float loc, float lat, float lng){
    {
        typedef Tuple2_float_float Tuple;
        { return sqrt( (lat - loc._0) * (lat - loc._0) + (lng - loc._1) * (lng - loc._1) ); }; 
    }
}
kernel void KERNEL(const global Tuple2_float_float* restrict v__4, float v__5, float v__6, global float* v__8, int v_N_0){
    #ifndef WORKGROUP_GUARD
    #define WORKGROUP_GUARD
    #endif
    WORKGROUP_GUARD
    {
        // Static local memory
        // Typed Value memory
        // Private Memory
        // iteration count is exactly 1, no loop emitted
        {
            int v_gl_id_3 = get_global_id(0); 
            v__8[v_gl_id_3] = distance_(v__4[v_gl_id_3], v__5, v__6); 
        }
    }
}

