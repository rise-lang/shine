#ifndef Tuple2_float_float_DEFINED
#define Tuple2_float_float_DEFINED
typedef struct __attribute__((aligned(4))){
    float _0;
    float _1;
} Tuple2_float_float;
#endif

#ifndef Tuple3_float_int_int_DEFINED
#define Tuple3_float_int_int_DEFINED
typedef struct __attribute__((aligned(4))){
    float _0;
    int _1;
    int _2;
} Tuple3_float_int_int;
#endif

#ifndef Tuple3_float_int_int_DEFINED
#define Tuple3_float_int_int_DEFINED
typedef struct __attribute__((aligned(4))){
    float _0;
    int _1;
    int _2;
} Tuple3_float_int_int;
#endif
#ifndef Tuple2_float_Tuple3_float_int_int_DEFINED
#define Tuple2_float_Tuple3_float_int_int_DEFINED
typedef struct __attribute__((aligned(4))){
    float _0;
    Tuple3_float_int_int _1;
} Tuple2_float_Tuple3_float_int_int;
#endif

float update(float dist, Tuple2_float_float pair){
    {
        typedef Tuple2_float_float Tuple;
        { return dist + (pair._0 - pair._1) * (pair._0 - pair._1); }; 
    }
}
Tuple3_float_int_int test(float dist, Tuple3_float_int_int tuple){
    {
        typedef Tuple3_float_int_int Tuple;
        {float min_dist = tuple._0;int i          = tuple._1;int index      = tuple._2;if (dist < min_dist) {  Tuple t = {dist, i + 1, i};  return t;} else {  Tuple t = {min_dist, i + 1, index};  return t;}}; 
    }
}
int select_(Tuple3_float_int_int tuple){
    {
        typedef Tuple3_float_int_int Tuple;
        { return tuple._2; }; 
    }
}
kernel void KERNEL(const global float* restrict ps, const global float* restrict cs, global int* output, int C, int F, int P){
        // Static local memory
        // Typed Value memory
        Tuple3_float_int_int acc1;
        float acc2;
        // Private Memory
        for (int gid0 = get_global_id(0); (gid0 < P); gid0 = (gid0 + get_global_size(0))){
            Tuple3_float_int_int v_tmp_45 = {3.40282347e+38, 0, 0};
            acc1 = v_tmp_45;
            // reduce_seq
            for (int i = 0; (i < C); i = (1 + i)){
                float v_tmp_46 = 0.0f;
                acc2 = v_tmp_46;
                // reduce_seq
                for (int j = 0; (j < F); j = (1 + j)){
                    acc2 = update(acc2, (Tuple2_float_float){ps[(gid0 + (P * j))], cs[(j + (F * i))]});
                }
                // end reduce_seq
                // map_seq
                // unroll
                acc1 = test(acc2, acc1);
                // end unroll
                // end map_seq
            }
            // end reduce_seq
            // map_seq
            // iteration count is exactly 1, no loop emitted
            {
                int v_i_15 = 0;
                // map_seq
                // iteration count is exactly 1, no loop emitted
                {
                    int v_i_16 = 0;
                    output[gid0] = select_(acc1);
                }
                // end map_seq
            }
            // end map_seq
        }
}

