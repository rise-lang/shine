#ifndef Tuple2_float_float_DEFINED
#define Tuple2_float_float_DEFINED
typedef struct __attribute__((aligned(4))){
    float _0;
    float _1;
} Tuple2_float_float;
#endif

#ifndef Tuple3_float_float_int_DEFINED
#define Tuple3_float_float_int_DEFINED
typedef struct __attribute__((aligned(4))){
    float _0;
    float _1;
    int _2;
} Tuple3_float_float_int;
#endif

float idIF(int x){
    {
        { return (float)(x*1.0); }; 
    }
}
float mult(float l, float r){
    {
        { return l * r; }; 
    }
}
float subtract(float l, float r){
    {
        { return l - r; }; 
    }
}
float addTuple(Tuple2_float_float x){
    {
        typedef Tuple2_float_float Tuple;
        {return x._0 + x._1;}; 
    }
}
float getCF(int neigh, float cfB, float cfI){
    {
        { if(neigh < 6) { return cfB; } else{ return cfI;} }; 
    }
}
float id(float x){
    {
        { return x; }; 
    }
}
float subtractTuple(Tuple2_float_float x){
    {
        typedef Tuple2_float_float Tuple;
        {return x._0 - x._1;}; 
    }
}
int idxF(int i, int j, int k, int m, int n, int o){
    {
        {int count = 6; if(i == (m-1) || i == 0){ count--; } if(j == (n-1) || j == 0){ count--; } if(k == (o-1) || k == 0){ count--; }return count; }; 
    }
}
float add(float x, float y){
    {
        { return x+y; }; 
    }
}
kernel void KERNEL(const global float* restrict v__50, const global float* restrict v__51, global float* v__94, int v_M_0, int v_N_1, int v_O_2){
        // Static local memory
        // Typed Value memory
        float v__57; 
        float v__60; 
        float v__75; 
        float v__78; 
        float v__79; 
        float v__88; 
        float v__89; 
        // Private Memory
        float v__56; 
        float v__59; 
        float v__62; 
        float v__64; 
        float v__66; 
        float v__68; 
        float v__70; 
        float v__72; 
        float v__74; 
        float v__77; 
        float v__81; 
        float v__83; 
        float v__85; 
        float v__87; 
        float v__91; 
        float v__93; 
        for (int v_gl_id_47 = get_global_id(2); (v_gl_id_47 < v_O_2); v_gl_id_47 = (v_gl_id_47 + get_global_size(2))){
            for (int v_gl_id_48 = get_global_id(1); (v_gl_id_48 < v_N_1); v_gl_id_48 = (v_gl_id_48 + get_global_size(1))){
                for (int v_gl_id_49 = get_global_id(0); (v_gl_id_49 < v_M_0); v_gl_id_49 = (v_gl_id_49 + get_global_size(0))){
                    v__56 = idIF(idxF((1 + v_gl_id_47), (1 + v_gl_id_48), (1 + v_gl_id_49), (2 + v_O_2), (2 + v_N_1), (2 + v_M_0))); 
                    float v_tmp_208 = 0.3333333f; 
                    v__57 = v_tmp_208; 
                    v__59 = mult(v__56, v__57); 
                    float v_tmp_209 = 2.0f; 
                    v__60 = v_tmp_209; 
                    v__62 = subtract(v__60, v__59); 
                    v__64 = mult(v__62, v__51[(7 + v_gl_id_49 + (2 * v_M_0 * v_gl_id_47) + (2 * v_N_1) + (2 * v_N_1 * v_gl_id_47) + (2 * v_gl_id_48) + (3 * v_M_0) + (v_M_0 * v_N_1) + (v_M_0 * v_N_1 * v_gl_id_47) + (4 * v_gl_id_47) + (v_M_0 * v_gl_id_48))]); 
                    v__66 = add(v__51[(11 + v_gl_id_49 + (2 * v_M_0 * v_N_1) + (2 * v_M_0 * v_gl_id_47) + (2 * v_gl_id_48) + (5 * v_M_0) + (2 * v_N_1 * v_gl_id_47) + (4 * v_N_1) + (v_M_0 * v_N_1 * v_gl_id_47) + (4 * v_gl_id_47) + (v_M_0 * v_gl_id_48))], v__51[(9 + v_gl_id_49 + (2 * v_M_0 * v_gl_id_47) + (2 * v_N_1) + (2 * v_N_1 * v_gl_id_47) + (2 * v_gl_id_48) + (4 * v_M_0) + (v_M_0 * v_N_1) + (v_M_0 * v_N_1 * v_gl_id_47) + (4 * v_gl_id_47) + (v_M_0 * v_gl_id_48))]); 
                    v__68 = add(v__66, v__51[(8 + v_gl_id_49 + (2 * v_M_0 * v_gl_id_47) + (2 * v_N_1) + (2 * v_N_1 * v_gl_id_47) + (2 * v_gl_id_48) + (3 * v_M_0) + (v_M_0 * v_N_1) + (v_M_0 * v_N_1 * v_gl_id_47) + (4 * v_gl_id_47) + (v_M_0 * v_gl_id_48))]); 
                    v__70 = add(v__68, v__51[(6 + v_gl_id_49 + (2 * v_M_0 * v_gl_id_47) + (3 * v_M_0) + (2 * v_N_1 * v_gl_id_47) + (2 * v_N_1) + (v_M_0 * v_N_1) + (v_M_0 * v_N_1 * v_gl_id_47) + (2 * v_gl_id_48) + (4 * v_gl_id_47) + (v_M_0 * v_gl_id_48))]); 
                    v__72 = add(v__70, v__51[(5 + v_gl_id_49 + (2 * v_M_0) + (2 * v_M_0 * v_gl_id_47) + (2 * v_N_1) + (2 * v_N_1 * v_gl_id_47) + (4 * v_gl_id_47) + (v_M_0 * v_N_1) + (v_M_0 * v_N_1 * v_gl_id_47) + (2 * v_gl_id_48) + (v_M_0 * v_gl_id_48))]); 
                    v__74 = add(v__72, v__51[(3 + v_M_0 + v_gl_id_49 + (2 * v_M_0 * v_gl_id_47) + (2 * v_N_1 * v_gl_id_47) + (2 * v_gl_id_48) + (v_M_0 * v_N_1 * v_gl_id_47) + (4 * v_gl_id_47) + (v_M_0 * v_gl_id_48))]); 
                    float v_tmp_210 = 0.3333333f; 
                    v__75 = v_tmp_210; 
                    v__77 = mult(v__74, v__75); 
                    float v_tmp_211 = 0.9971132f; 
                    v__78 = v_tmp_211; 
                    float v_tmp_212 = 1.0f; 
                    v__79 = v_tmp_212; 
                    v__81 = getCF(idxF((1 + v_gl_id_47), (1 + v_gl_id_48), (1 + v_gl_id_49), (2 + v_O_2), (2 + v_N_1), (2 + v_M_0)), v__78, v__79); 
                    v__83 = mult(v__50[(7 + v_gl_id_49 + (2 * v_M_0 * v_gl_id_47) + (2 * v_N_1) + (2 * v_N_1 * v_gl_id_47) + (2 * v_gl_id_48) + (3 * v_M_0) + (v_M_0 * v_N_1) + (v_M_0 * v_N_1 * v_gl_id_47) + (4 * v_gl_id_47) + (v_M_0 * v_gl_id_48))], v__81); 
                    v__85 = subtractTuple((Tuple2_float_float){v__77, v__83}); 
                    v__87 = addTuple((Tuple2_float_float){v__64, v__85}); 
                    float v_tmp_213 = 0.9971216f; 
                    v__88 = v_tmp_213; 
                    float v_tmp_214 = 1.0f; 
                    v__89 = v_tmp_214; 
                    v__91 = getCF(idxF((1 + v_gl_id_47), (1 + v_gl_id_48), (1 + v_gl_id_49), (2 + v_O_2), (2 + v_N_1), (2 + v_M_0)), v__88, v__89); 
                    v__93 = mult(v__87, v__91); 
                    v__94[(v_gl_id_49 + (v_M_0 * v_N_1 * v_gl_id_47) + (v_M_0 * v_gl_id_48))] = id(v__93); 
                }
            }
        }
}
