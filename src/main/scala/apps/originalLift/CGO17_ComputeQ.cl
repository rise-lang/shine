#ifndef Tuple_float_float_DEFINED
#define Tuple_float_float_DEFINED
typedef struct {
  float _0;
  float _1;
} Tuple_float_float;
#endif
#ifndef Tuple_float_float_float_float_DEFINED
#define Tuple_float_float_float_float_DEFINED
typedef struct {
  float _0;
  float _1;
  float _2;
  float _3;
} Tuple_float_float_float_float;
#endif
#ifndef Tuple_float_float_float_float_float_DEFINED
#define Tuple_float_float_float_float_float_DEFINED
typedef struct {
  float _0;
  float _1;
  float _2;
  float _3;
  float _4;
} Tuple_float_float_float_float_float;
#endif

Tuple_float_float id(Tuple_float_float x){
  typedef Tuple_float_float Tuple;
  { return x; }}

Tuple_float_float pair(float x, float y){
  typedef Tuple_float_float Tuple;
  { Tuple t = {x, y}; return t; }}

Tuple_float_float computeQ(float sX, float sY, float sZ, float Kx, float Ky, float Kz, float PhiMag, Tuple_float_float acc){
  typedef Tuple_float_float Tuple;
  {
    #define PIx2 6.2831853071795864769252867665590058f
    float expArg = PIx2 * (Kx * sX + Ky * sY + Kz * sZ);
    acc._0 = acc._0 + PhiMag * cos(expArg);
    acc._1 = acc._1 + PhiMag * sin(expArg);

    return acc;
}}

kernel void KERNEL(int v_K_0, int bla, const global float* restrict v__20, const global float* restrict v__21, const global float* restrict v__22, const global float* restrict v__23, const global float* restrict v__24, const global Tuple_float_float_float_float* restrict v__25, global Tuple_float_float* v__36, int v_X_3){
  /* Static local memory */
  /* Typed Value memory */
  float v__29;
  float v__28;
  float v__27;
  /* Private Memory */
  Tuple_float_float v__32_0;
  {
    int v_gl_id_6 = get_global_id(0);
    {
      /* let: */
      v__27 = v__20[v_gl_id_6];
      /* let: */
      v__28 = v__21[v_gl_id_6];
      /* let: */
      v__29 = v__22[v_gl_id_6];
      v__32_0 = pair(v__23[v_gl_id_6], v__24[v_gl_id_6]);
      {
        /* reduce_seq */
        for (int v_i_4 = 0; v_i_4 < v_K_0; v_i_4 += 1) {
          {
            v__32_0 = computeQ(v__27, v__28, v__29, v__25[v_i_4]._0, v__25[v_i_4]._1, v__25[v_i_4]._2, v__25[v_i_4]._3, v__32_0);
          }
        }
        /* end reduce_seq */
      }
      /* map_seq */
      /* unroll */
      v__36[v_gl_id_6] = id(v__32_0);
      /* end unroll */
      /* end map_seq */
    }
  }
}
