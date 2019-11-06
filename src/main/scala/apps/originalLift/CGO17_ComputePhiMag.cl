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

float phiMag(float phiR, float phiI){
  { return phiR * phiR + phiI * phiI; }}

kernel void KERNEL(const global float* restrict v__7, const global float* restrict v__8, global float* v__12, int v_K_0){
  /* Static local memory */
  /* Typed Value memory */
  /* Private Memory */
  {
    int v_gl_id_2 = get_global_id(0);
    {
      v__12[v_gl_id_2] = phiMag(v__7[v_gl_id_2], v__8[v_gl_id_2]);
    }
  }
}


