#ifndef Tuple_float_float_DEFINED
#define Tuple_float_float_DEFINED
typedef struct {
  float _0;
  float _1;
} Tuple_float_float;
#endif

float distance_(Tuple_float_float loc, float lat, float lng){
  typedef Tuple_float_float Tuple;

  { return sqrt( (lat - loc._0) * (lat - loc._0) + (lng - loc._1) * (lng - loc._1) ); }
}
kernel void KERNEL(const global Tuple_float_float* restrict v__7, float v__8, float v__9, global float* v__12, int v_N_0){
  /* Static local memory */
  /* Typed Value memory */
  /* Private Memory */
  /* iteration count is exactly 1, no loop emitted */
  {
    int v_gl_id_6 = get_global_id(0);
    v__12[v_gl_id_6] = distance_(v__7[v_gl_id_6], v__8, v__9);
  }
}
