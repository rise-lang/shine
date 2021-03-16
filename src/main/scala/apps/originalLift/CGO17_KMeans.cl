#ifndef Tuple_float_float_DEFINED
#define Tuple_float_float_DEFINED
typedef struct {
  float _0;
  float _1;
} Tuple_float_float;
#endif

#ifndef Tuple_float_int_int_DEFINED
#define Tuple_float_int_int_DEFINED
typedef struct {
  float _0;
  int _1;
  int _2;
} Tuple_float_int_int;
#endif

#ifndef Tuple_float_int_int_DEFINED
#define Tuple_float_int_int_DEFINED
typedef struct {
  float _0;
  int _1;
  int _2;
} Tuple_float_int_int;
#endif
#ifndef Tuple_float_Tuple_float_int_int_DEFINED
#define Tuple_float_Tuple_float_int_int_DEFINED
typedef struct {
  float _0;
  Tuple_float_int_int _1;
} Tuple_float_Tuple_float_int_int;
#endif

float update(float dist, Tuple_float_float pair){
  typedef Tuple_float_float Tuple;

  { return dist + (pair._0 - pair._1) * (pair._0 - pair._1); }
}
Tuple_float_int_int test(float dist, Tuple_float_int_int tuple){
  typedef Tuple_float_int_int Tuple;

  {float min_dist = tuple._0;int i          = tuple._1;int index      = tuple._2;if (dist < min_dist) {  Tuple t = {dist, i + 1, i};  return t;} else {  Tuple t = {min_dist, i + 1, index};  return t;}}
}
int select_(Tuple_float_int_int tuple){
  typedef Tuple_float_int_int Tuple;

  { return tuple._2; }
}
kernel void KERNEL(const global float* restrict v__19, const global float* restrict v__20, global int* v__32, int v_C_1, int v_F_2, int v_P_0){
  /* Static local memory */
  /* Typed Value memory */
  Tuple_float_int_int v__22;
  float v__24;
  /* Private Memory */
  /* iteration count is exactly 1, no loop emitted */
  {
    int v_gl_id_12 = get_global_id(0);
    Tuple_float_int_int v_tmp_51 = {3.40282347e+38, 0, 0};
    v__22 = v_tmp_51;
    /* reduce_seq */
    for (int v_i_13 = 0;v_i_13<v_C_1;v_i_13 = (1 + v_i_13)){
      float v_tmp_52 = 0.0f;
      v__24 = v_tmp_52;
      /* reduce_seq */
      for (int v_i_15 = 0;v_i_15<v_F_2;v_i_15 = (1 + v_i_15)){
        v__24 = update(v__24, (Tuple_float_float){v__19[(v_gl_id_12 + (v_P_0 * v_i_15))], v__20[(v_i_15 + (v_F_2 * v_i_13))]});
      }
      /* end reduce_seq */
      /* map_seq */
      /* unroll */
      v__22 = test(v__24, v__22);
      /* end unroll */
      /* end map_seq */
    }
    /* end reduce_seq */
    /* map_seq */
    /* iteration count is exactly 1, no loop emitted */
    {
      int v_i_17 = 0;
      /* map_seq */
      /* iteration count is exactly 1, no loop emitted */
      {
        int v_i_18 = 0;
        v__32[v_gl_id_12] = select_(v__22);
      }
      /* end map_seq */
    }
    /* end map_seq */
  }
}

