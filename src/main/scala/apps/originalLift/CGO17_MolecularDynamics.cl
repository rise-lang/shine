float4 id(float4 x){
  { return x; }
}
float4 updateF(float4 f, float4 ipos, float4 jpos, float cutsq, float lj1, float lj2){
  {
  // Calculate distance
  float delx = ipos.x - jpos.x;
  float dely = ipos.y - jpos.y;
  float delz = ipos.z - jpos.z;
  float r2inv = delx*delx + dely*dely + delz*delz;
  // If distance is less than cutoff, calculate force
  if (r2inv < cutsq) {
    r2inv = 1.0f/r2inv;
    float r6inv = r2inv * r2inv * r2inv;
    float forceC = r2inv*r6inv*(lj1*r6inv - lj2);
    f.x += delx * forceC;
    f.y += dely * forceC;
    f.z += delz * forceC;
  }
  return f;
}

}
float4 id4(float4 x){
  { return x; }
}
kernel void KERNEL(const global float* restrict v__16, const global int* restrict v__17, float v__18, float v__19, float v__20, global float* v__29, int v_M_1, int v_N_0){
  /* Static local memory */
  /* Typed Value memory */
  float4 v__24;
  /* Private Memory */
  float4 v__23_0;

  /* iteration count is exactly 1, no loop emitted */
  int v_wg_id_12 = get_group_id(0);
  {
    /* iteration count is exactly 1, no loop emitted */
    {
      int v_l_id_13 = get_local_id(0);
      v__23_0 = id(vload4((v_l_id_13 + (128 * v_wg_id_12)),v__16));
      float4 v_tmp_42 = 0.0f;
      v__24 = v_tmp_42;
      /* reduce_seq */
      for (int v_i_14 = 0;v_i_14<v_M_1;v_i_14 = (1 + v_i_14)){
        v__24 = updateF(v__24, v__23_0, vload4(v__17[(v_l_id_13 + (128 * v_wg_id_12) + (v_N_0 * v_i_14))],v__16), v__18, v__19, v__20);
      }
      /* end reduce_seq */
      /* map_seq */
      /* iteration count is exactly 1, no loop emitted */
      {
        int v_i_15 = 0;
        vstore4(id4(v__24),(v_l_id_13 + (128 * v_wg_id_12)),v__29);;
      }
      /* end map_seq */
    }
  }
}

