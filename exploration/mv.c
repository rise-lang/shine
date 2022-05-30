#include <stdio.h>
#include <stdlib.h>


const char k0_source[] =
""
""
""
"__kernel __attribute__ ((reqd_work_group_size(32, 4, 1)))"
"void k0(global float* restrict output, int n160, int n161, const global float* restrict e162, const global float* restrict e163){"
"  /* Start of moved local vars */"
"  /* End of moved local vars */"
"  /* mapGlobal */"
"  for (int gl_id_3674801 = get_global_id(1); gl_id_3674801 < n161; gl_id_3674801 = 32 + gl_id_3674801) {"
"    /* mapLocal */"
"    for (int l_id_3674802 = get_local_id(1); l_id_3674802 < 1; l_id_3674802 = 4 + l_id_3674802) {"
"      /* mapGlobal */"
"      for (int gl_id_3674803 = get_global_id(0); gl_id_3674803 < 1; gl_id_3674803 = 512 + gl_id_3674803) {"
"        /* mapWorkGroup */"
"        for (int wg_id_3674804 = get_group_id(0); wg_id_3674804 < 1; wg_id_3674804 = 16 + wg_id_3674804) {"
"          /* oclReduceSeq */"
"          {"
"            float x3674772;"
"            x3674772 = 0.0f;"
"            for (int i_3674805 = 0; i_3674805 < n160; i_3674805 = 1 + i_3674805) {"
"              x3674772 = x3674772 + (e162[(((i_3674805 + (gl_id_3674801 * n160)) + (gl_id_3674803 * n160)) + (l_id_3674802 * n160)) + (n160 * wg_id_3674804)] * e163[i_3674805]);"
"            }"
"            "
"            output[((gl_id_3674801 + gl_id_3674803) + l_id_3674802) + wg_id_3674804] = x3674772;"
"          }"
"          "
"        }"
"        "
"      }"
"      "
"    }"
"    "
"  }"
"  "
"}"
"";

#define loadKernel(ctx, id)\
  loadKernelFromSource(ctx, #id, id##_source, sizeof(id##_source) - 1)

#include "ocl/ocl.h"
struct fun_t {
  Kernel k0;
};

typedef struct fun_t fun_t;

void fun_init(Context ctx, fun_t* self){
  (*self).k0 = loadKernel(ctx, k0);
}

void fun_destroy(Context ctx, fun_t* self){
  destroyKernel(ctx, (*self).k0);
}

void fun_run(Context ctx, fun_t* self, Buffer moutput, int n160, int n161, Buffer me162, Buffer me163){
  {
    DeviceBuffer b0 = deviceBufferSync(ctx, moutput, n161 * sizeof(float), DEVICE_WRITE);
    DeviceBuffer b3 = deviceBufferSync(ctx, me162, n161 * (n160 * sizeof(float)), DEVICE_READ);
    DeviceBuffer b4 = deviceBufferSync(ctx, me163, n160 * sizeof(float), DEVICE_READ);
    const size_t global_size[3] = (const size_t[3]){512, 32, 1};
    const size_t local_size[3] = (const size_t[3]){32, 4, 1};
    const KernelArg args[5] = (const KernelArg[5]){KARG(b0), KARG(n160), KARG(n161), KARG(b3), KARG(b4)};
    launchKernel(ctx, (*self).k0, global_size, local_size, 5, args);
  }

}

void fun_init_run(Context ctx, Buffer moutput, int n160, int n161, Buffer me162, Buffer me163){
  fun_t fun;
  fun_init(ctx, &fun);
  fun_run(ctx, &fun, moutput, n160, n161, me162, me163);
  fun_destroy(ctx, &fun);
}




int main(int argc, char** argv) {
  Context ctx = createDefaultContext();
  fun_t fun;
  fun_init(ctx, &fun);

  FILE* file;
  Buffer inBuff0 = createBuffer(ctx, 1048576 * sizeof(float), HOST_WRITE | DEVICE_READ);
  float* in0 = hostBufferSync(ctx, inBuff0, 1048576 * sizeof(float), HOST_WRITE);
  file = fopen("/tmp/mvInput16683767421335608290", "r");
  for (int i = 0; i < 1048576; i++) {
    fscanf(file, "%f", &in0[i]);
  }
  fclose(file);


  Buffer inBuff1 = createBuffer(ctx, 1024 * sizeof(float), HOST_WRITE | DEVICE_READ);
  float* in1 = hostBufferSync(ctx, inBuff1, 1024 * sizeof(float), HOST_WRITE);
  file = fopen("/tmp/mvInput17647910111591046576", "r");
  for (int i = 0; i < 1024; i++) {
    fscanf(file, "%f", &in1[i]);
  }
  fclose(file);


 Buffer outBuff = createBuffer(ctx, 1024 * sizeof(float), HOST_READ | DEVICE_WRITE);
 float* out = hostBufferSync(ctx, outBuff, 1024 * sizeof(float), HOST_READ);


  int iterations = atoi(argv[1]);
  for (int sample = 0; sample < iterations; sample++) {
      fun_run(ctx, &fun, outBuff, 1024, 1024, inBuff0, inBuff1);
  }

  file = fopen("/tmp/mvInput10650451907416943904", "r");
  float x;
  int diffs = 0;
  for (int i = 0; i < 1024; i++) {
    if(out[i] != fscanf(file, "%f", &x)){
      diffs++;
    }
  }

 destroyBuffer(ctx, inBuff0);
 destroyBuffer(ctx, inBuff1);

  destroyBuffer(ctx, outBuff);

  if(diffs > 0){ //Not OK
    exit(42);
  }

  fun_destroy(ctx, &fun);
  destroyContext(ctx);
  return EXIT_SUCCESS;
}#include <stdio.h>
 #include <stdlib.h>


 const char k0_source[] =
 ""
 ""
 ""
 "__kernel __attribute__ ((reqd_work_group_size(32, 4, 1)))"
 "void k0(global float* restrict output, int n160, int n161, const global float* restrict e162, const global float* restrict e163){"
 "  /* Start of moved local vars */"
 "  /* End of moved local vars */"
 "  /* mapGlobal */"
 "  for (int gl_id_3674801 = get_global_id(1); gl_id_3674801 < n161; gl_id_3674801 = 32 + gl_id_3674801) {"
 "    /* mapLocal */"
 "    for (int l_id_3674802 = get_local_id(1); l_id_3674802 < 1; l_id_3674802 = 4 + l_id_3674802) {"
 "      /* mapGlobal */"
 "      for (int gl_id_3674803 = get_global_id(0); gl_id_3674803 < 1; gl_id_3674803 = 512 + gl_id_3674803) {"
 "        /* mapWorkGroup */"
 "        for (int wg_id_3674804 = get_group_id(0); wg_id_3674804 < 1; wg_id_3674804 = 16 + wg_id_3674804) {"
 "          /* oclReduceSeq */"
 "          {"
 "            float x3674772;"
 "            x3674772 = 0.0f;"
 "            for (int i_3674805 = 0; i_3674805 < n160; i_3674805 = 1 + i_3674805) {"
 "              x3674772 = x3674772 + (e162[(((i_3674805 + (gl_id_3674801 * n160)) + (gl_id_3674803 * n160)) + (l_id_3674802 * n160)) + (n160 * wg_id_3674804)] * e163[i_3674805]);"
 "            }"
 "            "
 "            output[((gl_id_3674801 + gl_id_3674803) + l_id_3674802) + wg_id_3674804] = x3674772;"
 "          }"
 "          "
 "        }"
 "        "
 "      }"
 "      "
 "    }"
 "    "
 "  }"
 "  "
 "}"
 "";

 #define loadKernel(ctx, id)\
   loadKernelFromSource(ctx, #id, id##_source, sizeof(id##_source) - 1)

 #include "ocl/ocl.h"
 struct fun_t {
   Kernel k0;
 };

 typedef struct fun_t fun_t;

 void fun_init(Context ctx, fun_t* self){
   (*self).k0 = loadKernel(ctx, k0);
 }

 void fun_destroy(Context ctx, fun_t* self){
   destroyKernel(ctx, (*self).k0);
 }

 void fun_run(Context ctx, fun_t* self, Buffer moutput, int n160, int n161, Buffer me162, Buffer me163){
   {
     DeviceBuffer b0 = deviceBufferSync(ctx, moutput, n161 * sizeof(float), DEVICE_WRITE);
     DeviceBuffer b3 = deviceBufferSync(ctx, me162, n161 * (n160 * sizeof(float)), DEVICE_READ);
     DeviceBuffer b4 = deviceBufferSync(ctx, me163, n160 * sizeof(float), DEVICE_READ);
     const size_t global_size[3] = (const size_t[3]){512, 32, 1};
     const size_t local_size[3] = (const size_t[3]){32, 4, 1};
     const KernelArg args[5] = (const KernelArg[5]){KARG(b0), KARG(n160), KARG(n161), KARG(b3), KARG(b4)};
     launchKernel(ctx, (*self).k0, global_size, local_size, 5, args);
   }

 }

 void fun_init_run(Context ctx, Buffer moutput, int n160, int n161, Buffer me162, Buffer me163){
   fun_t fun;
   fun_init(ctx, &fun);
   fun_run(ctx, &fun, moutput, n160, n161, me162, me163);
   fun_destroy(ctx, &fun);
 }




 int main(int argc, char** argv) {
   Context ctx = createDefaultContext();
   fun_t fun;
   fun_init(ctx, &fun);

   FILE* file;
   Buffer inBuff0 = createBuffer(ctx, 1048576 * sizeof(float), HOST_WRITE | DEVICE_READ);
   float* in0 = hostBufferSync(ctx, inBuff0, 1048576 * sizeof(float), HOST_WRITE);
   file = fopen("/tmp/mvInput16683767421335608290", "r");
   for (int i = 0; i < 1048576; i++) {
     fscanf(file, "%f", &in0[i]);
   }
   fclose(file);


   Buffer inBuff1 = createBuffer(ctx, 1024 * sizeof(float), HOST_WRITE | DEVICE_READ);
   float* in1 = hostBufferSync(ctx, inBuff1, 1024 * sizeof(float), HOST_WRITE);
   file = fopen("/tmp/mvInput17647910111591046576", "r");
   for (int i = 0; i < 1024; i++) {
     fscanf(file, "%f", &in1[i]);
   }
   fclose(file);


  Buffer outBuff = createBuffer(ctx, 1024 * sizeof(float), HOST_READ | DEVICE_WRITE);
  float* out = hostBufferSync(ctx, outBuff, 1024 * sizeof(float), HOST_READ);


   int iterations = atoi(argv[1]);
   for (int sample = 0; sample < iterations; sample++) {
       fun_run(ctx, &fun, outBuff, 1024, 1024, inBuff0, inBuff1);
   }

   file = fopen("/tmp/mvInput10650451907416943904", "r");
   float x;
   int diffs = 0;
   for (int i = 0; i < 1024; i++) {
     if(out[i] != fscanf(file, "%f", &x)){
       diffs++;
     }
   }

  destroyBuffer(ctx, inBuff0);
  destroyBuffer(ctx, inBuff1);

   destroyBuffer(ctx, outBuff);

   if(diffs > 0){ //Not OK
     exit(42);
   }

   fun_destroy(ctx, &fun);
   destroyContext(ctx);
   return EXIT_SUCCESS;
 }#include <stdio.h>
  #include <stdlib.h>


  const char k0_source[] =
  ""
  ""
  ""
  "__kernel __attribute__ ((reqd_work_group_size(32, 4, 1)))"
  "void k0(global float* restrict output, int n160, int n161, const global float* restrict e162, const global float* restrict e163){"
  "  /* Start of moved local vars */"
  "  /* End of moved local vars */"
  "  /* mapGlobal */"
  "  for (int gl_id_3674801 = get_global_id(1); gl_id_3674801 < n161; gl_id_3674801 = 32 + gl_id_3674801) {"
  "    /* mapLocal */"
  "    for (int l_id_3674802 = get_local_id(1); l_id_3674802 < 1; l_id_3674802 = 4 + l_id_3674802) {"
  "      /* mapGlobal */"
  "      for (int gl_id_3674803 = get_global_id(0); gl_id_3674803 < 1; gl_id_3674803 = 512 + gl_id_3674803) {"
  "        /* mapWorkGroup */"
  "        for (int wg_id_3674804 = get_group_id(0); wg_id_3674804 < 1; wg_id_3674804 = 16 + wg_id_3674804) {"
  "          /* oclReduceSeq */"
  "          {"
  "            float x3674772;"
  "            x3674772 = 0.0f;"
  "            for (int i_3674805 = 0; i_3674805 < n160; i_3674805 = 1 + i_3674805) {"
  "              x3674772 = x3674772 + (e162[(((i_3674805 + (gl_id_3674801 * n160)) + (gl_id_3674803 * n160)) + (l_id_3674802 * n160)) + (n160 * wg_id_3674804)] * e163[i_3674805]);"
  "            }"
  "            "
  "            output[((gl_id_3674801 + gl_id_3674803) + l_id_3674802) + wg_id_3674804] = x3674772;"
  "          }"
  "          "
  "        }"
  "        "
  "      }"
  "      "
  "    }"
  "    "
  "  }"
  "  "
  "}"
  "";

  #define loadKernel(ctx, id)\
    loadKernelFromSource(ctx, #id, id##_source, sizeof(id##_source) - 1)

  #include "ocl/ocl.h"
  struct fun_t {
    Kernel k0;
  };

  typedef struct fun_t fun_t;

  void fun_init(Context ctx, fun_t* self){
    (*self).k0 = loadKernel(ctx, k0);
  }

  void fun_destroy(Context ctx, fun_t* self){
    destroyKernel(ctx, (*self).k0);
  }

  void fun_run(Context ctx, fun_t* self, Buffer moutput, int n160, int n161, Buffer me162, Buffer me163){
    {
      DeviceBuffer b0 = deviceBufferSync(ctx, moutput, n161 * sizeof(float), DEVICE_WRITE);
      DeviceBuffer b3 = deviceBufferSync(ctx, me162, n161 * (n160 * sizeof(float)), DEVICE_READ);
      DeviceBuffer b4 = deviceBufferSync(ctx, me163, n160 * sizeof(float), DEVICE_READ);
      const size_t global_size[3] = (const size_t[3]){512, 32, 1};
      const size_t local_size[3] = (const size_t[3]){32, 4, 1};
      const KernelArg args[5] = (const KernelArg[5]){KARG(b0), KARG(n160), KARG(n161), KARG(b3), KARG(b4)};
      launchKernel(ctx, (*self).k0, global_size, local_size, 5, args);
    }

  }

  void fun_init_run(Context ctx, Buffer moutput, int n160, int n161, Buffer me162, Buffer me163){
    fun_t fun;
    fun_init(ctx, &fun);
    fun_run(ctx, &fun, moutput, n160, n161, me162, me163);
    fun_destroy(ctx, &fun);
  }




  int main(int argc, char** argv) {
    Context ctx = createDefaultContext();
    fun_t fun;
    fun_init(ctx, &fun);

    FILE* file;
    Buffer inBuff0 = createBuffer(ctx, 1048576 * sizeof(float), HOST_WRITE | DEVICE_READ);
    float* in0 = hostBufferSync(ctx, inBuff0, 1048576 * sizeof(float), HOST_WRITE);
    file = fopen("/tmp/mvInput16683767421335608290", "r");
    for (int i = 0; i < 1048576; i++) {
      fscanf(file, "%f", &in0[i]);
    }
    fclose(file);


    Buffer inBuff1 = createBuffer(ctx, 1024 * sizeof(float), HOST_WRITE | DEVICE_READ);
    float* in1 = hostBufferSync(ctx, inBuff1, 1024 * sizeof(float), HOST_WRITE);
    file = fopen("/tmp/mvInput17647910111591046576", "r");
    for (int i = 0; i < 1024; i++) {
      fscanf(file, "%f", &in1[i]);
    }
    fclose(file);


   Buffer outBuff = createBuffer(ctx, 1024 * sizeof(float), HOST_READ | DEVICE_WRITE);
   float* out = hostBufferSync(ctx, outBuff, 1024 * sizeof(float), HOST_READ);


    int iterations = atoi(argv[1]);
    for (int sample = 0; sample < iterations; sample++) {
        fun_run(ctx, &fun, outBuff, 1024, 1024, inBuff0, inBuff1);
    }

    file = fopen("/tmp/mvInput10650451907416943904", "r");
    float x;
    int diffs = 0;
    for (int i = 0; i < 1024; i++) {
      if(out[i] != fscanf(file, "%f", &x)){
        diffs++;
      }
    }

   destroyBuffer(ctx, inBuff0);
   destroyBuffer(ctx, inBuff1);

    destroyBuffer(ctx, outBuff);

    if(diffs > 0){ //Not OK
      exit(42);
    }

    fun_destroy(ctx, &fun);
    destroyContext(ctx);
    return EXIT_SUCCESS;
  }#include <stdio.h>
   #include <stdlib.h>


   const char k0_source[] =
   ""
   ""
   ""
   "__kernel __attribute__ ((reqd_work_group_size(32, 4, 1)))"
   "void k0(global float* restrict output, int n160, int n161, const global float* restrict e162, const global float* restrict e163){"
   "  /* Start of moved local vars */"
   "  /* End of moved local vars */"
   "  /* mapGlobal */"
   "  for (int gl_id_3674801 = get_global_id(1); gl_id_3674801 < n161; gl_id_3674801 = 32 + gl_id_3674801) {"
   "    /* mapLocal */"
   "    for (int l_id_3674802 = get_local_id(1); l_id_3674802 < 1; l_id_3674802 = 4 + l_id_3674802) {"
   "      /* mapGlobal */"
   "      for (int gl_id_3674803 = get_global_id(0); gl_id_3674803 < 1; gl_id_3674803 = 512 + gl_id_3674803) {"
   "        /* mapWorkGroup */"
   "        for (int wg_id_3674804 = get_group_id(0); wg_id_3674804 < 1; wg_id_3674804 = 16 + wg_id_3674804) {"
   "          /* oclReduceSeq */"
   "          {"
   "            float x3674772;"
   "            x3674772 = 0.0f;"
   "            for (int i_3674805 = 0; i_3674805 < n160; i_3674805 = 1 + i_3674805) {"
   "              x3674772 = x3674772 + (e162[(((i_3674805 + (gl_id_3674801 * n160)) + (gl_id_3674803 * n160)) + (l_id_3674802 * n160)) + (n160 * wg_id_3674804)] * e163[i_3674805]);"
   "            }"
   "            "
   "            output[((gl_id_3674801 + gl_id_3674803) + l_id_3674802) + wg_id_3674804] = x3674772;"
   "          }"
   "          "
   "        }"
   "        "
   "      }"
   "      "
   "    }"
   "    "
   "  }"
   "  "
   "}"
   "";

   #define loadKernel(ctx, id)\
     loadKernelFromSource(ctx, #id, id##_source, sizeof(id##_source) - 1)

   #include "ocl/ocl.h"
   struct fun_t {
     Kernel k0;
   };

   typedef struct fun_t fun_t;

   void fun_init(Context ctx, fun_t* self){
     (*self).k0 = loadKernel(ctx, k0);
   }

   void fun_destroy(Context ctx, fun_t* self){
     destroyKernel(ctx, (*self).k0);
   }

   void fun_run(Context ctx, fun_t* self, Buffer moutput, int n160, int n161, Buffer me162, Buffer me163){
     {
       DeviceBuffer b0 = deviceBufferSync(ctx, moutput, n161 * sizeof(float), DEVICE_WRITE);
       DeviceBuffer b3 = deviceBufferSync(ctx, me162, n161 * (n160 * sizeof(float)), DEVICE_READ);
       DeviceBuffer b4 = deviceBufferSync(ctx, me163, n160 * sizeof(float), DEVICE_READ);
       const size_t global_size[3] = (const size_t[3]){512, 32, 1};
       const size_t local_size[3] = (const size_t[3]){32, 4, 1};
       const KernelArg args[5] = (const KernelArg[5]){KARG(b0), KARG(n160), KARG(n161), KARG(b3), KARG(b4)};
       launchKernel(ctx, (*self).k0, global_size, local_size, 5, args);
     }

   }

   void fun_init_run(Context ctx, Buffer moutput, int n160, int n161, Buffer me162, Buffer me163){
     fun_t fun;
     fun_init(ctx, &fun);
     fun_run(ctx, &fun, moutput, n160, n161, me162, me163);
     fun_destroy(ctx, &fun);
   }




   int main(int argc, char** argv) {
     Context ctx = createDefaultContext();
     fun_t fun;
     fun_init(ctx, &fun);

     FILE* file;
     Buffer inBuff0 = createBuffer(ctx, 1048576 * sizeof(float), HOST_WRITE | DEVICE_READ);
     float* in0 = hostBufferSync(ctx, inBuff0, 1048576 * sizeof(float), HOST_WRITE);
     file = fopen("/tmp/mvInput16683767421335608290", "r");
     for (int i = 0; i < 1048576; i++) {
       fscanf(file, "%f", &in0[i]);
     }
     fclose(file);


     Buffer inBuff1 = createBuffer(ctx, 1024 * sizeof(float), HOST_WRITE | DEVICE_READ);
     float* in1 = hostBufferSync(ctx, inBuff1, 1024 * sizeof(float), HOST_WRITE);
     file = fopen("/tmp/mvInput17647910111591046576", "r");
     for (int i = 0; i < 1024; i++) {
       fscanf(file, "%f", &in1[i]);
     }
     fclose(file);


    Buffer outBuff = createBuffer(ctx, 1024 * sizeof(float), HOST_READ | DEVICE_WRITE);
    float* out = hostBufferSync(ctx, outBuff, 1024 * sizeof(float), HOST_READ);


     int iterations = atoi(argv[1]);
     for (int sample = 0; sample < iterations; sample++) {
         fun_run(ctx, &fun, outBuff, 1024, 1024, inBuff0, inBuff1);
     }

     file = fopen("/tmp/mvInput10650451907416943904", "r");
     float x;
     int diffs = 0;
     for (int i = 0; i < 1024; i++) {
       if(out[i] != fscanf(file, "%f", &x)){
         diffs++;
       }
     }

    destroyBuffer(ctx, inBuff0);
    destroyBuffer(ctx, inBuff1);

     destroyBuffer(ctx, outBuff);

     if(diffs > 0){ //Not OK
       exit(42);
     }

     fun_destroy(ctx, &fun);
     destroyContext(ctx);
     return EXIT_SUCCESS;
   }#include <stdio.h>
    #include <stdlib.h>


    const char k0_source[] =
    ""
    ""
    ""
    "__kernel __attribute__ ((reqd_work_group_size(32, 4, 1)))"
    "void k0(global float* restrict output, int n160, int n161, const global float* restrict e162, const global float* restrict e163){"
    "  /* Start of moved local vars */"
    "  /* End of moved local vars */"
    "  /* mapGlobal */"
    "  for (int gl_id_3674801 = get_global_id(1); gl_id_3674801 < n161; gl_id_3674801 = 32 + gl_id_3674801) {"
    "    /* mapLocal */"
    "    for (int l_id_3674802 = get_local_id(1); l_id_3674802 < 1; l_id_3674802 = 4 + l_id_3674802) {"
    "      /* mapGlobal */"
    "      for (int gl_id_3674803 = get_global_id(0); gl_id_3674803 < 1; gl_id_3674803 = 512 + gl_id_3674803) {"
    "        /* mapWorkGroup */"
    "        for (int wg_id_3674804 = get_group_id(0); wg_id_3674804 < 1; wg_id_3674804 = 16 + wg_id_3674804) {"
    "          /* oclReduceSeq */"
    "          {"
    "            float x3674772;"
    "            x3674772 = 0.0f;"
    "            for (int i_3674805 = 0; i_3674805 < n160; i_3674805 = 1 + i_3674805) {"
    "              x3674772 = x3674772 + (e162[(((i_3674805 + (gl_id_3674801 * n160)) + (gl_id_3674803 * n160)) + (l_id_3674802 * n160)) + (n160 * wg_id_3674804)] * e163[i_3674805]);"
    "            }"
    "            "
    "            output[((gl_id_3674801 + gl_id_3674803) + l_id_3674802) + wg_id_3674804] = x3674772;"
    "          }"
    "          "
    "        }"
    "        "
    "      }"
    "      "
    "    }"
    "    "
    "  }"
    "  "
    "}"
    "";

    #define loadKernel(ctx, id)\
      loadKernelFromSource(ctx, #id, id##_source, sizeof(id##_source) - 1)

    #include "ocl/ocl.h"
    struct fun_t {
      Kernel k0;
    };

    typedef struct fun_t fun_t;

    void fun_init(Context ctx, fun_t* self){
      (*self).k0 = loadKernel(ctx, k0);
    }

    void fun_destroy(Context ctx, fun_t* self){
      destroyKernel(ctx, (*self).k0);
    }

    void fun_run(Context ctx, fun_t* self, Buffer moutput, int n160, int n161, Buffer me162, Buffer me163){
      {
        DeviceBuffer b0 = deviceBufferSync(ctx, moutput, n161 * sizeof(float), DEVICE_WRITE);
        DeviceBuffer b3 = deviceBufferSync(ctx, me162, n161 * (n160 * sizeof(float)), DEVICE_READ);
        DeviceBuffer b4 = deviceBufferSync(ctx, me163, n160 * sizeof(float), DEVICE_READ);
        const size_t global_size[3] = (const size_t[3]){512, 32, 1};
        const size_t local_size[3] = (const size_t[3]){32, 4, 1};
        const KernelArg args[5] = (const KernelArg[5]){KARG(b0), KARG(n160), KARG(n161), KARG(b3), KARG(b4)};
        launchKernel(ctx, (*self).k0, global_size, local_size, 5, args);
      }

    }

    void fun_init_run(Context ctx, Buffer moutput, int n160, int n161, Buffer me162, Buffer me163){
      fun_t fun;
      fun_init(ctx, &fun);
      fun_run(ctx, &fun, moutput, n160, n161, me162, me163);
      fun_destroy(ctx, &fun);
    }




    int main(int argc, char** argv) {
      Context ctx = createDefaultContext();
      fun_t fun;
      fun_init(ctx, &fun);

      FILE* file;
      Buffer inBuff0 = createBuffer(ctx, 1048576 * sizeof(float), HOST_WRITE | DEVICE_READ);
      float* in0 = hostBufferSync(ctx, inBuff0, 1048576 * sizeof(float), HOST_WRITE);
      file = fopen("/tmp/mvInput16683767421335608290", "r");
      for (int i = 0; i < 1048576; i++) {
        fscanf(file, "%f", &in0[i]);
      }
      fclose(file);


      Buffer inBuff1 = createBuffer(ctx, 1024 * sizeof(float), HOST_WRITE | DEVICE_READ);
      float* in1 = hostBufferSync(ctx, inBuff1, 1024 * sizeof(float), HOST_WRITE);
      file = fopen("/tmp/mvInput17647910111591046576", "r");
      for (int i = 0; i < 1024; i++) {
        fscanf(file, "%f", &in1[i]);
      }
      fclose(file);


     Buffer outBuff = createBuffer(ctx, 1024 * sizeof(float), HOST_READ | DEVICE_WRITE);
     float* out = hostBufferSync(ctx, outBuff, 1024 * sizeof(float), HOST_READ);


      int iterations = atoi(argv[1]);
      for (int sample = 0; sample < iterations; sample++) {
          fun_run(ctx, &fun, outBuff, 1024, 1024, inBuff0, inBuff1);
      }

      file = fopen("/tmp/mvInput10650451907416943904", "r");
      float x;
      int diffs = 0;
      for (int i = 0; i < 1024; i++) {
        if(out[i] != fscanf(file, "%f", &x)){
          diffs++;
        }
      }

     destroyBuffer(ctx, inBuff0);
     destroyBuffer(ctx, inBuff1);

      destroyBuffer(ctx, outBuff);

      if(diffs > 0){ //Not OK
        exit(42);
      }

      fun_destroy(ctx, &fun);
      destroyContext(ctx);
      return EXIT_SUCCESS;
    }