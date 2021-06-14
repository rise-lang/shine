#ifndef SHINE_OCL_H
#define SHINE_OCL_H


#define CL_TARGET_OPENCL_VERSION 120
#define CL_USE_DEPRECATED_OPENCL_1_2_APIS
#ifdef __APPLE__
  #include "OpenCL/opencl.h"
#else
  #include "CL/cl.h"
#endif

//Can't forward reference typedef
typedef cl_mem DeviceBuffer;

#include "../runtime.h"

struct ContextImpl {
  cl_context inner;
  cl_platform_id platform;
  cl_device_id device;
  cl_command_queue queue;
};

struct KernelImpl {
  cl_kernel inner;
  cl_program program;
};

Context createContext(const char* platform_subname, const char* device_type_str);
Kernel loadKernelFromSource(Context ctx, const char* name, const char* source, size_t length);
Kernel loadKernelFromFile(Context ctx, const char* name, const char* path);
void launchKernel(
  Context ctx, Kernel k,
  const size_t* global_size, const size_t* local_size,
  size_t arg_count, const KernelArg* args);

const char* oclErrorToString(cl_int error);
bool oclReportError(cl_int error, const char* msg);
void oclFatalError(cl_int error, const char* msg);



#endif
