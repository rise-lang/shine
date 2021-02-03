#ifndef SHINE_RUNTIME_H
#define SHINE_RUNTIME_H

#include <stdint.h>
#include <stdbool.h>

#define CL_TARGET_OPENCL_VERSION 120
#define CL_USE_DEPRECATED_OPENCL_1_2_APIS
#ifdef __APPLE__
    #include "OpenCL/opencl.h"
#else
    #include "CL/cl.h"
#endif

struct ContextImpl {
  cl_context ocl;
  cl_command_queue queue;
};

typedef struct ContextImpl* Context;
typedef struct BufferImpl* Buffer;

enum AccessFlags {
  HOST_READ = 1 << 0,
  HOST_WRITE = 1 << 1,
  TARGET_READ = 1 << 2,
  TARGET_WRITE = 1 << 3,
};

Buffer createBuffer(Context ctx, size_t byte_size, AccessFlags access);
destroyBuffer(Context ctx, Buffer b);

void* hostBufferSync(Context ctx, Buffer b, size_t byte_size, AccessFlags access);
void targetBufferSync(Context ctx, Buffer b, size_t byte_size, AccessFlags access);

#endif