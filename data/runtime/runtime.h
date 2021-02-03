#ifndef SHINE_RUNTIME_H
#define SHINE_RUNTIME_H

#include <stdlib.h>
#include <stdint.h>
#include <stdbool.h>
#include <stdio.h>
#include "ocl.h"

typedef struct ContextImpl* Context;
typedef struct KernelImpl* Kernel;
typedef struct BufferImpl* Buffer;

enum AccessFlags {
  HOST_READ = 1 << 0,
  HOST_WRITE = 1 << 1,
  TARGET_READ = 1 << 2,
  TARGET_WRITE = 1 << 3,
};

Buffer createBuffer(Context ctx, size_t byte_size, AccessFlags access);
void destroyBuffer(Context ctx, Buffer b);

void* hostBufferSync(Context ctx, Buffer b, size_t byte_size, AccessFlags access);
cl_mem targetBufferSync(Context ctx, Buffer b, size_t byte_size, AccessFlags access);

Kernel loadKernel(Context ctx, const char* path);
void launchKernel(Context ctx, Kernel k, const size_t* global_size, const size_t* local_size);
void destroyKernel(Context ctx, Kernel k);

#endif