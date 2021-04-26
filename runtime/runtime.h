#ifndef SHINE_RUNTIME_H
#define SHINE_RUNTIME_H

#include <stdlib.h>
#include <stdint.h>
#include <stdbool.h>
#include <stdio.h>
#include "ocl.h"
#include "time_utils.h"

typedef struct ContextImpl* Context;
typedef struct KernelImpl* Kernel;
typedef struct BufferImpl* Buffer;

typedef enum {
  HOST_READ = 1 << 0,
  HOST_WRITE = 1 << 1,
  DEVICE_READ = 1 << 2,
  DEVICE_WRITE = 1 << 3,
} AccessFlags;

Context createDefaultContext();
Context createContext(const char* platform_subname, const char* device_type_str);
void destroyContext(Context ctx);
void waitFinished(Context ctx);

Buffer createBuffer(Context ctx, size_t byte_size, AccessFlags access);
void destroyBuffer(Context ctx, Buffer b);

void* hostBufferSync(Context ctx, Buffer b, size_t byte_size, AccessFlags access);

DeviceBuffer deviceBufferSync(Context ctx, Buffer b, size_t byte_size, AccessFlags access);

Kernel loadKernelFromSource(Context ctx, const char* name, const char* source, size_t length);
Kernel loadKernelFromFile(Context ctx, const char* name, const char* path);
void destroyKernel(Context ctx, Kernel k);

typedef struct {
  size_t size;
  void* value;
} KernelArg;

void launchKernel(
  Context ctx, Kernel k,
  const size_t* global_size, const size_t* local_size,
  size_t arg_count, const KernelArg* args);

#endif