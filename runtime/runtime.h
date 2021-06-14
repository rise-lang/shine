#ifndef SHINE_RUNTIME_H
#define SHINE_RUNTIME_H

#include <stdlib.h>
#include <stdint.h>
#include <stdbool.h>
#include <stdio.h>

typedef struct ContextImpl* Context;
typedef struct KernelImpl* Kernel;
typedef struct BufferImpl* Buffer;

typedef struct {
  size_t size;
  void* value;
} KernelArg;

//#ifndef SHINE_TARGET_GAP8
//    #include "ocl/ocl.h"
//#else
//    #include "gap8/gap8.h"
//#endif
#include "time_utils.h"

typedef enum {
  HOST_READ = 1 << 0,
  HOST_WRITE = 1 << 1,
  DEVICE_READ = 1 << 2,
  DEVICE_WRITE = 1 << 3,
} AccessFlags;

Context createDefaultContext();
void destroyContext(Context ctx);
void waitFinished(Context ctx);

Buffer createBuffer(Context ctx, size_t byte_size, AccessFlags access);
void destroyBuffer(Context ctx, Buffer b);
void* hostBufferSync(Context ctx, Buffer b, size_t byte_size, AccessFlags access);
DeviceBuffer deviceBufferSync(Context ctx, Buffer b, size_t byte_size, AccessFlags access);

void destroyKernel(Context ctx, Kernel k);

#define KARG(val) { sizeof(val), &val }
#define LARG(size) { size, NULL }

#endif
