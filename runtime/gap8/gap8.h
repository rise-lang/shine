#ifndef SHINE_GAP8_H
#define SHINE_GAP8_H

#include "pmsis.h"
#include "gaplib/ImgIO.h"
#include "../runtime.h"

struct ContextImpl {
    struct pi_device cl_device;
    struct pi_cluster_conf cl_configuration;
};

struct KernelImpl {
    struct pi_cluster_task* cl_task;
};

struct BufferImpl {
    void* inner;
    size_t byte_size;
};


typedef void* DeviceBuffer;



Context createContext(int device_id);
Kernel loadKernel(void (*handler)(void*), size_t stack_size);
void launchKernel(
  Context ctx, Kernel k,
  int num_threads,
  size_t arg_count, const KernelArg* args);

#endif
