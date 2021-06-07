#ifndef SHINE_GAP8_H
#define SHINE_GAP8_H

#include "pmsis.h"
#include "gaplib/ImgIO.h"

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

struct ContextParamsImpl {
    int device_id;
};

struct KernelParamsImpl {
    void (*handler)(void* args);
    size_t stack_size;
}

struct LaunchConfigImpl {
    int num_threads;
}


typedef void* DeviceBuffer;

#define KARG(val) { sizeof(val), &val }
#define LARG(size) { size, NULL }

#endif
