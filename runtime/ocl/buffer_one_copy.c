#include "ocl.h"

struct BufferImpl {
  void* host_mem;
  cl_mem device_mem;
  bool host_dirty;
  bool device_dirty;
};

static
cl_mem_flags accessToMemFlags(AccessFlags in) {
  cl_mem_flags out = 0;
  if ((in & DEVICE_READ) && (in & DEVICE_WRITE)) {
    out |= CL_MEM_READ_WRITE;
  } else {
    if (in & DEVICE_READ) { out |= CL_MEM_READ_ONLY; };
    if (in & DEVICE_WRITE) { out |= CL_MEM_WRITE_ONLY; };
  }
  return out;
}

Buffer createBuffer(Context ctx, size_t byte_size, AccessFlags access) {
  Buffer b = malloc(sizeof(struct BufferImpl));
  if ((access & HOST_READ) || (access & HOST_WRITE)) {
    b->host_mem = malloc(byte_size);
  } else {
    b->host_mem = NULL;
  }
  if ((access & DEVICE_READ) || (access & DEVICE_WRITE)) {
    cl_int err;
    b->device_mem = clCreateBuffer(ctx->inner, accessToMemFlags(access), byte_size, NULL, &err);
    oclFatalError(err, "could not create buffer");
  } else {
    b->device_mem = 0;
  }
  b->host_dirty = false;
  b->device_dirty = false;
  return b;
}

void destroyBuffer(Context ctx, Buffer b) {
  if (b->host_mem != NULL) { free(b->host_mem); }
  if (b->device_mem != 0) {
    oclReportError(clReleaseMemObject(b->device_mem), "buffer release");
  }
  free(b);
}

void* hostBufferSync(Context ctx, Buffer b, size_t byte_size, AccessFlags access) {
  if ((access & HOST_READ) && b->device_dirty) {
    oclFatalError(clEnqueueReadBuffer(ctx->queue, b->device_mem, CL_TRUE, 0, byte_size,
      b->host_mem, 0, NULL, NULL),
      "could not download buffer data to host"
    );
    b->device_dirty = false;
  }
  if (access & HOST_WRITE) {
    b->host_dirty = true;
  }
  return b->host_mem;
}

DeviceBuffer deviceBufferSync(Context ctx, Buffer b, size_t byte_size, AccessFlags access) {
  if ((access & DEVICE_READ) && b->host_dirty) {
    oclFatalError(clEnqueueWriteBuffer(ctx->queue, b->device_mem, CL_FALSE, 0, byte_size,
      b->host_mem, 0, NULL, NULL),
      "could not upload buffer data to host"
    );
    b->host_dirty = false;
  }
  if (access & DEVICE_WRITE) {
    b->device_dirty = true;
  }
  return b->device_mem;
}
