#include "runtime.h"

struct BufferImpl {
  void* host_mem;
  cl_mem target_mem;
  bool host_dirty;
  bool target_dirty;
};

static
cl_mem_flags accessToMemFlags(AccessFlags in) {
  cl_mem_flags out = 0;
  if ((in & TARGET_READ) && (in & TARGET_WRITE)) {
    out |= CL_MEM_READ_WRITE;
  } else {
    if (in & TARGET_READ) { out |= CL_MEM_READ_ONLY; };
    if (in & TARGET_WRITE) { out |= CL_MEM_WRITE_ONLY; };
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
  if ((access & TARGET_READ) || (access & TARGET_WRITE)) {
    cl_int err;
    b->target_mem = clCreateBuffer(ctx->inner, accessToMemFlags(access), byte_size, NULL, &err);
    oclFatalError(err, "could not create buffer");
  } else {
    b->target_mem = 0;
  }
  b->host_dirty = false;
  b->target_dirty = false;
  return b;
}

void destroyBuffer(Context ctx, Buffer b) {
  if (b->host_mem != NULL) { free(b->host_mem); }
  if (b->target_mem != 0) {
    oclReportError(clReleaseMemObject(b->target_mem), "buffer release");
  }
  free(b);
}

void* hostBufferSync(Context ctx, Buffer b, size_t byte_size, AccessFlags access) {
  if ((access & HOST_READ) && b->target_dirty) {
    oclFatalError(clEnqueueReadBuffer(ctx->queue, b->target_mem, CL_TRUE, 0, byte_size,
      b->host_mem, 0, NULL, NULL),
      "could not download buffer data to host"
    );
  }
  if (access & HOST_WRITE) {
    b->host_dirty = true;
  }
  return b->host_mem;
}

cl_mem targetBufferSync(Context ctx, Buffer b, size_t byte_size, AccessFlags access) {
  if ((access & TARGET_READ) && b->host_dirty) {
    oclFatalError(clEnqueueWriteBuffer(ctx->queue, b->target_mem, CL_FALSE, 0, byte_size,
      b->host_mem, 0, NULL, NULL),
      "could not upload buffer data to host"
    );
  }
  if (access & TARGET_WRITE) {
    b->target_dirty = true;
  }
  return b->target_mem;
}