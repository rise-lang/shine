#include "ocl.h"

struct BufferImpl {
  cl_mem mem;
  void* mapped;
  AccessFlags possibleAccess;
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
  cl_mem_flags mf = accessToMemFlags(access) | CL_MEM_ALLOC_HOST_PTR;
  Buffer b = malloc(sizeof(struct BufferImpl));
  cl_int err;
  b->mem = clCreateBuffer(ctx->inner, mf, byte_size, NULL, &err);
  oclFatalError(err, "could not create buffer");
  b->mapped = NULL;
  b->possibleAccess = access;
  return b;
}

void destroyBuffer(Context ctx, Buffer b) {
  oclReportError(clReleaseMemObject(b->mem), "buffer release");
  free(b);
}

static
cl_map_flags accessToMapFlags(AccessFlags in) {
  cl_map_flags out = 0;
  if (in & HOST_READ) { out |= CL_MAP_READ; };
  if (in & HOST_WRITE) { out |= CL_MAP_WRITE; };
  return out;
}

void* hostBufferSync(Context ctx, Buffer b, size_t byte_size, AccessFlags access) {
  if (b->mapped == NULL) {
    // the map flags consider the worst case access
    // otherwise a map(READ) followed by a map(WRITE) would require re-mapping
    // TODO: is this a good design choice?
    cl_int err;
    b->mapped = clEnqueueMapBuffer(ctx->queue, b->mem, CL_TRUE,
      accessToMapFlags(b->possibleAccess), 0, byte_size, 0, NULL, NULL, &err);
    oclFatalError(err, "could not map buffer on host");
  }
  return b->mapped;
}

DeviceBuffer deviceBufferSync(Context ctx, Buffer b, size_t byte_size, AccessFlags access) {
  if (b->mapped != NULL) {
    oclFatalError(clEnqueueUnmapMemObject(ctx->queue, b->mem, b->mapped, 0, NULL, NULL),
      "could not unmap buffer on host");
    b->mapped = NULL;
  }
  return b->mem;
}
