#include "runtime.h"

struct BufferImpl {
  cl_mem mem;
  void* mapped;
  AccessFlags worstCaseAccess;
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
  cl_mem_flags mf = accessToMemFlags(access) | CL_MEM_ALLOC_HOST_PTR;
  Buffer b = malloc(sizeof(BufferImpl));
  b->mem = clCreateBuffer(ctx->ocl, mf, byte_size, NULL, NULL);
  b->mapped = NULL;
  b->worstCaseAccess = access;
  return b;
}

void destroyBuffer(Context ctx, Buffer b) {
  clReleaseMemObject(b->mem);
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
    b->mapped = clEnqueueMapBuffer(ctx->queue, b->mem, CL_TRUE,
      accessToMapFlags(b->worstCaseAccess), 0, byte_size, 0, NULL, NULL, NULL);
    b->mapped = true;
  }
  return b->mapped;
}

void targetBufferSync(Context ctx, Buffer b, size_t byte_size, AccessFlags access) {
  if (b->mapped != NULL) {
    clEnqueueUnmapMemObject(ctx->queue, b->mem, b->mapped, 0, NULL, NULL);
    b->mapped = NULL;
  }
}