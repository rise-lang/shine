#include "gap8.h"

Buffer createBuffer(Context ctx, size_t byte_size, AccessFlags access){
    Buffer buffer = (Buffer) pi_l2_malloc(sizeof(struct BufferImpl));
    buffer->inner = (void*) pi_l2_malloc(byte_size);
    buffer->byte_size = byte_size;
    return buffer;
}

void destroyBuffer(Context ctx, Buffer b){
    pmsis_l2_malloc_free(b->inner, b->byte_size);
    pmsis_l2_malloc_free(b, sizeof(struct BufferImpl));
}

void* hostBufferSync(Context ctx, Buffer b, size_t byte_size, AccessFlags access){
    return b->inner;
}
DeviceBuffer deviceBufferSync(Context ctx, Buffer b, size_t byte_size, AccessFlags access){
    return b->inner;
}
