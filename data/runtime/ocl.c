#include "runtime.h"

const char* oclErrorToString(cl_int error) {
  switch (error) {
#define variant(x) case x: return #x;
    variant(CL_SUCCESS)
    variant(CL_DEVICE_NOT_FOUND)
    variant(CL_DEVICE_NOT_AVAILABLE)
    variant(CL_COMPILER_NOT_AVAILABLE)
    variant(CL_MEM_OBJECT_ALLOCATION_FAILURE)
    variant(CL_OUT_OF_RESOURCES)
    variant(CL_OUT_OF_HOST_MEMORY)
    variant(CL_PROFILING_INFO_NOT_AVAILABLE)
    variant(CL_MEM_COPY_OVERLAP)
    variant(CL_IMAGE_FORMAT_MISMATCH)
    variant(CL_IMAGE_FORMAT_NOT_SUPPORTED)
    variant(CL_BUILD_PROGRAM_FAILURE)
    variant(CL_MAP_FAILURE)
    variant(CL_MISALIGNED_SUB_BUFFER_OFFSET)
    variant(CL_EXEC_STATUS_ERROR_FOR_EVENTS_IN_WAIT_LIST)
    variant(CL_COMPILE_PROGRAM_FAILURE)
    variant(CL_LINKER_NOT_AVAILABLE)
    variant(CL_LINK_PROGRAM_FAILURE)
    variant(CL_DEVICE_PARTITION_FAILED)
    variant(CL_KERNEL_ARG_INFO_NOT_AVAILABLE)
    variant(CL_INVALID_VALUE)
    variant(CL_INVALID_DEVICE_TYPE)
    variant(CL_INVALID_PLATFORM)
    variant(CL_INVALID_DEVICE)
    variant(CL_INVALID_CONTEXT)
    variant(CL_INVALID_QUEUE_PROPERTIES)
    variant(CL_INVALID_COMMAND_QUEUE)
    variant(CL_INVALID_HOST_PTR)
    variant(CL_INVALID_MEM_OBJECT)
    variant(CL_INVALID_IMAGE_DESCRIPTOR)
    variant(CL_INVALID_IMAGE_SIZE)
    variant(CL_INVALID_SAMPLER)
    variant(CL_INVALID_BINARY)
    variant(CL_INVALID_BUILD_OPTIONS)
    variant(CL_INVALID_PROGRAM)
    variant(CL_INVALID_PROGRAM_EXECUTABLE)
    variant(CL_INVALID_KERNEL_NAME)
    variant(CL_INVALID_KERNEL_DEFINITION)
    variant(CL_INVALID_KERNEL)
    variant(CL_INVALID_ARG_INDEX)
    variant(CL_INVALID_ARG_VALUE)
    variant(CL_INVALID_ARG_SIZE)
    variant(CL_INVALID_KERNEL_ARGS)
    variant(CL_INVALID_WORK_DIMENSION)
    variant(CL_INVALID_WORK_GROUP_SIZE)
    variant(CL_INVALID_WORK_ITEM_SIZE)
    variant(CL_INVALID_GLOBAL_OFFSET)
    variant(CL_INVALID_EVENT_WAIT_LIST)
    variant(CL_INVALID_EVENT)
    variant(CL_INVALID_OPERATION)
    variant(CL_INVALID_BUFFER_SIZE)
    variant(CL_INVALID_GLOBAL_WORK_SIZE)
    variant(CL_INVALID_PROPERTY)
    variant(CL_INVALID_COMPILER_OPTIONS)
    variant(CL_INVALID_LINKER_OPTIONS)
    variant(CL_INVALID_DEVICE_PARTITION_COUNT)
#undef variant
    default: return "UNKNOWN CL ERROR";
  }
}

bool oclReportError(cl_int error, const char* msg) {
  if (error != CL_SUCCESS) {
    fprintf(stderr, "%s: %s\n", msg, ocl_error_to_string(error));
    return true;
  }
  return false;
}

void oclFatalError(cl_int error, const char* msg) {
  if (oclReportError(error, msg)) {
    exit(EXIT_FAILURE);
  }
}

Kernel loadKernel(Context ctx, const char* name, const char* path) {
  FILE* f = fopen(path, "rb");
  if (!f) {
      fprintf(stderr, "could not open kernel source\n");
      exit(EXIT_FAILURE);
  }
  fseek(f, 0, SEEK_END);
  size_t length = ftell(f);
  rewind(f);
  char* source = (char*) malloc(length * sizeof(char));
  if (fread(source, sizeof(char), length, f) != length) {
      fprintf(stderr, "could not read kernel source\n");
      exit(EXIT_FAILURE);
  }
  fclose(f);

  cl_int err;
  const char* sources[] = { source };
  const size_t lengths[] = { length };
  cl_program program = clCreateProgramWithSource(ctx->inner, 1, sources, lengths, &err);
  free(source);
  oclFatalError(err, "could not create program");

  // 2.0: -cl-uniform-work-group-size
  const char* options = "-cl-fast-relaxed-math -Werror -cl-std=CL1.2";
  if (oclReportError(
    clBuildProgram(program, 1, &ctx->device, options, NULL, NULL),
    "could not build program"
  )) {
      size_t log_size;
      oclFatalError(clGetProgramBuildInfo(program, ctx->device, CL_PROGRAM_BUILD_LOG, 0, NULL, &log_size),
        "could not get program info");
      char* log_string = (char*) malloc(log_size * sizeof(char));
      oclFatalError(clGetProgramBuildInfo(program, ctx->device, CL_PROGRAM_BUILD_LOG, log_size, log_string, NULL),
        "could not get program info");
      fprintf(stderr, "%s\n", log_string);
      free(log_string);
      exit(EXIT_FAILURE);
  }

  cl_kernel kernel = clCreateKernel(program, name, &err);
  oclFatalError(err, "could not create kernel");

  Kernel k = malloc(sizeof(KernelImpl));
  k->program = program;
  k->inner = kernel;
  return k;
}

void launchKernel(Context ctx, Kernel k, const size_t* global_size, const size_t* local_size) {
  oclFatalError(
    clEnqueueNDRangeKernel(ctx->queue, k->inner, 3, NULL, global_size, local_size, 0, NULL, NULL),
    "could not launch kernel"
  )
}

void destroyKernel(Context ctx, Kernel k) {
  oclReportError(clReleaseKernel(k->inner), "kernel release");
  oclReportError(clReleaseProgram(k->program), "program release");
  free(k);
}