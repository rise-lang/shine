#include "ocl.h"
#include <string.h>

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
    fprintf(stderr, "%s: %s\n", msg, oclErrorToString(error));
    return true;
  }
  return false;
}

void oclFatalError(cl_int error, const char* msg) {
  if (oclReportError(error, msg)) {
    exit(EXIT_FAILURE);
  }
}

const size_t NAME_BUF_SIZE = 512;

static
cl_platform_id find_platform(cl_uint platform_count, const cl_platform_id* platform_ids, const char* subname) {
  for (cl_uint i = 0; i < platform_count; i++) {
    char name_buf[NAME_BUF_SIZE];
    size_t name_size;
    oclFatalError(clGetPlatformInfo(platform_ids[i], CL_PLATFORM_NAME, sizeof(name_buf), name_buf, &name_size),
      "could not get platform info");
    if (name_size > sizeof(name_buf)) {
      fprintf(stderr, "did not expect such a long OpenCL platform name (%zu)\n", name_size);
      name_buf[NAME_BUF_SIZE - 1] = '\0';
    }

    if (strstr(name_buf, subname) != NULL) {
      return platform_ids[i];
    }
  }

  fprintf(stderr, "did not find any OpenCL platform with subname '%s'\n", subname);
  exit(EXIT_FAILURE);
}

static
Context createContextWithIDs(cl_platform_id platform_id, cl_device_id device_id) {
  char name_buf[NAME_BUF_SIZE];
  size_t name_size;
  oclFatalError(clGetPlatformInfo(platform_id, CL_PLATFORM_NAME, sizeof(name_buf), name_buf, &name_size),
    "could not get platform info");
  if (name_size > sizeof(name_buf)) {
    fprintf(stderr, "did not expect such a long OpenCL platform name (%zu)\n", name_size);
    name_buf[NAME_BUF_SIZE - 1] = '\0';
  }
  fprintf(stderr, "using OpenCL platform '%s'\n", name_buf);

  oclFatalError(clGetDeviceInfo(device_id, CL_DEVICE_NAME, sizeof(name_buf), name_buf, &name_size),
    "could not get device info");
  if (name_size > sizeof(name_buf)) {
    fprintf(stderr, "did not expect such a long OpenCL device name (%zu)\n", name_size);
    name_buf[NAME_BUF_SIZE - 1] = '\0';
  }
  fprintf(stderr, "using OpenCL device '%s'\n", name_buf);

  cl_int err;
  const cl_context_properties ctx_props[] = {
    CL_CONTEXT_PLATFORM, (cl_context_properties)(platform_id),
    0
  };
  cl_context ctx = clCreateContext(ctx_props, 1, &device_id, NULL, NULL, &err);
  oclFatalError(err, "could not create context");

  // 2.0: clCreateCommandQueueWithProperties
  cl_command_queue queue = clCreateCommandQueue(ctx, device_id, CL_QUEUE_PROFILING_ENABLE, &err);
  oclFatalError(err, "could not create command queue");

  Context context = malloc(sizeof(struct ContextImpl));
  context->inner = ctx;
  context->platform = platform_id;
  context->device = device_id;
  context->queue = queue;
  return context;
}

Context createDefaultContext() {
  // NULL platform does not work with the AMD APP SDK
  // we take the first returned platform as the default
  cl_platform_id platform_id;
  cl_uint platform_count = 0;
  oclFatalError(clGetPlatformIDs(1, &platform_id, &platform_count),
    "could not get platform IDs");
  if (platform_count == 0) {
    fprintf(stderr, "did not find any OpenCL platform\n");
    exit(EXIT_FAILURE);
  }

  cl_device_id device_id;
  cl_uint device_count = 0;
  // using CL_DEVICE_TYPE_DEFAULT does not work with the AMD APP SDK
  // we take the first returned device as the default instead
  oclFatalError(clGetDeviceIDs(platform_id, CL_DEVICE_TYPE_ALL, 1, &device_id, &device_count),
    "could not get device IDs");
  if (device_count == 0) {
    fprintf(stderr, "did not find any OpenCL device\n");
    exit(EXIT_FAILURE);
  }

  return createContextWithIDs(platform_id, device_id);
}

Context createContext(const char* platform_subname, const char* device_type_str) {
  cl_device_type device_type;
  if (strcmp(device_type_str, "cpu") == 0) {
    device_type = CL_DEVICE_TYPE_CPU;
  } else if (strcmp(device_type_str, "gpu") == 0) {
    device_type = CL_DEVICE_TYPE_GPU;
  } else if (strcmp(device_type_str, "any") == 0) {
    device_type = CL_DEVICE_TYPE_ALL;
  } else {
    fprintf(stderr, "unexpected device type string: %s\n", device_type_str);
    exit(EXIT_FAILURE);
  }

  const cl_uint platform_entries = 256;
  cl_platform_id platform_ids[platform_entries];
  cl_uint platform_count = 0;
  oclFatalError(clGetPlatformIDs(platform_entries, platform_ids, &platform_count),
   "could not get platform IDs");
  if (platform_count > platform_entries) {
    fprintf(stderr, "did not expect that many OpenCL platforms (%u)\n", platform_count);
    platform_count = platform_entries;
  }

  cl_platform_id platform_id = find_platform(platform_count, platform_ids, platform_subname);

  const cl_uint device_entries = 1;
  cl_uint device_count = 0;
  cl_device_id device_id;
  oclFatalError(clGetDeviceIDs(platform_id, device_type, device_entries, &device_id, &device_count),
    "could not get device IDs");
  if (device_count == 0) {
    fprintf(stderr, "did not find any OpenCL device\n");
    exit(EXIT_FAILURE);
  }

  return createContextWithIDs(platform_id, device_id);
}

void destroyContext(Context ctx) {
  oclReportError(clReleaseCommandQueue(ctx->queue), "release queue");
  oclReportError(clReleaseContext(ctx->inner), "release context");
  free(ctx);
}

void waitFinished(Context ctx) {
  oclFatalError(clFinish(ctx->queue), "waiting for commands");
}

Kernel loadKernelFromSource(Context ctx, const char* name, const char* source, size_t length) {
  cl_int err;
  const char* sources[] = { source };
  const size_t lengths[] = { length };
  cl_program program = clCreateProgramWithSource(ctx->inner, 1, sources, lengths, &err);
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

  Kernel k = malloc(sizeof(struct KernelImpl));
  k->program = program;
  k->inner = kernel;
  return k;
}

Kernel loadKernelFromFile(Context ctx, const char* name, const char* path) {
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

  Kernel k = loadKernelFromSource(ctx, name, source, length);
  free(source);
  return k;
}

void launchKernel(
  Context ctx, Kernel k,
  const size_t* global_size, const size_t* local_size,
  size_t arg_count, const KernelArg* args
) {
  for (int i = 0; i < arg_count; i++) {
    clSetKernelArg(k->inner, i, args[i].size, args[i].value);
  }
  oclFatalError(
    clEnqueueNDRangeKernel(ctx->queue, k->inner, 3, NULL, global_size, local_size, 0, NULL, NULL),
    "could not launch kernel"
  );
}

void destroyKernel(Context ctx, Kernel k) {
  oclReportError(clReleaseKernel(k->inner), "kernel release");
  oclReportError(clReleaseProgram(k->program), "program release");
  free(k);
}
