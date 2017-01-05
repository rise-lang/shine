#define __NO_STD_VECTOR // Use cl::vector instead of STL version
#define __CL_ENABLE_EXCEPTIONS
#include <CL/cl.hpp>
#include <algorithm>
#include <utility>
#include <iostream>
#include <fstream>
#include <string>
#include <vector>
using namespace cl;

bool ends_with(const std::string& value, const std::string& ending) {
  if (ending.size() > value.size()) return false;
  return std::equal(ending.rbegin(), ending.rend(), value.rbegin());
}
 
int main(int argc, char** argv) {
    // Create the two input vectors
    const int size = 1024*1024*16;
    const int outputSize = size / 2048;

    std::vector<float> x(size);
    std::vector<float> z(outputSize);
    for(int i = 0; i < size; i++) {
      x[i] = (float)((i % 5) + 1);
    }
 
   try {
        if (argc != 4) {
          std::cout << "Usage 'asum kernelFile platformId deviceId'\n";
          std::abort();
        }

        int platformId = std::atoi(argv[2]);
        int deviceId = std::atoi(argv[3]);
        // Get available platforms
        vector<Platform> platforms;
        Platform::get(&platforms);
 
        // Select the default platform and create a context using this platform and the GPU
        cl_context_properties cps[3] = { 
            CL_CONTEXT_PLATFORM, 
            (cl_context_properties)(platforms[platformId])(), 
            0 
        };
        Context context( CL_DEVICE_TYPE_GPU, cps);
 
        // Get a list of devices on this platform
        vector<Device> devices = context.getInfo<CL_CONTEXT_DEVICES>();
 
        // Create a command queue and use the first device
        CommandQueue queue = CommandQueue(context, devices[deviceId], CL_QUEUE_PROFILING_ENABLE);
 
        // Read source file
        std::string filename {argv[1]};
        std::ifstream file {filename};
        std::string sourceCode {std::istreambuf_iterator<char>(file), std::istreambuf_iterator<char>()};

        Program::Sources source(1, std::make_pair(sourceCode.c_str(), sourceCode.length()+1));
 
        // Make program of the source code in the context
        Program program = Program(context, source);
 
        // Build program for these specific devices
        program.build(devices);
 
        // Make kernel
        Kernel kernel(program, "KERNEL");
 
        // Create memory buffers
        Buffer input = Buffer(context, CL_MEM_READ_ONLY, size * sizeof(float));
        Buffer output = Buffer(context, CL_MEM_WRITE_ONLY, outputSize * sizeof(float));

        std::vector<int> times(10);

        for (int i = 0; i < 10; ++i) {
        // Copy lists A and B to the memory buffers
        queue.enqueueWriteBuffer(input, CL_TRUE, 0, size * sizeof(float), x.data());
 
        // Set arguments to kernel
        if (filename == "amdNvidiaDerived2.cl") {
            kernel.setArg(0, output);
            kernel.setArg(1, input);
            kernel.setArg(2, LocalSpaceArg{256});
            kernel.setArg(3, LocalSpaceArg{256});
            kernel.setArg(4, LocalSpaceArg{256});
            kernel.setArg(5, LocalSpaceArg{4});
            kernel.setArg(6, size);
        } else if (ends_with(filename, "-old.cl")) {
            kernel.setArg(0, input);
            kernel.setArg(1, output);
            kernel.setArg(2, size);
        } else {
            kernel.setArg(0, output);
            kernel.setArg(1, input);
            kernel.setArg(2, size);
        }
 
        // Run the kernel on specific ND range
        cl::Event event;
        queue.enqueueNDRangeKernel(kernel, NullRange, {size}, {128}, NULL, &event);
 
        // Read buffer C into a local list
        queue.enqueueReadBuffer(output, CL_TRUE, 0, outputSize * sizeof(int), z.data());

        times[i] = event.getProfilingInfo<CL_PROFILING_COMMAND_END>()
                 - event.getProfilingInfo<CL_PROFILING_COMMAND_START>();
        }

        std::sort(times.begin(), times.end());

        std::cout << filename << "\n";
        std::cout << "Platform: " << platforms[platformId].getInfo<CL_PLATFORM_NAME>() << "\n";
        std::cout << "Device: " << devices[deviceId].getInfo<CL_DEVICE_NAME>() << "\n";
        std::cout << "Min: " << times.front() * 1e-06 << " ms\n";
        std::cout << "Max: " << times.back() * 1e-06 << " ms\n";
        std::cout << "Median: " << times[times.size()/2] * 1e-06 << " ms\n";
 
    } catch(Error error) {
       std::cout << error.what() << "(" << error.err() << ")" << std::endl;
    }
 
    return 0;
}

