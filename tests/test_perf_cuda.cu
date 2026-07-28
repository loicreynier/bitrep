#include <cuda_runtime.h>
#include <math.h>
#include <stdio.h>

#define CUDA_CHECK(expr_to_check)                                                                                      \
  do {                                                                                                                 \
    cudaError_t result = expr_to_check;                                                                                \
    if (result != cudaSuccess) {                                                                                       \
      fprintf(stderr, "CUDA Runtime Error: %s:%i:%d = %s\n", __FILE__, __LINE__, result, cudaGetErrorString(result));  \
    }                                                                                                                  \
  } while (0)

extern "C" {
__device__ double br_sin(double x);
__device__ double br_cos(double x);
__device__ double br_exp(double x);
__device__ double br_atan(double x);
__device__ double br_log(double x);
}

__global__ void bitrepKernel(double *input, double *output, int n) {
  int idx = blockIdx.x * blockDim.x + threadIdx.x;
  double arg;
  if (idx < n) {
    arg = input[idx];
    output[idx] = br_sin(arg) + br_cos(arg) + br_exp(arg) + br_log(abs(arg) + 0.001) + br_atan(arg);
  }
}

__global__ void nativeKernel(double *input, double *output, int n) {
  int idx = blockIdx.x * blockDim.x + threadIdx.x;
  double arg;
  if (idx < n) {
    arg = input[idx];
    output[idx] = sin(arg) + cos(arg) + exp(arg) + log(abs(arg) + 0.001) + atan(arg);
  }
}

volatile double checksum = 0.0; // Anti-DCE

int main() {
  int n = 1000000;
  size_t bytes = n * sizeof(double);
  int n_warmup = 3;
  int n_rep = 20;

  double *x = nullptr;
  double *y = nullptr;
  double *dev_x = nullptr;
  double *dev_y = nullptr;

  cudaMallocHost(&x, bytes);
  cudaMallocHost(&y, bytes);
  cudaMalloc(&dev_x, bytes);
  cudaMalloc(&dev_y, bytes);

  for (int i = 0; i < n; i++) {
    x[i] = -5.0 + 10.0 * (double)(i - 1) / double(n - 1);
  }

  cudaMemcpy(dev_x, x, bytes, cudaMemcpyHostToDevice);
  int blockSize = 256;
  int gridSize = (n + blockSize - 1) / blockSize;

  // Warmup
  for (int i = 0; i < n_warmup; i++) {
    bitrepKernel<<<gridSize, blockSize>>>(dev_x, dev_y, n);
    nativeKernel<<<gridSize, blockSize>>>(dev_x, dev_y, n);
  }
  cudaDeviceSynchronize();
  CUDA_CHECK(cudaGetLastError());

  cudaEvent_t start, stop;
  cudaEventCreate(&start);
  cudaEventCreate(&stop);

  // Bitrep
  cudaEventRecord(start);
  for (int i = 0; i < n_rep; i++) {
    bitrepKernel<<<gridSize, blockSize>>>(dev_x, dev_y, n);
  }
  cudaEventRecord(stop);
  cudaEventSynchronize(stop);

  float ms_bitrep = 0;
  cudaEventElapsedTime(&ms_bitrep, start, stop);

  // Native
  cudaEventRecord(start);
  for (int i = 0; i < n_rep; i++) {
    nativeKernel<<<gridSize, blockSize>>>(dev_x, dev_y, n);
  }
  cudaEventRecord(stop);
  cudaEventSynchronize(stop);

  float ms_native = 0;
  cudaEventElapsedTime(&ms_native, start, stop);

  cudaMemcpy(y, dev_y, bytes, cudaMemcpyDeviceToHost);
  for (int i = 0; i < n; i++) {
    checksum += y[i];
  }

  printf("--------------------------------------------------");
  printf("\nBITREP:\n");
  printf("  Total time: %.3f ms\n", ms_bitrep);
  printf("  Per iteration: %.3f ms\n", ms_bitrep / n_rep);
  printf("  Per element: %.3f ns\n", (ms_bitrep * 1e6) / (n_rep * n));
  printf("\nNATIVE:\n");
  printf("  Total time: %.3f ms\n", ms_native);
  printf("  Per iteration: %.3f ms\n", ms_native / n_rep);
  printf("  Per element: %.3f ns\n", (ms_native * 1e6) / (n_rep * n));
  printf("\nOverhead (native/bitrep): %.2fx\n", ms_bitrep / ms_native);

  cudaEventDestroy(start);
  cudaEventDestroy(stop);

  cudaFreeHost(x);
  cudaFreeHost(y);
  cudaFree(dev_x);
  cudaFree(dev_y);

  return 0;
}
