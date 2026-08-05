#include <algorithm>
#include <cmath>
#include <cstdint>
#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <cuda_runtime.h>
#include <vector>

#define CUDA_CHECK(call)                                                                                               \
  do {                                                                                                                 \
    cudaError_t err = (call);                                                                                          \
    if (err != cudaSuccess) {                                                                                          \
      std::fprintf(stderr, "[CUDA ERROR] %s:%d: %s\n", __FILE__, __LINE__, cudaGetErrorString(err));                   \
      std::exit(1);                                                                                                    \
    }                                                                                                                  \
  } while (0)

extern "C" {
__device__ __host__ double br_sin(double x);
__device__ __host__ double br_cos(double x);
__device__ __host__ double br_exp(double x);
__device__ __host__ double br_atan(double x);
__device__ __host__ double br_log(double x);
}

constexpr int N = 10000;
constexpr double TOL = 1.0e-13;

static bool all_identical = true;
static bool all_equivalent = true;

__global__ void kernel_native(const double *x_sin, const double *x_cos, const double *x_exp, const double *x_log,
                              const double *x_atan, double *y_sin, double *y_cos, double *y_exp, double *y_log,
                              double *y_atan, int n) {
  int i = blockIdx.x * blockDim.x + threadIdx.x;
  if (i < n) {
    y_sin[i] = sin(x_sin[i]);
    y_cos[i] = cos(x_cos[i]);
    y_exp[i] = exp(x_exp[i]);
    y_log[i] = log(x_log[i]);
    y_atan[i] = atan(x_atan[i]);
  }
}

__global__ void kernel_bitrep(const double *x_sin, const double *x_cos, const double *x_exp, const double *x_log,
                              const double *x_atan, double *y_sin, double *y_cos, double *y_exp, double *y_log,
                              double *y_atan, int n) {
  int i = blockIdx.x * blockDim.x + threadIdx.x;
  if (i < n) {
    y_sin[i] = br_sin(x_sin[i]);
    y_cos[i] = br_cos(x_cos[i]);
    y_exp[i] = br_exp(x_exp[i]);
    y_log[i] = br_log(x_log[i]);
    y_atan[i] = br_atan(x_atan[i]);
  }
}

/// @brief Whether at least one CUDA device is available.
static bool is_gpu_available() {
  int count = 0;
  cudaError_t err = cudaGetDeviceCount(&count);

  std::printf("[INFO] Number of CUDA devices detected: %d\n", (err == cudaSuccess) ? count : 0);

  if (err != cudaSuccess) {
    std::printf("[INFO] cudaGetDeviceCount failed: %s\n", cudaGetErrorString(err));
    return false;
  }

  if (count > 0) {
    cudaDeviceProp prop;
    CUDA_CHECK(cudaGetDeviceProperties(&prop, 0));
    std::printf("[INFO] Device 0: %s (compute capability %d.%d)\n", prop.name, prop.major, prop.minor);
  }

  return count > 0;
}

void dump_bits(const char *name, const double *x, const double *y, int n) {
  char fname[512];
  std::vector<int64_t> xbits(n);
  std::vector<int64_t> ybits(n);
  for (int i = 0; i < n; ++i) {
    std::memcpy(&ybits[i], &y[i], sizeof(double));
    std::memcpy(&xbits[i], &x[i], sizeof(double));
  }

  std::snprintf(fname, sizeof(fname), "x_%s.bin", name);
  FILE *f = std::fopen(fname, "wb");
  std::fwrite(&n, sizeof(int), 1, f);
  std::fwrite(xbits.data(), sizeof(int64_t), n, f);
  std::fclose(f);

  std::snprintf(fname, sizeof(fname), "y_%s.bin", name);
  f = std::fopen(fname, "wb");
  std::fwrite(&n, sizeof(int), 1, f);
  std::fwrite(ybits.data(), sizeof(int64_t), n, f);
  std::fclose(f);
}

/// @brief Compare two double arrays for bit-level identity and numerical tolerance.
///
/// Mirrors the Fortran `compare_data` subroutine: performs bit-exact comparison
/// via reinterpretation to `int64`, and tracks maximum absolute error against `TOL`.
/// Updates the global `all_identical` / `all_equivalent` flags.
static void compare_data(const double *a, const double *b, int n, const char *label1, const char *label2,
                         bool print_diff = false, bool critical = false) {
  bool identical = true;
  double max_err = 0.0;

  for (int i = 0; i < n; ++i) {
    int64_t i1, i2;
    std::memcpy(&i1, &a[i], sizeof(double));
    std::memcpy(&i2, &b[i], sizeof(double));

    if (i1 != i2) {
      if (print_diff) {
        std::printf("%-16s vs %-16s : %30.20e %30.20e\n", label1, label2, a[i], b[i]);
      }
      identical = false;
    }

    double err = std::fabs(a[i] - b[i]);
    max_err = std::max(max_err, err);
  }

  bool within_tol = (max_err <= TOL);

  char l1[32], l2[32];
  std::snprintf(l1, sizeof(l1), "%-16s", label1);
  std::snprintf(l2, sizeof(l2), "%-16s", label2);

  if (identical) {
    std::printf("%s vs %s are bit-identical\n", l1, l2);
  } else if (within_tol) {
    std::printf("%s vs %s are NOT bit-identical but within tolerance\n", l1, l2);
    if (critical)
      all_identical = false;
  } else {
    std::printf("%s vs %s are NOT bit-identical\n", l1, l2);
    all_identical = false;
    if (critical)
      all_identical = false;
    all_equivalent = false;
  }
}

int main() {
  const bool gpu_available = is_gpu_available();
  if (!gpu_available) {
    std::printf("[INFO] No GPU device detected: cannot run CUDA-specific comparisons\n");
  }

  std::printf("[INFO] tol = %e\n", TOL);

  constexpr double x1_sin = -5.0, x1_cos = -5.0, x1_exp = -5.0, x1_log = 0.001, x1_atan = -5.0;
  constexpr double x2_sin = +5.0, x2_cos = +5.0, x2_exp = +5.0, x2_log = 1000.0, x2_atan = +5.0;

  double x_sin[N], x_cos[N], x_exp[N], x_log[N], x_atan[N];

  for (int i = 0; i < N; ++i) {
    double x2 = static_cast<double>(i) / static_cast<double>(N - 1);
    double x1 = 1.0 - x2;
    x_sin[i] = x1_sin * x1 + x2_sin * x2;
    x_cos[i] = x1_cos * x1 + x2_cos * x2;
    x_exp[i] = x1_exp * x1 + x2_exp * x2;
    x_log[i] = x1_log * x1 + x2_log * x2;
    x_atan[i] = x1_atan * x1 + x2_atan * x2;
  }

  // -- GPU computation ------------------------------------------------------------------------------------------------

  double y_sin_native[N], y_cos_native[N], y_exp_native[N], y_log_native[N], y_atan_native[N];
  double y_sin_bitrep[N], y_cos_bitrep[N], y_exp_bitrep[N], y_log_bitrep[N], y_atan_bitrep[N];

  if (gpu_available) {
    double *d_x_sin, *d_x_cos, *d_x_exp, *d_x_log, *d_x_atan;
    double *d_y_sin, *d_y_cos, *d_y_exp, *d_y_log, *d_y_atan;

    size_t bytes = N * sizeof(double);

    CUDA_CHECK(cudaMalloc(&d_x_sin, bytes));
    CUDA_CHECK(cudaMalloc(&d_x_cos, bytes));
    CUDA_CHECK(cudaMalloc(&d_x_exp, bytes));
    CUDA_CHECK(cudaMalloc(&d_x_log, bytes));
    CUDA_CHECK(cudaMalloc(&d_x_atan, bytes));
    CUDA_CHECK(cudaMalloc(&d_y_sin, bytes));
    CUDA_CHECK(cudaMalloc(&d_y_cos, bytes));
    CUDA_CHECK(cudaMalloc(&d_y_exp, bytes));
    CUDA_CHECK(cudaMalloc(&d_y_log, bytes));
    CUDA_CHECK(cudaMalloc(&d_y_atan, bytes));

    CUDA_CHECK(cudaMemcpy(d_x_sin, x_sin, bytes, cudaMemcpyHostToDevice));
    CUDA_CHECK(cudaMemcpy(d_x_cos, x_cos, bytes, cudaMemcpyHostToDevice));
    CUDA_CHECK(cudaMemcpy(d_x_exp, x_exp, bytes, cudaMemcpyHostToDevice));
    CUDA_CHECK(cudaMemcpy(d_x_log, x_log, bytes, cudaMemcpyHostToDevice));
    CUDA_CHECK(cudaMemcpy(d_x_atan, x_atan, bytes, cudaMemcpyHostToDevice));

    int threads = 256;
    int blocks = (N + threads - 1) / threads;

    kernel_native<<<blocks, threads>>>(d_x_sin, d_x_cos, d_x_exp, d_x_log, d_x_atan, d_y_sin, d_y_cos, d_y_exp, d_y_log,
                                       d_y_atan, N);
    CUDA_CHECK(cudaGetLastError());
    CUDA_CHECK(cudaDeviceSynchronize());

    CUDA_CHECK(cudaMemcpy(y_sin_native, d_y_sin, bytes, cudaMemcpyDeviceToHost));
    CUDA_CHECK(cudaMemcpy(y_cos_native, d_y_cos, bytes, cudaMemcpyDeviceToHost));
    CUDA_CHECK(cudaMemcpy(y_exp_native, d_y_exp, bytes, cudaMemcpyDeviceToHost));
    CUDA_CHECK(cudaMemcpy(y_log_native, d_y_log, bytes, cudaMemcpyDeviceToHost));
    CUDA_CHECK(cudaMemcpy(y_atan_native, d_y_atan, bytes, cudaMemcpyDeviceToHost));

    kernel_bitrep<<<blocks, threads>>>(d_x_sin, d_x_cos, d_x_exp, d_x_log, d_x_atan, d_y_sin, d_y_cos, d_y_exp, d_y_log,
                                       d_y_atan, N);
    CUDA_CHECK(cudaGetLastError());
    CUDA_CHECK(cudaDeviceSynchronize());

    CUDA_CHECK(cudaMemcpy(y_sin_bitrep, d_y_sin, bytes, cudaMemcpyDeviceToHost));
    CUDA_CHECK(cudaMemcpy(y_cos_bitrep, d_y_cos, bytes, cudaMemcpyDeviceToHost));
    CUDA_CHECK(cudaMemcpy(y_exp_bitrep, d_y_exp, bytes, cudaMemcpyDeviceToHost));
    CUDA_CHECK(cudaMemcpy(y_log_bitrep, d_y_log, bytes, cudaMemcpyDeviceToHost));
    CUDA_CHECK(cudaMemcpy(y_atan_bitrep, d_y_atan, bytes, cudaMemcpyDeviceToHost));

    CUDA_CHECK(cudaFree(d_x_sin));
    CUDA_CHECK(cudaFree(d_x_cos));
    CUDA_CHECK(cudaFree(d_x_exp));
    CUDA_CHECK(cudaFree(d_x_log));
    CUDA_CHECK(cudaFree(d_x_atan));
    CUDA_CHECK(cudaFree(d_y_sin));
    CUDA_CHECK(cudaFree(d_y_cos));
    CUDA_CHECK(cudaFree(d_y_exp));
    CUDA_CHECK(cudaFree(d_y_log));
    CUDA_CHECK(cudaFree(d_y_atan));
  }

  // -- CPU computation ------------------------------------------------------------------------------------------------

  double y_sin_cpu_native[N], y_cos_cpu_native[N], y_exp_cpu_native[N], y_log_cpu_native[N], y_atan_cpu_native[N];
  double y_sin_cpu_bitrep[N], y_cos_cpu_bitrep[N], y_exp_cpu_bitrep[N], y_log_cpu_bitrep[N], y_atan_cpu_bitrep[N];

  for (int i = 0; i < N; ++i) {
    y_sin_cpu_native[i] = std::sin(x_sin[i]);
    y_cos_cpu_native[i] = std::cos(x_cos[i]);
    y_exp_cpu_native[i] = std::exp(x_exp[i]);
    y_log_cpu_native[i] = std::log(x_log[i]);
    y_atan_cpu_native[i] = std::atan(x_atan[i]);
  }

  for (int i = 0; i < N; ++i) {
    y_sin_cpu_bitrep[i] = br_sin(x_sin[i]);
    y_cos_cpu_bitrep[i] = br_cos(x_cos[i]);
    y_exp_cpu_bitrep[i] = br_exp(x_exp[i]);
    y_log_cpu_bitrep[i] = br_log(x_log[i]);
    y_atan_cpu_bitrep[i] = br_atan(x_atan[i]);
  }

  // -- Comparisons ----------------------------------------------------------------------------------------------------

  if (gpu_available) {
    compare_data(y_sin_native, y_sin_cpu_native, N, "GPU::sin", "CPU::sin");
    compare_data(y_sin_bitrep, y_sin_cpu_bitrep, N, "GPU::br_sin", "CPU::br_sin", true, true);
  }
  compare_data(y_sin_cpu_native, y_sin_cpu_bitrep, N, "CPU::sin", "CPU::br_sin");

  if (gpu_available) {
    compare_data(y_cos_native, y_cos_cpu_native, N, "GPU::cos", "CPU::cos");
    compare_data(y_cos_bitrep, y_cos_cpu_bitrep, N, "GPU::br_cos", "CPU::br_cos", false, true);
  }
  compare_data(y_cos_cpu_native, y_cos_cpu_bitrep, N, "CPU::cos", "CPU::br_cos");

  if (gpu_available) {
    compare_data(y_exp_native, y_exp_cpu_native, N, "GPU::exp", "CPU::exp");
    compare_data(y_exp_bitrep, y_exp_cpu_bitrep, N, "GPU::br_exp", "CPU::br_exp", false, true);
  }
  compare_data(y_exp_cpu_native, y_exp_cpu_bitrep, N, "CPU::exp", "CPU::br_exp");

  if (gpu_available) {
    compare_data(y_log_native, y_log_cpu_native, N, "GPU::log", "CPU::log");
    compare_data(y_log_bitrep, y_log_cpu_bitrep, N, "GPU::br_log", "CPU::br_log", false, true);
  }
  compare_data(y_log_cpu_native, y_log_cpu_bitrep, N, "CPU::log", "CPU::br_log");

  if (gpu_available) {
    compare_data(y_atan_native, y_atan_cpu_native, N, "GPU::atan", "CPU::atan");
    compare_data(y_atan_bitrep, y_atan_cpu_bitrep, N, "GPU::br_atan", "CPU::br_atan", false, true);
  }
  compare_data(y_atan_cpu_native, y_atan_cpu_bitrep, N, "CPU::atan", "CPU::br_atan");

  // -- Dump data ------------------------------------------------------------------------------------------------------

  dump_bits("cos", x_cos, y_cos_cpu_bitrep, N);
  dump_bits("sin", x_sin, y_sin_cpu_bitrep, N);
  dump_bits("exp", x_exp, y_exp_cpu_bitrep, N);
  dump_bits("log", x_log, y_log_cpu_bitrep, N);
  dump_bits("atan", x_atan, y_atan_cpu_bitrep, N);

  // -- Final status ---------------------------------------------------------------------------------------------------

  if (!gpu_available) {
    std::printf("FAILED: No GPU device available\n");
    return 1;
  }

  if (!all_identical) {
    std::printf("FAILED: at least one GPU/CPU bit-reproducibility check did not pass\n");
    return 1;
  }

  if (!all_equivalent) {
    std::printf("FAILED: at least one original/bitrep tolerance check did not pass (tol=%e)\n", TOL);
    return 1;
  }

  return 0;
}
