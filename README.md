# `bitrep` - Bit-reproducible math functions

[Original source by Andrea Arteaga](https://github.com/andyspiros/bitrep),
ported to GPU through OpenACC or OpenMP and with a Fortran interface.

Bit-reproducible results have been tested on:

- CPU x86 Intel vs GPU NVIDIA V100 @ [Olympe (CALMIP)][Olympe]:
  - OpenACC & PGI 19.10 compiler
  - OpenACC & NVIDIA HPC SDK 22.7 compiler
- CPU ARM vs GPU NVIDIA A100 @ [Turpan (CALMIP)][Turpan]:
  - OpenACC/OpenMP & NVIDIA HPC SDK 25.3 compiler
- NVIDIA GH200 superchip @ [Kairos (CALMIP)][Kairos]:
  - OpenACC/OpenMP & NVIDIA HPC SDK 26.1 compiler
- CPU AMD vs GPU AMD MI250X @ [Adastra (CINES)][Adastra]:
  - OpenMP & AMD Flang 19.0 compiler

[Olympe]: https://www.calmip.univ-toulouse.fr/espace-utilisateurs/doc-technique-olympe
[Turpan]: https://www.calmip.univ-toulouse.fr/espace-utilisateurs/doc-technique-turpan
[Kairos]: https://www.calmip.univ-toulouse.fr/espace-utilisateurs/documentation-technique-kairos
[Adastra]: https://dci.dci-gitlab.cines.fr/webextranet/index.html

## Benchmark

On a transcendental-heavy kernel `sin + cos + exp + log + atan` per point
(see [`./tests/test_perf.f90`](./tests/test_perf.f90)),
the cost to pay for bit-reproducibility seems to be a **~3.5 to 4.5× slowdown** (NVHPC, 25+).

<details><summary>Benchmark result on Kairos, NVIDIA HPC SDK 26.1</summary>

```console
[reynier@kairosgh0 bitrep]$ ./build/test_perf
[INFO] Number of OpenACC devices detected: 1
[INFO] Device kind (`acc_device_kind` code): 4
libcupti.so not found
--------------------------------------------------
Native  :        0.016 ms
Bitrep  :        0.073 ms
Overhead:         4.56x

Accelerator Kernel Timing data
/users/p18043/reynier/code/hpc/bitrep/tests/test_perf.f90
  test_perf  NVIDIA  devicenum=0
    time(us): 56
    30: data region reached 2 times
        30: data copyin transfers: 1
             device time(us): total=56 max=56 min=56 avg=56
/users/p18043/reynier/code/hpc/bitrep/tests/test_perf.f90
  run_native  NVIDIA  devicenum=0
    time(us): 0
    75: compute region reached 23 times
        75: kernel launched 23 times
            grid: [7813]  block: [128]
            elapsed time(us): total=382 max=40 min=15 avg=16
    75: data region reached 46 times
/users/p18043/reynier/code/hpc/bitrep/tests/test_perf.f90
  run_bitrep  NVIDIA  devicenum=0
    time(us): 0
    89: compute region reached 23 times
        89: kernel launched 23 times
            grid: [7813]  block: [128]
            elapsed time(us): total=1,655 max=74 min=70 avg=71
    89: data region reached 46 times
/users/p18043/reynier/code/hpc/bitrep/tests/test_perf.f90
  anti_dce_kernel  NVIDIA  devicenum=0
    time(us): 19
    103: compute region reached 1 time
        103: kernel launched 1 time
            grid: [7813]  block: [128]
            elapsed time(us): total=17 max=17 min=17 avg=17
        103: reduction kernel launched 1 time
            grid: [1]  block: [256]
            elapsed time(us): total=17 max=17 min=17 avg=17
    103: data region reached 4 times
        103: data copyin transfers: 1
             device time(us): total=3 max=3 min=3 avg=3
        112: data copyout transfers: 1
             device time(us): total=16 max=16 min=16 avg=16

```
</details>

## Build instructions

> [!NOTE]
> `bitrep` can be built as a static library with the CMake build system.
> However, it's probably easier to copy the two source files (see `./src`) in your project.

The following will build a static library `libbitrep.a` and a test binary that will test
if identical result are obtained both on the CPU and the GPU (see `./tests/test_bitrep.f90`).

```bash
cmake -B build -S . -DCMAKE_VERBOSE_MAKEFILE=ON -DOFFLOAD_MODEL=OPENACC # -DCUDA_CC=70
cd build
./test_bitrep
# Or with SLURM:
# srun -n 1 --gres:gpu=1 --part=shared ./test_bitrep
# Output:
# GPU::sin         vs CPU::sin         are NOT identical but within tolerance
# GPU::br_sin      vs CPU::br_sin      are identical
# CPU::sin         vs CPU::br_sin      are NOT identical but within tolerance
# GPU::cos         vs CPU::cos         are NOT identical but within tolerance
# GPU::br_cos      vs CPU::br_cos      are identical
# CPU::cos         vs CPU::br_cos      are NOT identical but within tolerance
# GPU::exp         vs CPU::exp         are NOT identical but within tolerance
# GPU::br_exp      vs CPU::br_exp      are identical
# CPU::exp         vs CPU::br_exp      are NOT identical but within tolerance
```

On NVHPC, specifying the CUDA compute capability with `-DCUDA_CC=<cc>` (e.g. `70` for Volta GPUs)
may be necessary when building fails to generate code for all capabilities.

On AMD's OpenMP, target architecture must be specified with `-DAMD_GPU_ARCH=<arch>`
(e.g. `gfx90a` for MI250X).

## Other implementations

- [Original source by Andrea Arteaga](https://github.com/andyspiros/bitrep)
- [GPU OpenACC port of transcendental functions by Philippe Wautelet](https://github.com/pmarguinaud/bitrep)

## Reference

```bibtex
@inproceedings{Arteaga2014Bitrep,
  author = {Arteaga, Andrea and Fuhrer, Oliver and Hoefler, Torsten},
  booktitle = {2014 IEEE 28th International Parallel and Distributed Processing Symposium},
  title = {Designing Bit-Reproducible Portable High-Performance Applications},
  year = {2014},
  pages = {1235-1244},
  keywords = {determinism, reproducibility, parallelism, IEEE-754 standard},
  doi = {10.1109/IPDPS.2014.127},
  url = {https://doi.org/10.1109/IPDPS.2014.127}
}
```
