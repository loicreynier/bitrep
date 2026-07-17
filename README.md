# `bitrep` - Bit-reproductible math functions

[Original source by Andrea Arteaga](https://github.com/andyspiros/bitrep),
ported to GPU through OpenACC and with a Fortran interface.

Bit-reproductible results have been tested on:

- CPU x86 Intel vs GPU NVIDIA V100 @ [Olympe (CALMIP)][Olympe]:
  - PGI 19.10 compiler
  - NVIDIA HPC SDK 22.7 compiler
- CPU ARM vs GPU NVIDIA A100 @ [Turpan (CALMIP)][Turpan]: NVIDIA HPC SDK 25.3 compiler
- NVIDIA GH200 superchip @ [Kairos (CALMIP)][Kairos]: NVIDIA HPC SDK 26.1 compiler

[Olympe]: https://www.calmip.univ-toulouse.fr/espace-utilisateurs/doc-technique-olympe
[Turpan]: https://www.calmip.univ-toulouse.fr/espace-utilisateurs/doc-technique-turpan
[Kairos]: https://www.calmip.univ-toulouse.fr/espace-utilisateurs/documentation-technique-kairos

## Build instructions

> [!NOTE]
> `bitrep` can be built as a static library with the CMake build system.
> However it's probably easier to copy the two source files (see `./src`) in your project.

The following will build a static library `libbitrep.a` and a test binary that will test
if identical result are obtained both on the CPU and the GPU (see `./tests/test_bitrep.f90`).

```bash
cmake -B build -S . -DCMAKE_VERBOSE_MAKEFILE=ON # -DBUILD_TESTINGS=OFF -DCUDA_CC=cc70
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

On NVHPC, specifying the CUDA compute capability (e.g. `cc70` for Volta GPUs) may be necessary
when bulding fails to generate code for all capabilities.

## Other implementations

- [Original source by Andrea Arteaga](https://github.com/andyspiros/bitrep)
- [GPU OpenACC port of transcendental functions by Philippe Wautelet](https://github.com/pmarguinaud/bitrep)

## Reference

```bibtex
@inproceedings{Arteaga2014bitrep,
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
