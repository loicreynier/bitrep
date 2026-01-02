# `bitrep` - Bit-reproductible math functions

[Original source by Andrea Arteaga](https://github.com/andyspiros/bitrep),
ported to GPU through OpenACC and with a Fortran interface.

Bit-reproductible results have been tested on:

- NVHPC 25.3 compiler: CPU ARM vs GPU NVIDIA A100 @ [HPCC Turpan (CALMIP)][Turpan]

[Turpan]: https://www.calmip.univ-toulouse.fr/espace-utilisateurs/doc-technique-turpan

## Build instructions

The following will build a static library `libbitrep.a` and a test binary that will test
if identical result are obtained both on the CPU and the GPU (see `./tests/test_bitrep.f90`).

```bash
cmake -B build -S . -DCMAKE_VERBOSE_MAKEFILE=ON # -DBUILD_TESTINGS=OFF
cd build
./test-bitrep
# Output:
# GPU::sin         vs CPU::sin         are NOT identical
# GPU::br_sin      vs CPU::br_sin      are identical
# CPU::sin         vs CPU::br_sin      are NOT identical
# GPU::cos         vs CPU::cos         are NOT identical
# GPU::br_cos      vs CPU::br_cos      are identical
# CPU::cos         vs CPU::br_cos      are NOT identical
# GPU::exp         vs CPU::exp         are NOT identical
# GPU::br_exp      vs CPU::br_exp      are identical
# CPU::exp         vs CPU::br_exp      are NOT identical
```

## Other implementations

- [Original source by Andrea Arteaga](https://github.com/andyspiros/bitrep)
- [GPU OpenACC port of transcendental functions by Philippe Wautelet](https://github.com/pmarguinaud/bitrep)

## Reference

```bibtex
@INPROCEEDINGS{6877351,
  author={Arteaga, Andrea and Fuhrer, Oliver and Hoefler, Torsten},
  booktitle={2014 IEEE 28th International Parallel and Distributed Processing Symposium},
  title={Designing Bit-Reproducible Portable High-Performance Applications},
  year={2014},
  volume={},
  number={},
  pages={1235-1244},
  keywords={Standards;Libraries;Meteorology;Computer architecture;Accuracy;Computational modeling;Graphics processing units;determinism;reproducibility;parallelism;IEEE-754 standard},
  doi={10.1109/IPDPS.2014.127}}
```
