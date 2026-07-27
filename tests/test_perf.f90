program test_perf

  use, intrinsic :: iso_fortran_env, only: dp => real64, int64
  use bitrep, only: br_sin, br_cos, br_exp, br_log, br_atan

  implicit none

  integer, parameter :: n = 1000000
  integer, parameter :: n_warmup = 3
  integer, parameter :: n_rep = 20
  real(kind=dp)      :: x(n), y(n)
  real(kind=dp)      :: checksum
  integer            :: rep, i
  integer(int64)     :: t_start, t_end, clock_rate
  real(dp)           :: t_native, t_bitrep
  logical            :: gpu_available

  call check_gpu_available(gpu_available)
  if (.not. gpu_available) then
    write (*, "(A)") "[INFO] No GPU device detected: comparing OG CPU vs bitrep CPU functions"
  end if

  do i = 1, n
    x(i) = -5.0_dp + 10.0_dp*real(i - 1, dp)/real(n - 1, dp)
  end do

  call system_clock(count_rate=clock_rate)

#if defined(BITREP_USE_OPENACC)
  !$acc data copyin(x) create(y)
#elif defined(BITREP_USE_OPENMP)
  !$omp target data map(to: x) map(alloc: y)
#endif

  do rep = 1, n_warmup
    call run_native()
    call run_bitrep()
  end do

  t_native = huge(1.0_dp)
  do rep = 1, n_rep
    call system_clock(t_start)
    call run_native()
    call system_clock(t_end)
    t_native = min(t_native, real(t_end - t_start, dp)/real(clock_rate, dp))
  end do

  t_bitrep = huge(1.0_dp)
  do rep = 1, n_rep
    call system_clock(t_start)
    call run_bitrep()
    call system_clock(t_end)
    t_bitrep = min(t_bitrep, real(t_end - t_start, dp)/real(clock_rate, dp))
  end do

  checksum = 0.0_dp
  call anti_dce_kernel()

#if defined(BITREP_USE_OPENACC)
  !$acc end data
#else defined(BITREP_USE_OPENMP)
  !$omp end target data
#endif

  write (*, "(A)") "--------------------------------------------------"
  write (*, "(A,F12.3,A)") "Native  : ", t_native*1.0e3_dp, " ms"
  write (*, "(A,F12.3,A)") "Bitrep  : ", t_bitrep*1.0e3_dp, " ms"
  write (*, "(A,F12.2,A)") "Overhead: ", t_bitrep/t_native, "x"

contains

  subroutine run_native()

#if defined(BITREP_USE_OPENACC)
    !$acc parallel loop present(x, y)
#elif defined(BITREP_USE_OPENMP)
    !$omp target teams distribute parallel do
#endif

    do i = 1, n
      y(i) = sin(x(i)) + cos(x(i)) + exp(x(i)) + log(abs(x(i)) + 0.001_dp) + atan(x(i))
    end do

  end subroutine run_native

  subroutine run_bitrep()

#if defined(BITREP_USE_OPENACC)
    !$acc parallel loop present(x, y)
#elif defined(BITREP_USE_OPENMP)
    !$omp target teams distribute parallel do
#endif

    do i = 1, n
      y(i) = br_sin(x(i)) + br_cos(x(i)) + br_exp(x(i)) + br_log(abs(x(i)) + 0.001_dp) + br_atan(x(i))
    end do

  end subroutine run_bitrep

  subroutine anti_dce_kernel()

#if defined(BITREP_USE_OPENACC)
    !$acc parallel loop present(y) reduction(+:checksum)
#elif defined(BITREP_USE_OPENMP)
    !$omp target teams distribute parallel do reduction(+:checksum)
#endif

    do i = 1, n
      checksum = checksum + y(i)
    end do

  end subroutine anti_dce_kernel

  ! ffmt off
  !> @brief Detect whether a usable GPU is available for offload using OpenACC/OpenMP API.
  !>
  !> @param[out] available  `.true.` if at least one non-host offload device is detected.
  ! ffmt on
  subroutine check_gpu_available(available)

#if defined(BITREP_USE_OPENACC)
    use openacc, only: acc_device_kind, acc_device_host, acc_get_device_type, acc_get_num_devices
#elif defined(BITREP_USE_OPENMP)
    use omp_lib, only: omp_get_num_devices
#endif

    logical, intent(out) :: available
    integer              :: num_devices

#if defined(BITREP_USE_OPENACC)
    integer(acc_device_kind) :: dev_type

    dev_type = acc_get_device_type()
    num_devices = acc_get_num_devices(dev_type)
    available = (num_devices > 0)  ! .and. (dev_type /= acc_device_host)

    write (*, "(A,I0)") "[INFO] Number of OpenACC devices detected: ", num_devices
    if (dev_type == acc_device_host) then
      write (*, "(A)") "[INFO] Device kind: host (CPU fallback, no GPU offload)"
    else
      write (*, "(A,I0)") "[INFO] Device kind (`acc_device_kind` code): ", dev_type
    end if

#elif defined(BITREP_USE_OPENMP)
    num_devices = omp_get_num_devices()
    available = (num_devices > 0)

    write (*, "(A,I0)") "[INFO] Number of OpenMP target devices detected: ", num_devices
    if (.not. available) then
      write (*, "(A)") "[INFO] Device kind: host (CPU fallback, no GPU offload)"
    end if
#else
    available = .false.
    write (*, "(A)") "[INFO] Built without OpenACC/OpenMP support: CPU fallback, no GPU offload"
#endif

  end subroutine check_gpu_available

end program test_perf
