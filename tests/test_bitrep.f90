program test_bitrep

  use, intrinsic :: iso_fortran_env, only: dp => real64, int64
  use openacc, only: acc_device_kind, acc_device_host, acc_get_device_type, acc_get_num_devices
  use bitrep, only: br_sin, br_cos, br_exp, br_log, br_atan

  implicit none

  integer, parameter       :: n = 10000
  real(kind=dp), parameter :: tol = 1.0e-13_dp
  logical                  :: gpu_available, all_identical, all_equivalent
  real(kind=dp), parameter :: x1_sin = -5._dp, x1_cos = -5._dp, x1_exp = -5._dp, x1_log = 0.001_dp, x1_atan = -5._dp
  real(kind=dp), parameter :: x2_sin = +5._dp, x2_cos = +5._dp, x2_exp = +5._dp, x2_log = 1000._dp, x2_atan = +5._dp
  real(kind=dp)            :: x1, x2
  real(kind=dp)            :: x_sin(n), x_cos(n), x_exp(n), x_log(n), x_atan(n)
  real(kind=dp)            :: y_sin(n, 4), y_cos(n, 4), y_exp(n, 4), y_log(n, 4), y_atan(n, 4)
  integer                  :: i

  x_sin = 0._dp; y_sin = 0._dp

  call check_gpu_available(gpu_available)
  if (.not. gpu_available) then
    write (*, "(A)") "[INFO] No GPU device detected: comparing OG CPU vs bitrep CPU functions"
  end if

  write (*, "(A,E12.4)") "[INFO] tol = ", tol

  do i = 1, n
    x2 = real(i - 1, dp)/real(n - 1, dp)
    x1 = 1._dp - x2
    x_sin(i) = x1_sin*x1 + x2_sin*x2
    x_cos(i) = x1_cos*x1 + x2_cos*x2
    x_exp(i) = x1_exp*x1 + x2_exp*x2
    x_log(i) = x1_log*x1 + x2_log*x2
    x_atan(i) = x1_atan*x1 + x2_atan*x2
  end do

  !$acc data copyin (x_sin, x_cos, x_exp, x_log, x_atan) &
  !$acc    & copyout(y_sin(:,1:2), y_cos(:,1:2), y_exp(:,1:2), y_atan(:,1:2))

  !$acc serial
  do i = 1, n
    y_sin(i, 1) = sin(x_sin(i))
    y_cos(i, 1) = cos(x_cos(i))
    y_exp(i, 1) = exp(x_exp(i))
    y_log(i, 1) = log(x_log(i))
    y_atan(i, 1) = atan(x_atan(i))
  end do

  do i = 1, n
    y_sin(i, 2) = br_sin(x_sin(i))
    y_cos(i, 2) = br_cos(x_cos(i))
    y_exp(i, 2) = br_exp(x_exp(i))
    y_log(i, 2) = br_log(x_log(i))
    y_atan(i, 2) = br_atan(x_atan(i))
  end do
  !$acc end serial

  !$acc end data

  do i = 1, n
    y_sin(i, 3) = sin(x_sin(i))
    y_cos(i, 3) = cos(x_cos(i))
    y_exp(i, 3) = exp(x_exp(i))
    y_log(i, 3) = log(x_log(i))
    y_atan(i, 3) = atan(x_atan(i))
  end do

  do i = 1, n
    y_sin(i, 4) = br_sin(x_sin(i))
    y_cos(i, 4) = br_cos(x_cos(i))
    y_exp(i, 4) = br_exp(x_exp(i))
    y_log(i, 4) = br_log(x_log(i))
    y_atan(i, 4) = br_atan(x_atan(i))
  end do

  all_identical = .true.
  all_equivalent = .true.

  if (gpu_available) then
    call compare_data(y_sin(:,1), y_sin(:,3), "GPU::sin", "CPU::sin")
    call compare_data(y_sin(:,2), y_sin(:,4), "GPU::br_sin", "CPU::br_sin", critical=.true.)
  end if
  call compare_data(y_sin(:,3), y_sin(:,4), "CPU::sin", "CPU::br_sin")

  if (gpu_available) then
    call compare_data(y_cos(:,1), y_cos(:,3), "GPU::cos", "CPU::cos")
    call compare_data(y_cos(:,2), y_cos(:,4), "GPU::br_cos", "CPU::br_cos", critical=.true.)
  end if
  call compare_data(y_cos(:,3), y_cos(:,4), "CPU::cos", "CPU::br_cos")

  if (gpu_available) then
    call compare_data(y_exp(:,1), y_exp(:,3), "GPU::exp", "CPU::exp")
    call compare_data(y_exp(:,2), y_exp(:,4), "GPU::br_exp", "CPU::br_exp", critical=.true.)
  end if
  call compare_data(y_exp(:,3), y_exp(:,4), "CPU::exp", "CPU::br_exp")

  if (gpu_available) then
    call compare_data(y_log(:,1), y_log(:,3), "GPU::log", "CPU::log")
    call compare_data(y_log(:,2), y_log(:,4), "GPU::br_log", "CPU::br_log", critical=.true.)
  end if
  call compare_data(y_log(:,3), y_log(:,4), "CPU::log", "CPU::br_log")

  if (gpu_available) then
    call compare_data(y_atan(:,1), y_atan(:,3), "GPU::atan", "CPU::atan")
    call compare_data(y_atan(:,2), y_atan(:,4), "GPU::br_atan", "CPU::br_atan", critical=.true.)
  end if
  call compare_data(y_atan(:,3), y_atan(:,4), "CPU::atan", "CPU::br_atan")

  if (.not. gpu_available) then
    write (*, "(A)") "FAILED: No GPU device available"
    error stop 1
  end if

  if (.not. all_identical) then
    write (*, "(A)") "FAILED: at least one GPU/CPU bit-reproductibility check did not pass"
    error stop 1
  end if

  if (.not. all_equivalent) then
    write (*, "(A,E10.1)") "FAILED: at least one original/bitrep tolerance check did not pass."
    error stop 1
  end if

contains

  ! ffmt off
  !> @brief Detect whether a usable GPU is available for OpenACC offload using OpenACC API.
  !>
  !> @param[out] available  `.true.` if at least one non-host OpenACC device is detected.
  ! ffmt on
  subroutine check_gpu_available(available)

    logical, intent(out)     :: available
    integer                  :: num_devices
    integer(acc_device_kind) :: dev_type

    dev_type = acc_get_device_type()
    num_devices = acc_get_num_devices(dev_type)

    available = (num_devices > 0) .and. (dev_type /= acc_device_host)

    write (*, "(A,I0)") "[INFO] Number of OpenACC devices detected: ", num_devices

    if (dev_type == acc_device_host) then
      write (*, "(A)") "[INFO] Device kind: host (CPU fallback, no GPU offload)"
    else
      write (*, "(A,I0)") "[INFO] Device kind (`acc_device_kind` code): ", dev_type
    end if

  end subroutine check_gpu_available

  ! ffmt off
  !> @brief Compare two double precision arrays for bit-level identity and numerical tolerance.
  !>
  !> Perform an element-wise comparison of tow arrays of identical size.
  !> Two comparison criteria are evaluated:
  !>   - **Bit-exact reproducibility**: each element is compared via its raw bit
  !>     representation (using `transfer` to an integer of matching kind), which
  !>     detects any difference, however small, between the two arrays.
  !>   - **Numerical tolerance**: the maximum absolute difference (`max_err`)
  !>     across all elements is compared against the global tolerance `tol`
  !>     (module-level variable).
  !>
  !> Depending on the outcome, one of three states is reported:
  !>   1. Bit-identical: no bit differs between `a` and `b`.
  !>   2. Not bit-identical but within tolerance: some bits differ, but `max_err <= tol`.
  !>   3. Not bit-identical and outside tolerance: `max_err > tol`.
  !>
  !> The module-level flags `all_identical` and `all_equivalent` are updated
  !> accordingly to track the overall pass/fail status across multiple calls.
  !> If `critical` is set to `.true.`, any deviation from bit-exact
  !> reproducibility (even within tolerance) is treated as a failure and forces
  !> `all_identical` to `.false.`.
  !>
  !> @param[in] a          First array to compare (e.g. GPU-computed results).
  !> @param[in] b          Second array to compare (e.g. CPU-computed results).
  !> @param[in] label1     Label identifying array `a` in the diagnostic output.
  !> @param[in] label2     Label identifying array `b` in the diagnostic output.
  !> @param[in] print_diff Optional. If `.true.`, prints every element pair
  !>                       that differs at the bit level. Defaults to `.false.`.
  !> @param[in] critical   Optional. If `.true.`, any bit-level mismatch (even
  !>                       within numerical tolerance) is treated as a critical
  !>                       failure and sets `all_identical` to `.false.`.
  !>                       Defaults to `.false.`.
  ! ffmt on
  subroutine compare_data(a, b, label1, label2, print_diff, critical)

    real(kind=dp), intent(in)     :: a(:), b(:)
    character(len=*), intent(in)  :: label1, label2
    logical, intent(in), optional :: print_diff, critical
    integer(int64)                :: i1, i2
    real(kind=dp)                 :: err, max_err
    logical                       :: identical, critical_id, within_tol, print
    character(len=16)             :: l1, l2

    if (present(print_diff)) then
      print = print_diff
    else
      print = .false.
    end if

    if (present(critical)) then
      critical_id = critical
    else
      critical_id = .false.
    end if

    identical = .true.
    max_err = 0.0_dp
    l1 = adjustl(label1) // repeat(" ", 16 - len_trim(label1))
    l2 = adjustl(label2) // repeat(" ", 16 - len_trim(label2))

    do i = 1, n
      i1 = transfer(a(i), i1)
      i2 = transfer(b(i), i2)
      if (i1 /= i2) then
        if (print) then
          write (*, "(A,A,E30.20,A,E30.20)") l1 // " vs ", l2 // " : ", a(i), " ", b(i)
        end if
        identical = .false.
      end if

      err = abs(a(i) - b(i))
      max_err = max(max_err, err)
    end do

    within_tol = (max_err <= tol)

    if (identical) then
      write (*, "(A)") l1 // " vs " // l2 // " are bit-identical"
    else if (within_tol) then
      write (*, "(A)") l1 // " vs " // l2 // " are NOT bit-identical but within tolerance"
      if (critical_id) all_identical = .false.
    else
      write (*, "(A)") l1 // " vs " // l2 // " are NOT bit-identical"
      all_identical = .false.
      if (critical_id) all_identical = .false.
      all_equivalent = .false.
    end if

  end subroutine compare_data

end program test_bitrep
