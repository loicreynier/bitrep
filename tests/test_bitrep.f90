program test_bitrep
  use iso_fortran_env, only: dp => real64, int64
  use bitrep, only: br_sin, br_cos, br_exp, br_log, br_atan
  implicit none

  integer, parameter :: n = 10000

  real(kind=dp), parameter :: x1_sin = -5._dp, x1_cos = -5._dp, x1_exp = -5._dp, x1_log = 0.001_dp, x1_atan = -5._dp
  real(kind=dp), parameter :: x2_sin = +5._dp, x2_cos = +5._dp, x2_exp = +5._dp, x2_log = 1000._dp, x2_atan = +5._dp
  real(kind=dp) :: x1, x2
  real(kind=dp) :: x_sin(n), x_cos(n), x_exp(n), x_log(n), x_atan(n)
  real(kind=dp) :: y_sin(n, 4), y_cos(n, 4), y_exp(n, 4), y_log(n, 4), y_atan(n, 4)

  integer :: i

  x_sin = 0._dp; y_sin = 0._dp

  do i = 1, n
    x2 = real(i - 1, dp) / real(n - 1 / dp)
    x1 = 1._dp - x2
    x_sin(i) = x1_sin * x1 + x2_sin * x2
    x_cos(i) = x1_cos * x1 + x2_cos * x2
    x_exp(i) = x1_exp * x1 + x2_exp * x2
    x_log(i) = x1_log * x1 + x2_log * x2
    x_atan(i) = x1_atan * x1 + x2_atan * x2
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

  call compare_data(y_sin(:, 1), y_sin(:, 3), n, "GPU::sin", "CPU::sin", .false.)
  call compare_data(y_sin(:, 2), y_sin(:, 4), n, "GPU::br_sin", "CPU::br_sin", .false.)
  call compare_data(y_sin(:, 3), y_sin(:, 4), n, "CPU::sin", "CPU::br_sin", .false.)

  call compare_data(y_cos(:, 1), y_cos(:, 3), n, "GPU::cos", "CPU::cos", .false.)
  call compare_data(y_cos(:, 2), y_cos(:, 4), n, "GPU::br_cos", "CPU::br_cos", .false.)
  call compare_data(y_cos(:, 3), y_cos(:, 4), n, "CPU::cos", "CPU::br_cos", .false.)

  call compare_data(y_exp(:, 1), y_exp(:, 3), n, "GPU::exp", "CPU::exp", .false.)
  call compare_data(y_exp(:, 2), y_exp(:, 4), n, "GPU::br_exp", "CPU::br_exp", .false.)
  call compare_data(y_exp(:, 3), y_exp(:, 4), n, "CPU::exp", "CPU::br_exp", .false.)

  call compare_data(y_log(:, 1), y_log(:, 3), n, "GPU::log", "CPU::log", .false.)
  call compare_data(y_log(:, 2), y_log(:, 4), n, "GPU::br_log", "CPU::br_log", .false.)
  call compare_data(y_log(:, 3), y_log(:, 4), n, "CPU::log", "CPU::br_log", .false.)

  call compare_data(y_atan(:, 1), y_atan(:, 3), n, "GPU::atan", "CPU::atan", .false.)
  call compare_data(y_atan(:, 2), y_atan(:, 4), n, "GPU::br_atan", "CPU::br_atan", .false.)
  call compare_data(y_atan(:, 3), y_atan(:, 4), n, "CPU::atan", "CPU::br_atan", .false.)

contains

  subroutine compare_data(a, b, n, label1, label2, print_diff)

    real(kind=dp), intent(in) :: a(:), b(:)
    integer, intent(in) :: n
    character(len=*), intent(in) :: label1, label2
    logical, intent(in) :: print_diff

    integer :: i
    integer(int64) :: i1, i2
    logical :: identical
    character(len=16) :: l1, l2

    identical = .true.
    l1 = adjustl(label1)//repeat(" ", 16 - len_trim(label1))
    l2 = adjustl(label2)//repeat(" ", 16 - len_trim(label2))

    do i = 1, n
      i1 = transfer(a(i), i1)
      i2 = transfer(b(i), i2)
      if (i1 /= i2) then
        if (print_diff) then
          write (*, '(A,A,E30.20,A,E30.20)') l1//" vs ", l2//" : ", a(i), " ", b(i)
        end if
        identical = .false.
      end if
    end do

    if (identical) then
      write (*, '(A,A)') l1//" vs "//l2//" are identical"
    else
      write (*, '(A)') l1//' vs '//l2//' are '//char(27)//'[1;31mNOT'//char(27)//'[0m identical'
    end if

  end subroutine compare_data

end program test_bitrep
