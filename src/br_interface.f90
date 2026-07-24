module bitrep

  use, intrinsic :: iso_c_binding, only: c_double
  use, intrinsic :: iso_fortran_env, only: dp => real64
  implicit none

  private
  public :: br_sin, br_cos, br_exp, br_log, br_atan

contains

  elemental function br_sin(x)
    !$omp declare target
    !$acc routine seq
    real(kind=dp), intent(in) :: x
    real(kind=dp) :: br_sin

    interface
      pure function br_sin_c(x_c) bind(c, name="br_sin")
        !$omp declare target
        !$acc routine seq
        import c_double
        implicit none
        real(kind=c_double) :: br_sin_c
        real(kind=c_double), value, intent(in) :: x_c
      end function br_sin_c
    end interface

    br_sin = br_sin_c(real(x, kind=c_double))
  end function br_sin

  elemental function br_cos(x)
    !$omp declare target
    !$acc routine seq
    real(kind=dp), intent(in) :: x
    real(kind=dp) :: br_cos

    interface
      pure function br_cos_c(x_c) bind(c, name="br_cos")
        !$omp declare target
        !$acc routine seq
        import c_double
        implicit none
        real(kind=c_double) :: br_cos_c
        real(kind=c_double), value, intent(in) :: x_c
      end function br_cos_c
    end interface

    br_cos = br_cos_c(real(x, kind=c_double))
  end function br_cos

  elemental function br_exp(x)
    !$omp declare target
    !$acc routine seq
    real(kind=dp), intent(in) :: x
    real(kind=dp) :: br_exp

    interface
      pure function br_exp_c(x_c) bind(c, name="br_exp")
        !$omp declare target
        !$acc routine seq
        import c_double
        implicit none
        real(kind=c_double) :: br_exp_c
        real(kind=c_double), value, intent(in) :: x_c
      end function br_exp_c
    end interface

    br_exp = br_exp_c(real(x, kind=c_double))
  end function br_exp

  elemental function br_log(x)
    !$omp declare target
    !$acc routine seq
    real(kind=dp), intent(in) :: x
    real(kind=dp) :: br_log

    interface
      pure function br_log_c(x_c) bind(c, name="br_log")
        !$omp declare target
        !$acc routine seq
        import c_double
        implicit none
        real(kind=c_double) :: br_log_c
        real(kind=c_double), value, intent(in) :: x_c
      end function br_log_c
    end interface

    br_log = br_log_c(real(x, kind=c_double))
  end function br_log

  elemental function br_atan(x)
    !$omp declare target
    !$acc routine seq
    real(kind=dp), intent(in) :: x
    real(kind=dp) :: br_atan

    interface
      pure function br_atan_c(x_c) bind(c, name="br_atan")
        !$omp declare target
        !$acc routine seq
        import c_double
        implicit none
        real(kind=c_double) :: br_atan_c
        real(kind=c_double), value, intent(in) :: x_c
      end function br_atan_c
    end interface

    br_atan = br_atan_c(real(x, kind=c_double))
  end function br_atan

end module bitrep
