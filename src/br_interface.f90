#if defined(BITREP_USE_OPENACC)
#define __BITREP_DEVFUN $acc routine seq
#elif defined(BITREP_USE_OPENMP)
#define __BITREP_DEVFUN $omp declare target
#endif

module bitrep

  use, intrinsic :: iso_c_binding, only: c_double
  use, intrinsic :: iso_fortran_env, only: dp => real64
  implicit none

  private
  public :: br_sin, br_cos, br_exp, br_log, br_atan

contains

  elemental function br_sin(x)
!__BITREP_DEVFUN
    real(kind=dp), intent(in) :: x
    real(kind=dp) :: br_sin

    interface
      pure function br_sin_c(x_c) bind(c, name="br_sin")
!__BITREP_DEVFUN
        import c_double
        implicit none
        real(kind=c_double) :: br_sin_c
        real(kind=c_double), value, intent(in) :: x_c
      end function br_sin_c
    end interface

    br_sin = br_sin_c(real(x, kind=c_double))
  end function br_sin

  elemental function br_cos(x)
!__BITREP_DEVFUN
    real(kind=dp), intent(in) :: x
    real(kind=dp) :: br_cos

    interface
      pure function br_cos_c(x_c) bind(c, name="br_cos")
!__BITREP_DEVFUN
        import c_double
        implicit none
        real(kind=c_double) :: br_cos_c
        real(kind=c_double), value, intent(in) :: x_c
      end function br_cos_c
    end interface

    br_cos = br_cos_c(real(x, kind=c_double))
  end function br_cos

  elemental function br_exp(x)
!__BITREP_DEVFUN
    real(kind=dp), intent(in) :: x
    real(kind=dp) :: br_exp

    interface
      pure function br_exp_c(x_c) bind(c, name="br_exp")
!__BITREP_DEVFUN
        import c_double
        implicit none
        real(kind=c_double) :: br_exp_c
        real(kind=c_double), value, intent(in) :: x_c
      end function br_exp_c
    end interface

    br_exp = br_exp_c(real(x, kind=c_double))
  end function br_exp

  elemental function br_log(x)
!__BITREP_DEVFUN
    real(kind=dp), intent(in) :: x
    real(kind=dp) :: br_log

    interface
      pure function br_log_c(x_c) bind(c, name="br_log")
!__BITREP_DEVFUN
        import c_double
        implicit none
        real(kind=c_double) :: br_log_c
        real(kind=c_double), value, intent(in) :: x_c
      end function br_log_c
    end interface

    br_log = br_log_c(real(x, kind=c_double))
  end function br_log

  elemental function br_atan(x)
!__BITREP_DEVFUN
    real(kind=dp), intent(in) :: x
    real(kind=dp) :: br_atan

    interface
      pure function br_atan_c(x_c) bind(c, name="br_atan")
!__BITREP_DEVFUN
        import c_double
        implicit none
        real(kind=c_double) :: br_atan_c
        real(kind=c_double), value, intent(in) :: x_c
      end function br_atan_c
    end interface

    br_atan = br_atan_c(real(x, kind=c_double))
  end function br_atan

end module bitrep
