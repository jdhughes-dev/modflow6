!> @brief Tests of the BMI core, run against the simulation in .mf6minsim
!!
!! The simulation is global, so each test runs in its own process.
!<
module TestBmi
  use testdrive, only: error_type, unittest_type, new_unittest, check
  use iso_c_binding, only: c_char, c_double
  use mf6bmiError, only: BMI_SUCCESS
  use mf6bmiUtil, only: BMI_LENVERSION, BMI_LENCOMPONENTNAME, strlen, &
                        char_array_to_string
  use mf6bmi, only: bmi_get_bmi_version, bmi_get_component_name, &
                    bmi_initialize, bmi_update, bmi_finalize, &
                    get_current_time, get_end_time

  implicit none
  private
  public :: collect_bmi

contains

  subroutine collect_bmi(testsuite)
    type(unittest_type), allocatable, intent(out) :: testsuite(:)
    testsuite = [ &
                new_unittest("bmi_version", test_bmi_version), &
                new_unittest("component_name", test_component_name), &
                new_unittest("lifecycle", test_lifecycle) &
                ]
  end subroutine collect_bmi

  !> @brief Convert a null-terminated C string to a Fortran string
  !<
  function to_string(c_string, max_len) result(f_string)
    integer, intent(in) :: max_len
    character(kind=c_char), intent(in) :: c_string(max_len)
    character(len=:), allocatable :: f_string

    f_string = char_array_to_string(c_string, strlen(c_string, max_len))
  end function to_string

  subroutine test_bmi_version(error)
    type(error_type), allocatable, intent(out) :: error
    character(kind=c_char) :: c_version(BMI_LENVERSION)

    call check(error, bmi_get_bmi_version(c_version) == BMI_SUCCESS)
    if (allocated(error)) return
    call check(error, to_string(c_version, BMI_LENVERSION) == '3.0')
  end subroutine test_bmi_version

  subroutine test_component_name(error)
    type(error_type), allocatable, intent(out) :: error
    character(kind=c_char) :: c_name(BMI_LENCOMPONENTNAME)

    call check(error, bmi_get_component_name(c_name) == BMI_SUCCESS)
    if (allocated(error)) return
    call check(error, &
               to_string(c_name, BMI_LENCOMPONENTNAME) == 'MODFLOW 6')
  end subroutine test_component_name

  subroutine test_lifecycle(error)
    type(error_type), allocatable, intent(out) :: error
    real(c_double) :: current_time, end_time

    call check(error, bmi_initialize() == BMI_SUCCESS)
    if (allocated(error)) return

    call check(error, get_end_time(end_time) == BMI_SUCCESS)
    call check(error, get_current_time(current_time) == BMI_SUCCESS)
    if (allocated(error)) return
    do while (current_time < end_time)
      call check(error, bmi_update() == BMI_SUCCESS)
      call check(error, get_current_time(current_time) == BMI_SUCCESS)
      if (allocated(error)) return
    end do

    call check(error, bmi_finalize() == BMI_SUCCESS)
  end subroutine test_lifecycle

end module TestBmi
