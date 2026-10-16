!> @brief Tests of the BMI core, run against the simulation in .mf6minsim
!!
!! The simulation is global, so each test runs in its own process.
!<
module TestBmiCore
  use testdrive, only: error_type, unittest_type, new_unittest, check
  use iso_c_binding, only: c_char, c_double, c_int, c_null_char
  use mf6bmiError, only: BMI_SUCCESS, BMI_FAILURE
  use mf6bmiUtil, only: BMI_LENVERSION, BMI_LENCOMPONENTNAME, &
                        BMI_LENVARADDRESS, strlen, char_array_to_string
  use mf6bmi, only: bmi_get_bmi_version, bmi_get_component_name, &
                    bmi_initialize, bmi_update, bmi_finalize, &
                    get_current_time, get_end_time, &
                    get_varset_member_count, get_varset_members

  implicit none
  private
  public :: collect_bmicore

contains

  subroutine collect_bmicore(testsuite)
    type(unittest_type), allocatable, intent(out) :: testsuite(:)
    testsuite = [ &
                new_unittest("bmi_version", test_bmi_version), &
                new_unittest("component_name", test_component_name), &
                new_unittest("lifecycle", test_lifecycle), &
                new_unittest("varset_count", test_varset_count), &
                new_unittest("varset_members", test_varset_members), &
                new_unittest("varset_unknown", test_varset_unknown) &
                ]
  end subroutine collect_bmicore

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

  !> @brief Convert a Fortran string to a null-terminated C string
  !<
  function to_c(f_string) result(c_string)
    character(len=*), intent(in) :: f_string
    character(kind=c_char) :: c_string(len(f_string) + 1)
    integer :: i

    do i = 1, len(f_string)
      c_string(i) = f_string(i:i)
    end do
    c_string(len(f_string) + 1) = c_null_char
  end function to_c

  !> @brief Return true if the flat name buffer holds the address
  !<
  function has_member(c_names, count, address) result(found)
    character(kind=c_char), intent(in) :: c_names(:)
    integer, intent(in) :: count
    character(len=*), intent(in) :: address
    logical :: found
    integer :: i, start

    found = .false.
    do i = 1, count
      start = (i - 1) * BMI_LENVARADDRESS + 1
      if (to_string(c_names(start:start + BMI_LENVARADDRESS - 1), &
                    BMI_LENVARADDRESS) == address) then
        found = .true.
        exit
      end if
    end do
  end function has_member

  subroutine test_varset_count(error)
    type(error_type), allocatable, intent(out) :: error
    integer(c_int) :: n_input, n_output

    call check(error, bmi_initialize() == BMI_SUCCESS)
    if (allocated(error)) return

    call check(error, &
               get_varset_member_count(to_c('input'), n_input) == BMI_SUCCESS)
    call check(error, &
               get_varset_member_count(to_c('output'), n_output) == BMI_SUCCESS)
    if (allocated(error)) return
    call check(error, n_input > 0)
    call check(error, n_input == n_output)

    call check(error, bmi_finalize() == BMI_SUCCESS)
  end subroutine test_varset_count

  subroutine test_varset_members(error)
    type(error_type), allocatable, intent(out) :: error
    character(kind=c_char), allocatable :: c_names(:)
    integer(c_int) :: count

    call check(error, bmi_initialize() == BMI_SUCCESS)
    if (allocated(error)) return

    call check(error, &
               get_varset_member_count(to_c('input'), count) == BMI_SUCCESS)
    if (allocated(error)) return
    allocate (c_names(count * BMI_LENVARADDRESS))
    call check(error, &
               get_varset_members(to_c('input'), c_names) == BMI_SUCCESS)
    if (allocated(error)) return
    call check(error, has_member(c_names, count, 'LEFTMODEL/NPF/K11'))
    call check(error, has_member(c_names, count, 'RIGHTMODEL/X'))
    call check(error,.not. has_member(c_names, count, 'NOMODEL/X'))

    call check(error, bmi_finalize() == BMI_SUCCESS)
  end subroutine test_varset_members

  subroutine test_varset_unknown(error)
    type(error_type), allocatable, intent(out) :: error
    character(kind=c_char) :: c_names(BMI_LENVARADDRESS)
    integer(c_int) :: count

    call check(error, bmi_initialize() == BMI_SUCCESS)
    if (allocated(error)) return

    call check(error, &
               get_varset_member_count(to_c('not_a_set'), count) == BMI_FAILURE)
    call check(error, &
               get_varset_members(to_c('not_a_set'), c_names) == BMI_FAILURE)

    call check(error, bmi_finalize() == BMI_SUCCESS)
  end subroutine test_varset_unknown

end module TestBmiCore
