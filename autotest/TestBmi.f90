!> @brief Tests of the BMI core, run against the simulation in .mf6minsim
!!
!! The simulation is global, so each test runs in its own process.
!<
module TestBmi
  use testdrive, only: error_type, unittest_type, new_unittest, check
  use iso_c_binding, only: c_char, c_double, c_int
  use mf6bmiError, only: BMI_SUCCESS, BMI_FAILURE
  use mf6bmiUtil, only: BMI_LENVERSION, BMI_LENCOMPONENTNAME, &
                        BMI_LENVARADDRESS, BMI_LENEXTENSIONNAME, &
                        BMI_LENVARTYPE
  use BmiTestUtils, only: to_string, to_c, has_member, init, var_index
  use mf6bmi, only: bmi_get_bmi_version, bmi_get_component_name, &
                    bmi_initialize, bmi_update, bmi_finalize, &
                    get_current_time, get_end_time, &
                    get_varset_member_count, get_varset_members, &
                    get_var_index, get_var_type

  implicit none
  private
  public :: collect_bmi

contains

  subroutine collect_bmi(testsuite)
    type(unittest_type), allocatable, intent(out) :: testsuite(:)
    testsuite = [ &
                new_unittest("bmi_version", test_bmi_version), &
                new_unittest("component_name", test_component_name), &
                new_unittest("lifecycle", test_lifecycle), &
                new_unittest("config_file", test_config_file), &
                new_unittest("varset_count", test_varset_count), &
                new_unittest("varset_members", test_varset_members), &
                new_unittest("varset_unknown", test_varset_unknown), &
                new_unittest("var_index", test_var_index), &
                new_unittest("var_index_unknown", test_var_index_unknown), &
                new_unittest("var_index_across_run", test_var_index_across_run) &
                ]
  end subroutine collect_bmi

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

    call check(error, init() == BMI_SUCCESS)
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

  subroutine test_config_file(error)
    type(error_type), allocatable, intent(out) :: error
    character(kind=c_char) :: c_requested(BMI_LENEXTENSIONNAME)
    character(kind=c_char) :: c_supported(BMI_LENEXTENSIONNAME)
    integer(c_int) :: n_supported

    call check(error, &
               bmi_initialize(to_c('mfsim.nam'), 0, c_requested, &
                              n_supported, c_supported) == BMI_SUCCESS)
    if (allocated(error)) return
    call check(error, n_supported == 0)

    call check(error, bmi_finalize() == BMI_SUCCESS)
  end subroutine test_config_file

  subroutine test_varset_count(error)
    type(error_type), allocatable, intent(out) :: error
    integer(c_int) :: n_input, n_output

    call check(error, init() == BMI_SUCCESS)
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

    call check(error, init() == BMI_SUCCESS)
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

    call check(error, init() == BMI_SUCCESS)
    if (allocated(error)) return

    call check(error, &
               get_varset_member_count(to_c('not_a_set'), count) == BMI_FAILURE)
    call check(error, &
               get_varset_members(to_c('not_a_set'), c_names) == BMI_FAILURE)

    call check(error, bmi_finalize() == BMI_SUCCESS)
  end subroutine test_varset_unknown

  subroutine test_var_index(error)
    type(error_type), allocatable, intent(out) :: error
    character(kind=c_char), allocatable :: c_names(:)
    character(kind=c_char) :: c_type(BMI_LENVARTYPE)
    integer(c_int) :: count, i, idx, idx_k11, idx_x
    integer :: start

    call check(error, init() == BMI_SUCCESS)
    if (allocated(error)) return

    ! every variable in the input set has a distinct index
    call check(error, &
               get_varset_member_count(to_c('input'), count) == BMI_SUCCESS)
    if (allocated(error)) return
    allocate (c_names(count * BMI_LENVARADDRESS))
    call check(error, &
               get_varset_members(to_c('input'), c_names) == BMI_SUCCESS)
    if (allocated(error)) return
    do i = 1, count
      start = (i - 1) * BMI_LENVARADDRESS + 1
      call check(error, &
                 get_var_index(c_names(start:start + BMI_LENVARADDRESS - 1), &
                               idx) == BMI_SUCCESS)
      if (allocated(error)) return
      call check(error, idx == i)
      if (allocated(error)) return
    end do

    ! the index refers to the named variable
    idx_k11 = var_index('LEFTMODEL/NPF/K11')
    idx_x = var_index('RIGHTMODEL/X')
    call check(error, idx_k11 > 0 .and. idx_x > 0 .and. idx_k11 /= idx_x)
    call check(error, var_index('LEFTMODEL/NPF/K11') == idx_k11)
    call check(error, get_var_type(idx_k11, c_type) == BMI_SUCCESS)
    if (allocated(error)) return
    call check(error, to_string(c_type, BMI_LENVARTYPE) == 'DOUBLE (5)')

    call check(error, bmi_finalize() == BMI_SUCCESS)
  end subroutine test_var_index

  subroutine test_var_index_unknown(error)
    type(error_type), allocatable, intent(out) :: error
    character(kind=c_char) :: c_type(BMI_LENVARTYPE)
    integer(c_int) :: idx

    call check(error, init() == BMI_SUCCESS)
    if (allocated(error)) return

    call check(error, &
               get_var_index(to_c('NOMODEL/X'), idx) == BMI_FAILURE)
    call check(error, idx == -1)
    call check(error, get_var_type(0, c_type) == BMI_FAILURE)
    call check(error, get_var_type(huge(idx), c_type) == BMI_FAILURE)

    call check(error, bmi_finalize() == BMI_SUCCESS)
  end subroutine test_var_index_unknown

  !> @brief An index taken after initialize is unchanged at the end of the run
  !<
  subroutine test_var_index_across_run(error)
    type(error_type), allocatable, intent(out) :: error
    real(c_double) :: current_time, end_time
    integer(c_int) :: idx

    call check(error, init() == BMI_SUCCESS)
    if (allocated(error)) return
    idx = var_index('LEFTMODEL/X')

    call check(error, get_end_time(end_time) == BMI_SUCCESS)
    call check(error, get_current_time(current_time) == BMI_SUCCESS)
    if (allocated(error)) return
    do while (current_time < end_time)
      call check(error, bmi_update() == BMI_SUCCESS)
      call check(error, get_current_time(current_time) == BMI_SUCCESS)
      if (allocated(error)) return
    end do
    call check(error, var_index('LEFTMODEL/X') == idx)

    call check(error, bmi_finalize() == BMI_SUCCESS)
  end subroutine test_var_index_across_run

end module TestBmi
