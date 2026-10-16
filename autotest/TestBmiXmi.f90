!> @brief Tests of the 'xmi' BMI extension, run against .mf6minsim
!<
module TestBmiXmi
  use testdrive, only: error_type, unittest_type, new_unittest, check
  use iso_c_binding, only: c_int, c_double, c_ptr, c_associated, &
                           c_f_pointer, c_f_procpointer
  use mf6bmiError, only: BMI_SUCCESS, BMI_FAILURE
  use mf6bmi, only: bmi_finalize, get_current_time, get_end_time, &
                    get_time_step
  use mf6bmiExtension, only: bmi_get_extension, XmiTableType, &
                             XMI_TABLE_VERSION
  use BmiTestUtils, only: to_c, init

  implicit none
  private
  public :: collect_bmixmi

  abstract interface
    function xmi_dt_fn(dt) result(bmi_status) bind(C)
      import :: c_int, c_double
      real(c_double), intent(in) :: dt
      integer(c_int) :: bmi_status
    end function xmi_dt_fn
    function xmi_fn() result(bmi_status) bind(C)
      import :: c_int
      integer(c_int) :: bmi_status
    end function xmi_fn
    function xmi_count_fn(count) result(bmi_status) bind(C)
      import :: c_int
      integer(c_int), intent(out) :: count
      integer(c_int) :: bmi_status
    end function xmi_count_fn
    function xmi_idx_fn(idx) result(bmi_status) bind(C)
      import :: c_int
      integer(c_int) :: idx
      integer(c_int) :: bmi_status
    end function xmi_idx_fn
    function xmi_solve_fn(idx, has_converged) result(bmi_status) bind(C)
      import :: c_int
      integer(c_int), intent(in) :: idx
      integer(c_int), intent(out) :: has_converged
      integer(c_int) :: bmi_status
    end function xmi_solve_fn
  end interface

contains

  subroutine collect_bmixmi(testsuite)
    type(unittest_type), allocatable, intent(out) :: testsuite(:)
    testsuite = [ &
                new_unittest("negotiate", test_negotiate), &
                new_unittest("not_requested", test_not_requested), &
                new_unittest("unknown", test_unknown), &
                new_unittest("table", test_table), &
                new_unittest("time_loop", test_time_loop) &
                ]
  end subroutine collect_bmixmi

  subroutine test_negotiate(error)
    type(error_type), allocatable, intent(out) :: error
    character(len=:), allocatable :: supported(:)

    call check(error, &
               init([character(len=16) :: 'xmi', 'not_an_extension'], &
                    supported) == BMI_SUCCESS)
    if (allocated(error)) return
    call check(error, size(supported) == 1)
    if (allocated(error)) return
    call check(error, supported(1) == 'xmi')

    call check(error, bmi_finalize() == BMI_SUCCESS)
  end subroutine test_negotiate

  subroutine test_not_requested(error)
    type(error_type), allocatable, intent(out) :: error
    character(len=:), allocatable :: supported(:)
    type(c_ptr) :: c_table

    call check(error, init(supported=supported) == BMI_SUCCESS)
    if (allocated(error)) return
    call check(error, size(supported) == 0)
    call check(error, bmi_get_extension(to_c('xmi'), c_table) == BMI_FAILURE)
    call check(error,.not. c_associated(c_table))

    call check(error, bmi_finalize() == BMI_SUCCESS)
  end subroutine test_not_requested

  subroutine test_unknown(error)
    type(error_type), allocatable, intent(out) :: error
    type(c_ptr) :: c_table

    call check(error, &
               init([character(len=16) :: 'not_an_extension']) == BMI_SUCCESS)
    if (allocated(error)) return
    call check(error, &
               bmi_get_extension(to_c('not_an_extension'), c_table) == &
               BMI_FAILURE)

    call check(error, bmi_finalize() == BMI_SUCCESS)
  end subroutine test_unknown

  subroutine test_table(error)
    type(error_type), allocatable, intent(out) :: error
    type(c_ptr) :: c_table
    type(XmiTableType), pointer :: table

    call check(error, init([character(len=3) :: 'xmi']) == BMI_SUCCESS)
    if (allocated(error)) return
    call check(error, bmi_get_extension(to_c('xmi'), c_table) == BMI_SUCCESS)
    if (allocated(error)) return
    call c_f_pointer(c_table, table)
    call check(error, table%version == XMI_TABLE_VERSION)
    call check(error, table%nfunctions == 9)

    call check(error, bmi_finalize() == BMI_SUCCESS)
  end subroutine test_table

  !> @brief Run the simulation through the 'xmi' table to its end
  !<
  subroutine test_time_loop(error)
    type(error_type), allocatable, intent(out) :: error
    type(c_ptr) :: c_table
    type(XmiTableType), pointer :: table
    procedure(xmi_dt_fn), pointer :: prepare_time_step
    procedure(xmi_fn), pointer :: finalize_time_step
    procedure(xmi_count_fn), pointer :: get_subcomponent_count
    procedure(xmi_idx_fn), pointer :: prepare_solve, finalize_solve
    procedure(xmi_solve_fn), pointer :: solve
    real(c_double) :: current_time, end_time, dt
    integer(c_int) :: nsln, isln, kiter, has_converged

    call check(error, init([character(len=3) :: 'xmi']) == BMI_SUCCESS)
    if (allocated(error)) return
    call check(error, bmi_get_extension(to_c('xmi'), c_table) == BMI_SUCCESS)
    if (allocated(error)) return
    call c_f_pointer(c_table, table)
    call c_f_procpointer(table%prepare_time_step, prepare_time_step)
    call c_f_procpointer(table%finalize_time_step, finalize_time_step)
    call c_f_procpointer(table%get_subcomponent_count, get_subcomponent_count)
    call c_f_procpointer(table%prepare_solve, prepare_solve)
    call c_f_procpointer(table%solve, solve)
    call c_f_procpointer(table%finalize_solve, finalize_solve)

    call check(error, get_end_time(end_time) == BMI_SUCCESS)
    call check(error, get_current_time(current_time) == BMI_SUCCESS)
    if (allocated(error)) return
    do while (current_time < end_time)
      call check(error, get_time_step(dt) == BMI_SUCCESS)
      call check(error, prepare_time_step(dt) == BMI_SUCCESS)
      call check(error, get_subcomponent_count(nsln) == BMI_SUCCESS)
      if (allocated(error)) return
      do isln = 1, nsln
        call check(error, prepare_solve(isln) == BMI_SUCCESS)
        if (allocated(error)) return
        has_converged = 0
        kiter = 0
        do while (has_converged == 0 .and. kiter < 100)
          call check(error, solve(isln, has_converged) == BMI_SUCCESS)
          if (allocated(error)) return
          kiter = kiter + 1
        end do
        call check(error, has_converged == 1)
        call check(error, finalize_solve(isln) == BMI_SUCCESS)
        if (allocated(error)) return
      end do
      call check(error, finalize_time_step() == BMI_SUCCESS)
      call check(error, get_current_time(current_time) == BMI_SUCCESS)
      if (allocated(error)) return
    end do

    call check(error, bmi_finalize() == BMI_SUCCESS)
  end subroutine test_time_loop

end module TestBmiXmi
