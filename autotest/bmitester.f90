!> @brief Run one BMI test; the simulation is global, so one test per process
!<
program bmitester
  use, intrinsic :: iso_fortran_env, only: error_unit
  use testdrive, only: new_testsuite, testsuite_type, select_suite, &
    & run_selected
  use TestBmi, only: collect_bmi
  use TestBmiXmi, only: collect_bmixmi
  use TestBmiPointers, only: collect_bmipointers
  implicit none
  integer :: stat, is
  character(len=:), allocatable :: suite_name, test_name
  type(testsuite_type), allocatable :: testsuites(:)
  character(len=*), parameter :: fmt = '("#", *(1x, a))'

  stat = 0
  testsuites = [ &
               new_testsuite("Bmi", collect_bmi), &
               new_testsuite("BmiXmi", collect_bmixmi), &
               new_testsuite("BmiPointers", collect_bmipointers) &
               ]

  ! read the arguments here rather than with testdrive's get_argument, which
  ! sees none on macOS when test-drive links a different libgfortran
  call get_argument(1, suite_name)
  call get_argument(2, test_name)

  if (.not. (allocated(suite_name) .and. allocated(test_name))) then
    write (error_unit, fmt) "Usage: bmitester SUITE TEST"
    error stop 1
  end if

  is = select_suite(testsuites, suite_name)
  if (is < 1 .or. is > size(testsuites)) then
    write (error_unit, fmt) "Available testsuites"
    do is = 1, size(testsuites)
      write (error_unit, fmt) "-", testsuites(is)%name
    end do
    error stop 1
  end if

  write (error_unit, fmt) "Suite:", testsuites(is)%name
  call run_selected(testsuites(is)%collect, test_name, error_unit, stat)
  if (stat /= 0) then
    error stop 1
  end if

contains

  !> @brief Get a command-line argument, unallocated if it is missing
  !<
  subroutine get_argument(idx, arg)
    integer, intent(in) :: idx
    character(len=:), allocatable, intent(out) :: arg
    integer :: length, stat

    call get_command_argument(idx, length=length, status=stat)
    if (stat /= 0 .or. length == 0) return
    allocate (character(len=length) :: arg)
    call get_command_argument(idx, arg, status=stat)
    if (stat /= 0) deallocate (arg)
  end subroutine get_argument

end program bmitester
