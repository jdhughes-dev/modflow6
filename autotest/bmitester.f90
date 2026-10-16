!> @brief Run one BMI test; the simulation is global, so one test per process
!<
program bmitester
  use, intrinsic :: iso_fortran_env, only: error_unit
  use testdrive, only: new_testsuite, testsuite_type, select_suite, &
    & run_selected, get_argument
  use TestBmiCore, only: collect_bmicore
  implicit none
  integer :: stat, is
  character(len=:), allocatable :: suite_name, test_name
  type(testsuite_type), allocatable :: testsuites(:)
  character(len=*), parameter :: fmt = '("#", *(1x, a))'

  stat = 0
  testsuites = [ &
               new_testsuite("BmiCore", collect_bmicore) &
               ]

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

end program bmitester
