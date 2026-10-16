!> @brief Tests of the 'bmi:pointers' BMI extension, run against .mf6minsim
!<
module TestBmiPointers
  use testdrive, only: error_type, unittest_type, new_unittest, check
  use iso_c_binding, only: c_int, c_char, c_double, c_ptr, c_loc, &
                           c_associated, c_f_pointer, c_f_procpointer
  use KindModule, only: DP, I4B
  use MemoryManagerModule, only: mem_setptr, mem_reallocate, &
                                 mem_set_transient
  use mf6bmiError, only: BMI_SUCCESS, BMI_FAILURE
  use mf6bmiUtil, only: BMI_LENVARADDRESS
  use mf6bmi, only: bmi_finalize, bmi_update, get_value, get_var_nbytes, &
                    get_current_time, get_end_time, &
                    get_varset_member_count, get_varset_members
  use mf6bmiExtension, only: bmi_get_extension, PointersTableType, &
                             POINTERS_TABLE_VERSION
  use BmiTestUtils, only: to_c, init, has_member, var_index

  implicit none
  private
  public :: collect_bmipointers

  abstract interface
    function get_value_ptr_fn(c_var_index, c_arr_ptr) result(bmi_status) &
      bind(C)
      import :: c_char, c_ptr, c_int
      integer(c_int), intent(in) :: c_var_index
      type(c_ptr), intent(inout) :: c_arr_ptr
      integer(c_int) :: bmi_status
    end function get_value_ptr_fn
    function get_var_rank_fn(c_var_index, c_var_rank) result(bmi_status) &
      bind(C)
      import :: c_char, c_int
      integer(c_int), intent(in) :: c_var_index
      integer(c_int), intent(out) :: c_var_rank
      integer(c_int) :: bmi_status
    end function get_var_rank_fn
    function get_var_shape_fn(c_var_index, c_var_shape) result(bmi_status) &
      bind(C)
      import :: c_char, c_int
      integer(c_int), intent(in) :: c_var_index
      integer(c_int), intent(inout) :: c_var_shape(*)
      integer(c_int) :: bmi_status
    end function get_var_shape_fn
  end interface

  character(len=*), parameter :: K11_ADDRESS = 'LEFTMODEL/NPF/K11' !< a stable variable
  character(len=*), parameter :: HEAD_ADDRESS = 'LEFTMODEL/X' !< the left model heads

contains

  subroutine collect_bmipointers(testsuite)
    type(unittest_type), allocatable, intent(out) :: testsuite(:)
    testsuite = [ &
                new_unittest("negotiate", test_negotiate), &
                new_unittest("not_requested", test_not_requested), &
                new_unittest("table", test_table), &
                new_unittest("value_ptr", test_value_ptr), &
                new_unittest("stable_across_run", test_stable_across_run), &
                new_unittest("varsets", test_varsets), &
                new_unittest("realloc_stable", test_realloc_stable), &
                new_unittest("realloc_transient", test_realloc_transient), &
                new_unittest("realloc_not_requested", &
                             test_realloc_not_requested) &
                ]
  end subroutine collect_bmipointers

  !> @brief Initialize with 'bmi:pointers' and return its table
  !<
  subroutine init_pointers(error, table)
    type(error_type), allocatable, intent(out) :: error
    type(PointersTableType), pointer, intent(out) :: table
    type(c_ptr) :: c_table

    table => null()
    call check(error, &
               init([character(len=12) :: 'bmi:pointers']) == BMI_SUCCESS)
    if (allocated(error)) return
    call check(error, &
               bmi_get_extension(to_c('bmi:pointers'), c_table) == BMI_SUCCESS)
    if (allocated(error)) return
    call c_f_pointer(c_table, table)
  end subroutine init_pointers

  !> @brief Get a pointer to a rank 1 double variable through the table
  !<
  subroutine table_ptr_dbl1d(error, table, address, values)
    type(error_type), allocatable, intent(out) :: error
    type(PointersTableType), pointer, intent(in) :: table
    character(len=*), intent(in) :: address
    real(c_double), pointer, intent(out) :: values(:)
    procedure(get_value_ptr_fn), pointer :: get_value_ptr
    procedure(get_var_rank_fn), pointer :: get_var_rank
    procedure(get_var_shape_fn), pointer :: get_var_shape
    type(c_ptr) :: c_arr
    integer(c_int) :: rank, var_shape(1)

    values => null()
    call c_f_procpointer(table%get_value_ptr, get_value_ptr)
    call c_f_procpointer(table%get_var_rank, get_var_rank)
    call c_f_procpointer(table%get_var_shape, get_var_shape)
    call check(error, get_var_rank(var_index(address), rank) == BMI_SUCCESS)
    if (allocated(error)) return
    call check(error, rank == 1)
    call check(error, &
               get_var_shape(var_index(address), var_shape) == BMI_SUCCESS)
    call check(error, get_value_ptr(var_index(address), c_arr) == BMI_SUCCESS)
    if (allocated(error)) return
    call c_f_pointer(c_arr, values, var_shape)
  end subroutine table_ptr_dbl1d

  !> @brief Check that the pointer holds the same values as get_value
  !<
  subroutine check_matches_copy(error, address, values)
    type(error_type), allocatable, intent(out) :: error
    character(len=*), intent(in) :: address
    real(c_double), pointer, intent(in) :: values(:)
    real(c_double), allocatable, target :: copy(:)
    type(c_ptr) :: c_copy
    integer(c_int) :: nbytes
    integer :: i

    call check(error, get_var_nbytes(var_index(address), nbytes) == BMI_SUCCESS)
    if (allocated(error)) return
    allocate (copy(nbytes / 8))
    call check(error, size(copy) == size(values))
    if (allocated(error)) return
    c_copy = c_loc(copy)
    call check(error, get_value(var_index(address), c_copy) == BMI_SUCCESS)
    if (allocated(error)) return
    do i = 1, size(copy)
      call check(error, copy(i) == values(i))
      if (allocated(error)) return
    end do
  end subroutine check_matches_copy

  subroutine test_negotiate(error)
    type(error_type), allocatable, intent(out) :: error
    character(len=:), allocatable :: supported(:)

    call check(error, &
               init([character(len=12) :: 'bmi:pointers', 'xmi'], &
                    supported) == BMI_SUCCESS)
    if (allocated(error)) return
    call check(error, size(supported) == 2)
    if (allocated(error)) return
    call check(error, supported(1) == 'bmi:pointers')
    call check(error, supported(2) == 'xmi')

    call check(error, bmi_finalize() == BMI_SUCCESS)
  end subroutine test_negotiate

  subroutine test_not_requested(error)
    type(error_type), allocatable, intent(out) :: error
    type(c_ptr) :: c_table
    integer(c_int) :: count

    call check(error, init() == BMI_SUCCESS)
    if (allocated(error)) return
    call check(error, &
               bmi_get_extension(to_c('bmi:pointers'), c_table) == BMI_FAILURE)
    call check(error,.not. c_associated(c_table))
    call check(error, &
               get_varset_member_count(to_c('bmi:pointers:stable'), count) &
               == BMI_FAILURE)

    call check(error, bmi_finalize() == BMI_SUCCESS)
  end subroutine test_not_requested

  subroutine test_table(error)
    type(error_type), allocatable, intent(out) :: error
    type(PointersTableType), pointer :: table

    call init_pointers(error, table)
    if (allocated(error)) return
    call check(error, table%version == POINTERS_TABLE_VERSION)
    call check(error, table%nfunctions == 6)

    call check(error, bmi_finalize() == BMI_SUCCESS)
  end subroutine test_table

  subroutine test_value_ptr(error)
    type(error_type), allocatable, intent(out) :: error
    type(PointersTableType), pointer :: table
    real(c_double), pointer :: values(:)

    call init_pointers(error, table)
    if (allocated(error)) return
    call table_ptr_dbl1d(error, table, K11_ADDRESS, values)
    if (allocated(error)) return
    call check(error, size(values) == 5)
    call check_matches_copy(error, K11_ADDRESS, values)
    if (allocated(error)) return

    call check(error, bmi_finalize() == BMI_SUCCESS)
  end subroutine test_value_ptr

  !> @brief A stable pointer taken after initialize tracks the run
  !<
  subroutine test_stable_across_run(error)
    type(error_type), allocatable, intent(out) :: error
    type(PointersTableType), pointer :: table
    real(c_double), pointer :: head(:), head_now(:)
    real(c_double) :: current_time, end_time

    call init_pointers(error, table)
    if (allocated(error)) return
    call table_ptr_dbl1d(error, table, HEAD_ADDRESS, head)
    if (allocated(error)) return

    call check(error, get_end_time(end_time) == BMI_SUCCESS)
    call check(error, get_current_time(current_time) == BMI_SUCCESS)
    if (allocated(error)) return
    do while (current_time < end_time)
      call check(error, bmi_update() == BMI_SUCCESS)
      call check(error, get_current_time(current_time) == BMI_SUCCESS)
      if (allocated(error)) return
    end do

    call table_ptr_dbl1d(error, table, HEAD_ADDRESS, head_now)
    if (allocated(error)) return
    call check(error, c_associated(c_loc(head), c_loc(head_now)))
    call check(error, head(1) == 1.0_DP)
    call check_matches_copy(error, HEAD_ADDRESS, head)
    if (allocated(error)) return

    call check(error, bmi_finalize() == BMI_SUCCESS)
  end subroutine test_stable_across_run

  subroutine test_varsets(error)
    type(error_type), allocatable, intent(out) :: error
    type(PointersTableType), pointer :: table
    character(kind=c_char), allocatable :: c_names(:)
    integer(c_int) :: n_input, n_stable, n_transient

    call init_pointers(error, table)
    if (allocated(error)) return
    call check(error, &
               get_varset_member_count(to_c('input'), n_input) == BMI_SUCCESS)
    call check(error, &
               get_varset_member_count(to_c('bmi:pointers:stable'), &
                                       n_stable) == BMI_SUCCESS)
    call check(error, &
               get_varset_member_count(to_c('bmi:pointers:transient'), &
                                       n_transient) == BMI_SUCCESS)
    if (allocated(error)) return
    call check(error, n_stable + n_transient == n_input)

    allocate (c_names(max(1, n_stable) * BMI_LENVARADDRESS))
    call check(error, &
               get_varset_members(to_c('bmi:pointers:stable'), c_names) &
               == BMI_SUCCESS)
    if (allocated(error)) return
    call check(error, has_member(c_names, n_stable, K11_ADDRESS))

    call check(error, bmi_finalize() == BMI_SUCCESS)
  end subroutine test_varsets

  !> @brief Reallocating a stable variable terminates (meson should_fail)
  !<
  subroutine test_realloc_stable(error)
    type(error_type), allocatable, intent(out) :: error
    type(PointersTableType), pointer :: table
    real(DP), dimension(:), pointer, contiguous :: k

    call init_pointers(error, table)
    if (allocated(error)) return
    call mem_setptr(k, 'K11', 'LEFTMODEL/NPF')
    call mem_reallocate(k, size(k) + 1, 'K11', 'LEFTMODEL/NPF')

    ! not reached: the memory manager terminates the run
    call check(error, bmi_finalize() == BMI_SUCCESS)
  end subroutine test_realloc_stable

  subroutine test_realloc_transient(error)
    type(error_type), allocatable, intent(out) :: error
    type(PointersTableType), pointer :: table
    real(DP), dimension(:), pointer, contiguous :: k
    character(kind=c_char), allocatable :: c_names(:)
    integer(c_int) :: n_transient

    call init_pointers(error, table)
    if (allocated(error)) return
    call mem_set_transient('K11', 'LEFTMODEL/NPF')
    call mem_setptr(k, 'K11', 'LEFTMODEL/NPF')
    call mem_reallocate(k, size(k) + 1, 'K11', 'LEFTMODEL/NPF')
    call check(error, size(k) == 6)

    call check(error, &
               get_varset_member_count(to_c('bmi:pointers:transient'), &
                                       n_transient) == BMI_SUCCESS)
    if (allocated(error)) return
    allocate (c_names(max(1, n_transient) * BMI_LENVARADDRESS))
    call check(error, &
               get_varset_members(to_c('bmi:pointers:transient'), c_names) &
               == BMI_SUCCESS)
    call check(error, has_member(c_names, n_transient, K11_ADDRESS))

    call check(error, bmi_finalize() == BMI_SUCCESS)
  end subroutine test_realloc_transient

  subroutine test_realloc_not_requested(error)
    type(error_type), allocatable, intent(out) :: error
    real(DP), dimension(:), pointer, contiguous :: k

    call check(error, init() == BMI_SUCCESS)
    if (allocated(error)) return
    call mem_setptr(k, 'K11', 'LEFTMODEL/NPF')
    call mem_reallocate(k, size(k) + 1, 'K11', 'LEFTMODEL/NPF')
    call check(error, size(k) == 6)

    call check(error, bmi_finalize() == BMI_SUCCESS)
  end subroutine test_realloc_not_requested

end module TestBmiPointers
