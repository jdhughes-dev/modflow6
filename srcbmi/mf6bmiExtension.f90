!> @brief This module contains the BMI extension function tables
!!
!! get_extension() returns a pointer to a C struct of function pointers.
!! Each table starts with its version and the number of function pointers
!! that follow, so functions can only be appended.
!<
module mf6bmiExtension
  use iso_c_binding, only: c_int, c_char, c_ptr, c_funptr, c_funloc, c_loc, &
                           c_null_ptr
  use KindModule, only: I4B
  use mf6bmiError
  use mf6bmiUtil
  use mf6bmi
  use mf6xmi
  implicit none

  integer(c_int), parameter :: XMI_TABLE_VERSION = 1 !< version of XmiTableType
  integer(c_int), parameter :: POINTERS_TABLE_VERSION = 1 !< version of PointersTableType

  !> @brief Function table for the 'xmi' extension
  !<
  type, bind(C) :: XmiTableType
    integer(c_int) :: version !< table version
    integer(c_int) :: nfunctions !< number of function pointers that follow
    type(c_funptr) :: prepare_time_step
    type(c_funptr) :: do_time_step
    type(c_funptr) :: finalize_time_step
    type(c_funptr) :: get_subcomponent_count
    type(c_funptr) :: prepare_solve
    type(c_funptr) :: solve
    type(c_funptr) :: finalize_solve
    type(c_funptr) :: get_version
    type(c_funptr) :: get_var_address
  end type XmiTableType

  !> @brief Function table for the 'bmi:pointers' extension
  !<
  type, bind(C) :: PointersTableType
    integer(c_int) :: version !< table version
    integer(c_int) :: nfunctions !< number of function pointers that follow
    type(c_funptr) :: get_value_ptr
    type(c_funptr) :: get_value_ptr_double
    type(c_funptr) :: get_value_ptr_int
    type(c_funptr) :: get_value_ptr_bool
    type(c_funptr) :: get_var_rank
    type(c_funptr) :: get_var_shape
  end type PointersTableType

  type(XmiTableType), target, save :: xmi_table !< the 'xmi' function table
  type(PointersTableType), target, save :: pointers_table !< the 'bmi:pointers' function table

contains

  !> @brief Get the function table for a negotiated extension
  !!
  !! Fails when the extension is unknown or was not requested at initialize.
  !<
  function bmi_get_extension(c_extension_name, c_table) result(bmi_status) &
    bind(C, name="get_extension")
    !DIR$ ATTRIBUTES DLLEXPORT :: bmi_get_extension
    ! -- dummy variables
    character(kind=c_char), intent(in) :: c_extension_name(*) !< name of the extension
    type(c_ptr), intent(out) :: c_table !< pointer to the function table
    integer(kind=c_int) :: bmi_status !< BMI status code
    ! -- local variables
    character(len=:), allocatable :: extension_name

    extension_name = &
      char_array_to_string(c_extension_name, &
                           strlen(c_extension_name, BMI_LENEXTENSIONNAME))
    c_table = c_null_ptr
    bmi_status = BMI_FAILURE

    select case (extension_name)
    case (EXT_XMI)
      if (xmi_enabled) then
        call fill_xmi_table()
        c_table = c_loc(xmi_table)
        bmi_status = BMI_SUCCESS
      end if
    case (EXT_POINTERS)
      if (pointers_enabled) then
        call fill_pointers_table()
        c_table = c_loc(pointers_table)
        bmi_status = BMI_SUCCESS
      end if
    end select

    if (bmi_status /= BMI_SUCCESS) then
      write (bmi_last_error, fmt_unavailable_ext) trim(extension_name)
      call report_bmi_error(bmi_last_error)
    end if

  end function bmi_get_extension

  !> @brief Fill the 'xmi' function table
  !<
  subroutine fill_xmi_table()
    xmi_table%version = XMI_TABLE_VERSION
    xmi_table%nfunctions = 9
    xmi_table%prepare_time_step = c_funloc(xmi_prepare_time_step)
    xmi_table%do_time_step = c_funloc(xmi_do_time_step)
    xmi_table%finalize_time_step = c_funloc(xmi_finalize_time_step)
    xmi_table%get_subcomponent_count = c_funloc(xmi_get_subcomponent_count)
    xmi_table%prepare_solve = c_funloc(xmi_prepare_solve)
    xmi_table%solve = c_funloc(xmi_solve)
    xmi_table%finalize_solve = c_funloc(xmi_finalize_solve)
    xmi_table%get_version = c_funloc(xmi_get_version)
    xmi_table%get_var_address = c_funloc(get_var_address)
  end subroutine fill_xmi_table

  !> @brief Fill the 'bmi:pointers' function table
  !<
  subroutine fill_pointers_table()
    pointers_table%version = POINTERS_TABLE_VERSION
    pointers_table%nfunctions = 6
    pointers_table%get_value_ptr = c_funloc(get_value_ptr)
    pointers_table%get_value_ptr_double = c_funloc(get_value_ptr_double)
    pointers_table%get_value_ptr_int = c_funloc(get_value_ptr_int)
    pointers_table%get_value_ptr_bool = c_funloc(get_value_ptr_bool)
    pointers_table%get_var_rank = c_funloc(get_var_rank)
    pointers_table%get_var_shape = c_funloc(get_var_shape)
  end subroutine fill_pointers_table

end module mf6bmiExtension
