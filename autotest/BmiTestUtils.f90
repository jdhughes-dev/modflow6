!> @brief Helpers for the BMI tests: C strings and initialize
!<
module BmiTestUtils
  use iso_c_binding, only: c_char, c_int, c_null_char
  use mf6bmiUtil, only: BMI_LENVARADDRESS, BMI_LENEXTENSIONNAME, strlen, &
                        char_array_to_string
  use mf6bmi, only: bmi_initialize

  implicit none
  private
  public :: to_string, to_c, has_member, init

contains

  !> @brief Convert a null-terminated C string to a Fortran string
  !<
  function to_string(c_string, max_len) result(f_string)
    integer, intent(in) :: max_len
    character(kind=c_char), intent(in) :: c_string(max_len)
    character(len=:), allocatable :: f_string

    f_string = char_array_to_string(c_string, strlen(c_string, max_len))
  end function to_string

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

  !> @brief Initialize the simulation in the working directory
  !!
  !! Requests the named extensions, if any, and returns the supported ones.
  !<
  function init(requested, supported) result(status)
    character(len=*), intent(in), optional :: requested(:)
    character(len=:), allocatable, intent(out), optional :: supported(:)
    integer(c_int) :: status
    character(kind=c_char), allocatable :: c_requested(:), c_supported(:)
    integer(c_int) :: n_requested, n_supported
    integer :: i, j, start, nbuf

    n_requested = 0
    if (present(requested)) n_requested = size(requested)
    nbuf = max(1, n_requested) * BMI_LENEXTENSIONNAME
    allocate (c_requested(nbuf), c_supported(nbuf))
    do i = 1, nbuf
      c_requested(i) = c_null_char
      c_supported(i) = c_null_char
    end do
    do i = 1, n_requested
      start = (i - 1) * BMI_LENEXTENSIONNAME
      do j = 1, len_trim(requested(i))
        c_requested(start + j) = requested(i) (j:j)
      end do
    end do

    status = bmi_initialize(to_c(''), n_requested, c_requested, &
                            n_supported, c_supported)

    if (present(supported)) then
      allocate (character(len=BMI_LENEXTENSIONNAME) :: supported(n_supported))
      do i = 1, n_supported
        start = (i - 1) * BMI_LENEXTENSIONNAME + 1
        supported(i) = &
          to_string(c_supported(start:start + BMI_LENEXTENSIONNAME - 1), &
                    BMI_LENEXTENSIONNAME)
      end do
    end if
  end function init

end module BmiTestUtils
