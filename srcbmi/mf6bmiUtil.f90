!> @brief This module contains helper routines and parameters for the MODFLOW 6 BMI
!!
!<
module mf6bmiUtil
  use mf6bmiError
  use iso_c_binding, only: c_int, c_char, c_null_char
  use ConstantsModule, only: MAXCHARLEN, LENMEMPATH, LENVARNAME, &
                             LENMODELNAME, LINELENGTH, LENMEMTYPE, &
                             LENMEMADDRESS, LENCOMPONENTNAME
  use KindModule, only: DP, I4B, LGP
  use MessageModule, only: write_message
  use SimVariablesModule, only: istdout
  use MemoryHelperModule, only: split_mem_address, split_mem_path
  use HashTableModule, only: HashTableType, hash_table_cr, hash_table_da
  implicit none

  ! the following exported parameters will trigger annoying warnings with
  ! the Intel Fortran compiler (4049,4217). We know that these can be ignored:
  !
  ! https://community.intel.com/t5/Intel-Fortran-Compiler/suppress-linker-warnings/td-p/855137
  ! https://community.intel.com/t5/Intel-Fortran-Compiler/Locally-Defined-Symbol-Imported/m-p/900805
  !
  ! and gfortran does so anyway. They have been disabled in the linker config.

  integer(I4B), parameter :: LENGRIDTYPE = 16 !< max length for Fortran grid type string
  integer(I4B), parameter :: LENUNITS = 16 !< max length for Fortran units string
  integer(I4B), parameter :: LENVARLOCATION = 16 !< max length for Fortran variable location string

  integer(c_int), bind(C, name="BMI_LENVARTYPE") :: BMI_LENVARTYPE = &
                                                    LENMEMTYPE + 1 !< max. length for variable type C-strings
  !DIR$ ATTRIBUTES DLLEXPORT :: BMI_LENVARTYPE

  integer(c_int), bind(C, name="BMI_LENGRIDTYPE") :: BMI_LENGRIDTYPE = &
                                                     LENGRIDTYPE + 1 !< max. length for grid type C-strings
  !DIR$ ATTRIBUTES DLLEXPORT :: BMI_LENGRIDTYPE

  integer(c_int), bind(C, name="BMI_LENUNITS") :: BMI_LENUNITS = &
                                                  LENUNITS + 1 !< max. length for units C-strings
  !DIR$ ATTRIBUTES DLLEXPORT :: BMI_LENUNITS

  integer(c_int), bind(C, name="BMI_LENVARLOCATION") :: BMI_LENVARLOCATION = &
                                                        LENVARLOCATION + 1 !< max. length for variable location C-strings
  !DIR$ ATTRIBUTES DLLEXPORT :: BMI_LENVARLOCATION

  integer(c_int), bind(C, name="BMI_LENVARADDRESS") :: BMI_LENVARADDRESS = &
                                                       LENMEMADDRESS + 1 !< max. length for the variable's address C-string
  !DIR$ ATTRIBUTES DLLEXPORT :: BMI_LENVARADDRESS

  integer(c_int), bind(C, name="BMI_LENCOMPONENTNAME") :: BMI_LENCOMPONENTNAME = &
                                                          256 !< component name length, i.e. 'MODFLOW 6'
  !DIR$ ATTRIBUTES DLLEXPORT :: BMI_LENCOMPONENTNAME

  integer(c_int), bind(C, name="BMI_LENVERSION") :: BMI_LENVERSION = 256 !< length of version string, e.g. '6.3.1' or '6.4.1-dev'
  !DIR$ ATTRIBUTES DLLEXPORT :: BMI_LENVERSION

  integer(c_int), bind(C, name="BMI_LENSETNAME") :: BMI_LENSETNAME = 256 !< max. length for variable set name C-strings
  !DIR$ ATTRIBUTES DLLEXPORT :: BMI_LENSETNAME

  integer(c_int), bind(C, name="BMI_LENEXTENSIONNAME") :: &
    BMI_LENEXTENSIONNAME = 256 !< max. length for extension name C-strings
  !DIR$ ATTRIBUTES DLLEXPORT :: BMI_LENEXTENSIONNAME

  character(len=*), parameter :: EXT_XMI = 'xmi' !< name of the XMI extension
  character(len=*), parameter :: EXT_POINTERS = 'bmi:pointers' !< name of the pointers extension
  logical(LGP) :: xmi_enabled = .false. !< true when xmi was negotiated at initialize
  logical(LGP) :: pointers_enabled = .false. !< true when bmi:pointers was negotiated at initialize

  character(len=LENMEMADDRESS), dimension(:), allocatable :: &
    index_addresses !< variable address for each variable index
  integer(I4B) :: nindices = 0 !< number of variable indices assigned
  type(HashTableType), pointer :: index_map => null() !< variable index for each address

contains

  !> @brief Enable the requested extensions that are supported
  !!
  !! Both name lists are flat buffers with stride BMI_LENEXTENSIONNAME;
  !! @p c_supported must hold at least @p n_requested names.
  !<
  subroutine negotiate_extensions(n_requested, c_requested, n_supported, &
                                  c_supported)
    ! -- dummy variables
    integer(c_int), intent(in) :: n_requested !< number of requested extensions
    character(kind=c_char), intent(in) :: c_requested(*) !< requested extension names
    integer(c_int), intent(out) :: n_supported !< number of supported extensions
    character(kind=c_char), intent(inout) :: c_supported(*) !< supported extension names
    ! -- local variables
    character(len=:), allocatable :: extension_name
    integer(I4B) :: i, istart, iend

    xmi_enabled = .false.
    pointers_enabled = .false.
    n_supported = 0
    do i = 1, n_requested
      istart = (i - 1) * BMI_LENEXTENSIONNAME + 1
      iend = i * BMI_LENEXTENSIONNAME
      extension_name = &
        char_array_to_string(c_requested(istart:iend), &
                             strlen(c_requested(istart:iend), &
                                    BMI_LENEXTENSIONNAME))
      select case (extension_name)
      case (EXT_XMI)
        if (.not. xmi_enabled) then
          xmi_enabled = .true.
          call add_name(EXT_XMI)
        end if
      case (EXT_POINTERS)
        if (.not. pointers_enabled) then
          pointers_enabled = .true.
          call add_name(EXT_POINTERS)
        end if
      end select
    end do

  contains

    subroutine add_name(name)
      character(len=*), intent(in) :: name
      integer(I4B) :: j, offset

      offset = n_supported * BMI_LENEXTENSIONNAME
      do j = 1, len(name)
        c_supported(offset + j) = name(j:j)
      end do
      c_supported(offset + len(name) + 1) = c_null_char
      n_supported = n_supported + 1
    end subroutine add_name

  end subroutine negotiate_extensions

  !> @brief Assign an index to every variable in the memory manager
  !!
  !! Indices follow the memory manager order at the end of initialize;
  !! variables created later are appended when first looked up, so an
  !! index never changes meaning.
  !<
  subroutine create_var_indices()
    ! -- modules
    use MemoryManagerModule, only: memorystore
    use MemoryContainerIteratorModule, only: MemoryContainerIteratorType
    use MemoryTypeModule, only: MemoryType
    use MemoryHelperModule, only: create_mem_address
    ! -- local variables
    type(MemoryContainerIteratorType), allocatable :: itr
    type(MemoryType), pointer :: mt
    integer(I4B) :: idx

    call clear_var_indices()
    call hash_table_cr(index_map)
    allocate (index_addresses(max(1, memorystore%count())))

    itr = memorystore%iterator()
    do while (itr%has_next())
      call itr%next()
      mt => itr%value()
      idx = add_var_index(create_mem_address(mt%path, mt%name))
    end do

  end subroutine create_var_indices

  !> @brief Release the variable indices
  !<
  subroutine clear_var_indices()
    if (associated(index_map)) then
      call hash_table_da(index_map)
    end if
    if (allocated(index_addresses)) then
      deallocate (index_addresses)
    end if
    nindices = 0
  end subroutine clear_var_indices

  !> @brief Give the variable address the next index
  !<
  function add_var_index(var_address) result(idx)
    ! -- dummy variables
    character(len=*), intent(in) :: var_address !< full address of a variable
    integer(I4B) :: idx !< the new index
    ! -- local variables
    character(len=LENMEMADDRESS), dimension(:), allocatable :: tmp
    integer(I4B) :: i

    if (nindices == size(index_addresses)) then
      allocate (tmp(2 * nindices))
      do i = 1, nindices
        tmp(i) = index_addresses(i)
      end do
      call move_alloc(tmp, index_addresses)
    end if
    nindices = nindices + 1
    idx = nindices
    index_addresses(idx) = var_address
    call index_map%add(trim(var_address), idx)

  end function add_var_index

  !> @brief Get the index of an existing variable, assigning one if new
  !<
  function find_var_index(var_address) result(idx)
    ! -- dummy variables
    character(len=*), intent(in) :: var_address !< full address of a variable
    integer(I4B) :: idx !< the variable index

    idx = index_map%get(trim(var_address))
    if (idx == 0) then
      idx = add_var_index(var_address)
    end if

  end function find_var_index

  !> @brief Get the address of the variable with the given index
  !<
  subroutine index_to_address(c_var_index, var_address, success)
    ! -- dummy variables
    integer(c_int), intent(in) :: c_var_index !< index of the variable
    character(len=LENMEMADDRESS), intent(out) :: var_address !< full address of the variable
    logical(LGP), intent(out) :: success !< false when invalid

    success = (c_var_index >= 1 .and. c_var_index <= nindices)
    if (success) then
      var_address = index_addresses(c_var_index)
    else
      var_address = ''
      write (bmi_last_error, fmt_invalid_var_index) c_var_index
      call report_bmi_error(bmi_last_error)
    end if

  end subroutine index_to_address

  !> @brief Get the memory path and name of the variable with the given index
  !<
  subroutine split_index(c_var_index, mem_path, var_name, success)
    ! -- dummy variables
    integer(c_int), intent(in) :: c_var_index !< index of the variable
    character(len=LENMEMPATH), intent(out) :: mem_path !< memory path used by the memory manager
    character(len=LENVARNAME), intent(out) :: var_name !< name of the variable
    logical(LGP), intent(out) :: success !< false when invalid
    ! -- local variables
    character(len=LENMEMADDRESS) :: var_address
    logical(LGP) :: valid, found

    mem_path = ''
    var_name = ''
    call index_to_address(c_var_index, var_address, success)
    if (.not. success) return

    success = .false.
    call split_mem_address(var_address, mem_path, var_name, valid)
    if (.not. valid) then
      write (bmi_last_error, fmt_invalid_var) trim(var_address)
      call report_bmi_error(bmi_last_error)
      return
    end if

    ! the variable may have been released since the index was assigned
    call check_mem_address(mem_path, var_name, found)
    if (.not. found) then
      write (bmi_last_error, fmt_unknown_var) trim(var_name), trim(mem_path)
      call report_bmi_error(bmi_last_error)
      return
    end if

    success = .true.

  end subroutine split_index

  !> @brief Split the variable address string
  !!
  !! Splits the full address string into a memory path and variable name,
  !! following the rules used by the memory manager. The error handling
  !! is inside to avoid lots of duplication
  !<
  subroutine split_address(c_var_address, mem_path, var_name, success)
    ! -- modules
    use MemoryHelperModule, only: memPathSeparator
    ! -- dummy variables
    character(kind=c_char), intent(in) :: c_var_address(*) !< full address of a variable
    character(len=LENMEMPATH), intent(out) :: mem_path !< memory path used by the memory manager
    character(len=LENVARNAME), intent(out) :: var_name !< name of the variable
    logical(LGP), intent(out) :: success !< false when invalid
    ! -- local variables
    character(len=LENMEMADDRESS) :: var_address
    logical(LGP) :: valid, found

    success = .false.

    ! try and split the address string:
    var_address = char_array_to_string(c_var_address, &
                                       strlen(c_var_address, LENMEMADDRESS + 1))
    call split_mem_address(var_address, mem_path, var_name, valid)
    if (.not. valid) then
      write (bmi_last_error, fmt_invalid_var) trim(var_address)
      call report_bmi_error(bmi_last_error)
      return
    end if

    ! check if the variable even exists:
    call check_mem_address(mem_path, var_name, found)
    if (.not. found) then
      write (bmi_last_error, fmt_unknown_var) trim(var_name), trim(mem_path)
      call report_bmi_error(bmi_last_error)
      return
    end if

    success = .true.

  end subroutine split_address

  !> @brief Check if the variable exists in the memory manager
  !<
  subroutine check_mem_address(mem_path, var_name, found)
    ! -- modules
    use MemoryManagerModule, only: get_from_memorystore
    use MemoryTypeModule, only: MemoryType
    ! -- dummy variables
    character(len=LENMEMPATH), intent(in) :: mem_path !< memory path used by the memory manager
    character(len=LENVARNAME), intent(in) :: var_name !< name of the variable
    logical(LGP), intent(out) :: found !< true when found
    ! -- local variables
    type(MemoryType), pointer :: mt

    found = .false.
    mt => null()

    ! check = false: otherwise stop is called when the variable does not exist
    call get_from_memorystore(var_name, mem_path, mt, found, check=.false.)

  end subroutine check_mem_address

  !> @brief Returns the string length without the trailing null character
  !<
  pure function strlen(char_array, max_len) result(string_length)
    ! -- dummy variables
    integer(I4B), intent(in) :: max_len
    character(c_char), intent(in) :: char_array(max_len) !< C-style character string
    integer(I4B) :: string_length !< Fortran string length
    ! -- local variables
    integer(I4B) :: i

    string_length = 0
    do i = 1, size(char_array)
      if (char_array(i) .eq. C_NULL_CHAR) then
        string_length = i - 1
        exit
      end if
    end do

  end function strlen

  !> @brief Convert C-style string to Fortran character string
  !<
  pure function char_array_to_string(char_array, length) result(f_string)
    ! -- dummy variables
    integer(c_int), intent(in) :: length !< string length without terminating null character
    character(c_char), intent(in) :: char_array(length) !< string to convert
    character(len=length) :: f_string !< Fortran fixed length character string
    ! -- local variables
    integer(I4B) :: i

    do i = 1, length
      f_string(i:i) = char_array(i)
    end do

  end function char_array_to_string

  !> @brief Convert Fortran string to C-style character string
  !<
  pure function string_to_char_array(string, length) result(c_array)
    ! -- dummy variables
    integer(c_int), intent(in) :: length !< Fortran string length
    character(len=length), intent(in) :: string !< string to convert
    character(kind=c_char, len=1) :: c_array(length + 1) !< C-style character string
    ! -- local variables
    integer(I4B) :: i

    do i = 1, length
      c_array(i) = string(i:i)
    end do
    c_array(length + 1) = C_NULL_CHAR

  end function string_to_char_array

  !> @brief Extract the model name from a memory address string
  !<
  function extract_model_name(var_address, success) result(model_name)
    ! -- dummy variables
    character(len=*), intent(in) :: var_address !< the memory address for the variable
    character(len=LENMODELNAME) :: model_name !< the extracted model name
    logical(LGP), intent(out) :: success
    ! -- local variables
    character(len=LENMEMPATH) :: mem_path
    character(len=LENCOMPONENTNAME) :: dummy_component
    character(len=LENVARNAME) :: var_name
    logical(LGP) :: split_succeeded

    success = .false.

    call split_mem_address(var_address, mem_path, var_name, split_succeeded)
    if (.not. split_succeeded) then
      return
    end if

    call split_mem_path(mem_path, model_name, dummy_component)
    success = .true.

  end function extract_model_name

  !> @brief Get the model name from the grid id
  !<
  function get_model_name(grid_id) result(model_name)
    ! -- modules
    use ListsModule, only: basemodellist
    use BaseModelModule, only: BaseModelType, GetBaseModelFromList
    ! -- dummy variables
    integer(kind=c_int), intent(in) :: grid_id !< grid id
    character(len=LENMODELNAME) :: model_name !< model name
    ! -- local variables
    integer(I4B) :: i
    class(BaseModelType), pointer :: baseModel
    character(len=LINELENGTH) :: error_msg

    model_name = ''

    do i = 1, basemodellist%Count()
      baseModel => GetBaseModelFromList(basemodellist, i)
      if (baseModel%id == grid_id) then
        model_name = baseModel%name
        return
      end if
    end do

    write (error_msg, '(a,i0)') 'BMI error: no model for grid id ', grid_id
    call write_message(error_msg, iunit=istdout, skipbefore=1, skipafter=1)
  end function get_model_name

  !> @brief Get the solution object for this index
  !<
  function getSolution(subcomponent_idx) result(solution)
    ! -- modules
    use SolutionGroupModule
    use BaseSolutionModule, only: BaseSolutionType, GetBaseSolutionFromList
    use ListsModule, only: basesolutionlist, solutiongrouplist
    ! -- dummy variables
    integer(I4B), intent(in) :: subcomponent_idx !< index of solution
    class(BaseSolutionType), pointer :: solution !< Base Solution
    ! -- local variables
    class(SolutionGroupType), pointer :: sgp
    integer(I4B) :: solutionIdx

    ! this is equivalent to how it's done in sgp_ca
    sgp => GetSolutionGroupFromList(solutiongrouplist, 1)
    solutionIdx = sgp%idsolutions(subcomponent_idx)
    solution => GetBaseSolutionFromList(basesolutionlist, solutionIdx)
  end function getSolution

  !> @brief Get the grid type for a named model as a fortran string
  !<
  subroutine get_grid_type_model(model_name, grid_type_f)
    ! -- modules
    use ListsModule, only: basemodellist
    use BaseModelModule, only: BaseModelType, GetBaseModelFromList
    ! -- dummy variables
    character(len=LENMODELNAME) :: model_name
    character(len=LENGRIDTYPE) :: grid_type_f
    ! -- local variables
    integer(I4B) :: i
    class(BaseModelType), pointer :: baseModel

    do i = 1, basemodellist%Count()
      baseModel => GetBaseModelFromList(basemodellist, i)
      if (baseModel%name == model_name) then
        call baseModel%dis%get_dis_type(grid_type_f)
        return
      end if
    end do
  end subroutine get_grid_type_model

  !> @brief Confirm that grid is of an expected type
  !<
  function confirm_grid_type(grid_id, expected_type) result(is_match)
    ! -- dummy variables
    integer(kind=c_int), intent(in) :: grid_id
    character(len=*), intent(in) :: expected_type
    logical :: is_match
    ! -- local variables
    character(len=LENMODELNAME) :: model_name
    character(len=LENGRIDTYPE) :: grid_type

    is_match = .false.

    model_name = get_model_name(grid_id)
    call get_grid_type_model(model_name, grid_type)
    ! careful comparison:
    if (expected_type == grid_type) is_match = .true.
  end function confirm_grid_type

end module mf6bmiUtil
