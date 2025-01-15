!> @defgroup group_parameters evaluate_parameters
!> @brief Parameters module
module evaluate_parameters
    use evaluate_kinds
    use evaluate_string
    use evaluate_value
    
    implicit none; private
    
    !> @class param
    !! @ingroup group_parameters
    !! @brief   Provides a simple class for describing equation parameters.
    !! @verbatim type, private :: param @endverbatim
    !! <h2>Examples</h2>
    !! The following example demonstrates some of the main members of the 
    !! @link evaluate_parameters::param param @endlink class.
    !! @n
    !! @code
    !! params(1)%symbol = 'PI'
    !! params(1)%val = (3.14159265358979_r8, 0.0_r8)
    !! params(2)%symbol = 'I'
    !! params(2)%val = (0.0_r8, 1.0_r8)
    !! @endcode
    !! @par
    !! <h2>Constructors</h2>
    !! Initializes a new instance of the @link evaluate_parameters::param param @endlink class
    !! <h3>param(character(24) symbol, complex(c8) val)</h3>
    !! 
    !! @param[in] symbol the name of the parameter
    !! @param[in] value the value of the parameter as complex(c8)
    !! 
    !! @b Examples
    !! @code{.f90}
    !!  type(param) :: p
    !!  p = param('x', (0.0_r8, 0.0_r8))
    !! @endcode
    !! @b Remarks
    type, public :: param
    !! @cond
        character(24)   :: symbol
        complex(c8)     :: val
    !! @endcond
    end type
    
    integer                  :: nparams = 0, lbuffer = 100
    type(param), allocatable :: params(:) ! Symbol table
    
    
    public :: defparam_c8, &
              defparam_c4, &
              defparam_r8, &
              defparam_r4, &
              defparam_i8, &
              defparam_i4, &
              getparam, &
              getvalue, &
              listvar
    
    contains
    
    subroutine defparam_c8(sym, val, ierr)   
        character(*), intent(in)        :: sym
        complex(c8), intent(in)         :: val
        integer, intent(out), optional  :: ierr
        !private
        character(len_trim(sym)) :: usym
        integer :: i
        if (present(ierr)) ierr = 0
        
        call setdefault()

        ! Assign val to sym if sym is already in symbol table
        usym = uppercase(sym)
        if (.not. is_letter(sym(1:1)) .or. len_trim(sym) > 24) then
            if (present(ierr)) ierr = 11
            write (*, *) 'Error: symbol ', trim(sym), ' has improper format'
            write (*, *)
            return
        end if
        do i = 1, nparams
            if (trim(usym) == trim(params(i)%symbol)) then
                params(i)%val = val
                return
            end if
        end do

        call addparam(usym, val)
    contains
        subroutine addparam(sym, val)
            character(*), intent(in) :: sym
            complex(c8), intent(in) :: val
            if (nparams < size(params)) then
                nparams = nparams + 1 ! Otherwise assign val to new symbol sym
                params(nparams)%symbol = sym
                params(nparams)%val = val
            else
                block
                    type(param), allocatable :: temp(:)
                    allocate(temp(nparams + lbuffer))
                    temp(:nparams) = params
                    call move_alloc(temp, params)
                    
                    nparams = nparams + 1
                    params(nparams)%symbol = sym
                    params(nparams)%val = val
                end block
            end if
        end subroutine
    end subroutine

    subroutine defparam_c4(sym, val)
        character(*), intent(in) :: sym
        complex(c4), intent(in) :: val
        complex(c8) :: vald

        vald = val
        call defparam_c8(sym, vald)
    end subroutine

    subroutine defparam_r8(sym, val)
        character(*), intent(in) :: sym
        real(r8), intent(in) :: val
        complex(c8) :: vald

        vald = cmplx(val, 0.0_r8, c8)
        call defparam_c8(sym, vald)
    end subroutine

    subroutine defparam_r4(sym, val)
        character(*), intent(in) :: sym
        real(r4), intent(in) :: val
        complex(c8) :: vald

        vald = cmplx(val, 0.0, c8)
        call defparam_c8(sym, vald)
    end subroutine

    subroutine defparam_i8(sym, ival)
        character(*), intent(in) :: sym
        integer(i8), intent(in) :: ival
        complex(c8) :: vald

        vald = cmplx(real(ival, r8), 0.0_r8, c8)
        call defparam_c8(sym, vald)
    end subroutine

    subroutine defparam_i4(sym, ival)
        character(*), intent(in) :: sym
        integer(i4), intent(in) :: ival
        complex(c8) :: vald

        vald = cmplx(real(ival, r8), 0.0, c8)
        call defparam_c8(sym, vald)
    end subroutine
    
    !> @brief   Find double precision complex value var
    !!          corresponding to symbol sym
    !! @ingroup group_parameters
    !! @param[inout] sym parameter symbol
    !! @param[out] val parameter value as single precision integer
    !! @param[out] (optional) ierr error code
    !!
    !! @b Remarks
    subroutine getparam(sym, var, ierr) 
        character(*), intent(inout)     :: sym
        complex(c8), intent(out)        :: var
        integer, intent(out), optional  :: ierr
        !private
        character(len_trim(sym)) :: usym
        integer :: ifind, j

        if (present(ierr)) ierr = 0
        sym = adjustl(sym)
        if (.not. is_letter(sym(1:1)) .or. len_trim(sym) > 24) then
            if (present(ierr)) ierr = 11
            write (*, *) 'Error: symbol ', trim(sym), ' has incorrect format'
            write (*, *)
            return
        end if
        ifind = 0
        usym = uppercase(sym)
        do j = 1, nparams
            if (trim(usym) == trim(params(j)%symbol)) then
                var = params(j)%val
                ifind = j
                exit
            end if
        end do
        if (ifind == 0) then
            if (present(ierr)) ierr = 4
            write (*, *) 'Error: symbol ', trim(sym), ' not in symbol table'
            write (*, *)
            return
        end if
    end subroutine
    
    !> @brief   Finds double precision complex value
    !!          corresponding to number string xinchar
    !!          or value in symbol table corresponding
    !!          to symbol name xinchar.
    !! @ingroup group_parameters
    !! @param[inout] xinchar input string
    !! @param[out] cval parameter value as double precision complex
    !! @param[out] (optional) ierr error code
    !!
    !! @b Remarks
    subroutine getvalue(xinchar, cval, ierr) 
        character(*), intent(inout)     :: xinchar
        complex(c8), intent(out)        :: cval
        integer, intent(out), optional  :: ierr
        !private
        real(r8) :: rval
        integer :: ios
        
        if (present(ierr)) ierr = 0

        if (is_letter(xinchar(1:1))) then ! xinchar is a symbol
            call getparam(xinchar, cval)
        else ! xinchar is a number string
            call value(xinchar, rval, ios) ! rval is the value of xinchar
            if (ios > 0) then
                if (present(ierr)) ierr = 3
                write (*, *) 'Error: number string ', trim(xinchar), ' does not correspond to a valid number'
                write (*, *)
            end if
            cval = cmplx(rval, 0.0_r8, c8)
            return
        end if
    end subroutine

    !> @brief   List all variables, their names and their values
    !! @ingroup group_parameters
    !!
    !! @b Remarks
    subroutine listvar()
        integer :: i

        write (*, '(/a)') ' VARIABLE LIST:'
        call setdefault()
        do i = 1, nparams
            write (*, *) trim(params(i)%symbol), ' = ', params(i)%val
        end do
    end subroutine
    
    !> @private
    subroutine setdefault()
        if (nparams == 0) then ! Initialize symbol table
            if (allocated(params)) deallocate(params)
            allocate(params(lbuffer))
            
            params(1)%symbol = 'PI'
            params(1)%val = (3.14159265358979_r8, 0.0_r8)
            params(2)%symbol = 'I'
            params(2)%val = (0.0_r8, 1.0_r8)
            nparams = 2
        end if
    end subroutine
    
end module