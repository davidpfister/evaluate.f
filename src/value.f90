!> @defgroup group_value evaluate_value
!> @brief   Generic interface for converting a string to a
!! number. The interface works for integer values (single and double) and
!! real values (single and double).
!! @par
!! <h2>Examples</h2>
!! The following example demonstrates some of the methods found in the 
!! @link evaluate_value evaluate_value @endlink module.
!!
!! @code
!! character(:), allocatable  :: numstring
!! integer                            :: val, ierr
!! 
!! numstring = '3'
!! call value(numstring, val, ierr)
!! if (ierr /= 0) print*, 'error: ', ierr
!! !val equals 3
!! @endcode
!! @{
module evaluate_value
    use evaluate_kinds

    implicit none; private

    public :: value, is_digit

    !> @interface value
    !> @brief Converts  string to a number
    !! @par
    !! <h2>Methods</h2>
    !!
    !! <h3>value(character(*) str, real(r8) rnum, integer ierr)</h3>
    !! 
    !! @param[in] str input string
    !! @param[out] rnum numeric output value as real(r8)
    !! @param[out] ierr (optional) integer error code
    !! 
    !! <h2> </h2>
    !! <h3>value(character(*) str, real(r4) rnum, integer ierr)</h3>
    !! 
    !! @param[in] str input string
    !! @param[out] rnum numeric output value as real(r4)
    !! @param[out] ierr (optional) integer error code
    !!
    !! <h2> </h2>
    !! <h3>value(character(*) str, integer(i8) inum, integer ierr)</h3>
    !! 
    !! @param[in] str input string
    !! @param[out] inum numeric output value as integer(i8)
    !! @param[out] ierr (optional) integer error code
    !!
    !! <h2> </h2> 
    !! <h3>value(character(*) str, integer(i4) inum, integer ierr)</h3>
    !! 
    !! @param[in] str input string
    !! @param[out] inum numeric output value as integer(i4)
    !! @param[out] ierr (optional) integer error code
    !!
    !! <h2> Examples </h2>
    !! The following demonstrate a call to the `value` interface.
    !! The value of `PI`, previously stored in the list of parameters
    !! is retrieved and assigned to the variable `p`.
    !! @code{.f90}
    !!  real(r8) :: p
    !!  integer :: ierr
    !!
    !!  call value('PI', p, ierr)
    !!  ! p = 3.14159265358979_r8
    !! @endcode
    !! <h2> Remarks </h2>
    interface value
    !! @cond
        module procedure value_r8
        module procedure value_r4
        module procedure value_i8
        module procedure value_i4
    !! @endcond
    end interface
    
    contains
    
    subroutine value_r8(str, rnum, ierr)
        character(*), intent(in)    :: str
        real(r8), intent(out)       :: rnum
        integer, intent(out)        :: ierr
        !private
        integer :: ilen, ipos

        ilen = len_trim(str)
        ipos = scan(str, 'Ee')
        if (.not. is_digit(str(ilen:ilen)) .and. ipos /= 0) then
            ierr = 3
            return
        end if
        read (str, *, iostat=ierr) rnum
    end subroutine

    subroutine value_r4(str, rnum, ierr)
        character(*), intent(in)    :: str
        real(r4), intent(out)       :: rnum
        integer, intent(out)        :: ierr
        !private
        real(r8)     :: rnumd

        call value_r8(str, rnumd, ierr)
        if (abs(rnumd) > huge(rnum)) then
            ierr = 15
            return
        end if
        if (abs(rnumd) < tiny(rnum)) rnum = 0.0_r4
        rnum = rnumd
    end subroutine

    subroutine value_i8(str, inum, ierr)
        character(*), intent(in)    :: str
        integer(i8), intent(out)    :: inum
        integer, intent(out)        :: ierr
        !private
        real(r8) :: rnum

        call value_r8(str, rnum, ierr)
        if (abs(rnum) > huge(inum)) then
            ierr = 15
            return
        end if
        inum = nint(rnum, i8)
    end subroutine

    subroutine value_i4(str, inum, ierr)
        character(*), intent(in)    :: str
        integer(i4), intent(out)    :: inum
        integer, intent(out)        :: ierr
        !private
        real(r8) :: rnum

        call value_r8(str, rnum, ierr)
        if (abs(rnum) > huge(inum)) then
            ierr = 15
            return
        end if
        inum = nint(rnum, i4)
    end subroutine

    !> @brief   Check if a character is a digit.
    !! @param[in] ch input character
    !! @returns 
    !! _logical_. `.true.` if the character is a digit (0,1,...,9), `.false.` otherwise.
    !! @n@n
    !! @b Remarks
    pure function is_digit(ch) result(res)
        character(1), intent(in) :: ch
        logical :: res

        select case (ch)
        case ('0':'9')
            res = .true.
        case default
            res = .false.
        end select
    end function
end module
!> @}